(* cps-split.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature CPSSPLIT =
  sig
    val cpsSplit: CPS.function -> CPS.function list 
  end;

(** A dummy implementation for now **)
functor CpsSplitFun (MachSpec: MACH_SPEC): CPSSPLIT =
  struct

    fun cpsSplit f = [f]

  end


(*
functor CpsSplitFun (MachSpec: MACH_SPEC): CPSSPLIT = struct

    exception Impossible

    (* currently we don't deal with floating point stuff,
     * it is probably not worth the trouble here anyway *)
    val numRegs = MachSpec.numRegs
    val numCalleeSaves = MachSpec.numCalleeSaves

    val maxEscapeArgs = numRegs - 1 - numCalleeSaves - 2
    val maxContArgs = numRegs - 1 - 2

    structure C = CPS
    structure SL = SortedList
    structure A = LambdaVar
    structure M = IntmapF

    val add = SL.enter
    val del = SL.rmv
    val join = SL.merge
    val xcl = SL.remove
    val mkset = SL.uniq
    val inset = SL.member
    val intersect = SL.intersect

    fun lv_x (C.VAR v, l) = add (v, l)
      | lv_x (C.LABEL v, l) = add (v, l)
      | lv_x (_, l) = l

    infix $
    fun (f $ g) (x, y) = f (g x, y)
    fun fst (x, _) = x

    fun lv_record (l, v, elv) = foldl (lv_x $ fst) (del (v, elv)) l

    fun lv_xv (x, v, elv) = lv_x (x, del (v, elv))

    fun lv_app (x, l) = foldl lv_x (lv_x (x, [])) l

    fun lv_setter (l, elv) = foldl lv_x elv l

    fun lv_calc (l, v, elv) = foldl lv_x (del (v, elv)) l

    fun lv_branch (l, v, elv1, elv2) =
	foldl lv_x (del (v, join (elv1, elv2))) l

    fun lv'switch (x, v, el) =
	lv_x (x, del (v, foldl (join $ live) [] el))

    and lv'branch (l, v, e1, e2) = lv_branch (l, v, live e1, live e2)

    and lv'_fix (l, elv) = let
	fun f ((_, v, vl, _, e), (lv, bv)) =
	    (join (xcl (mkset vl, live e), lv), add (v, bv))
	val (lv, bv) = foldl f (elv, []) l
    in
	xcl (bv, lv)
    end

    and live (C.RECORD (_, l, v, e)) = lv_record (l, v, live e)
      | live (C.SELECT (_, x, v, _, e)) = lv_xv (x, v, live e)
      | live (C.OFFSET (_, x, v, e)) = lv_xv (x, v, live e)
      | live (C.APP (x, l)) = lv_app (x, l)
      | live (C.FIX (l, e)) = lv'_fix (l, live e)
      | live (C.SWITCH (x, v, el)) = lv'switch (x, v, el)
      | live (C.BRANCH (_, l, v, e1, e2)) = lv'branch (l, v, e1, e2)
      | live (C.SETTER (_, l, e)) = lv_setter (l, live e)
      | live (C.LOOKER (_, l, v, _, e)) = lv_calc (l, v, live e)
      | live (C.ARITH (_, l, v, _, e)) = lv_calc (l, v, live e)
      | live (C.PURE (_, l, v, _, e)) = lv_calc (l, v, live e)

    structure M = IntmapF

    (* scc stuff *)

    datatype node = N of { id: int,
			   function: C.function option,
			   edges: node list ref,
			   fv: A.lvar list }

    fun lt (N n1, N n2) = (#id n1) < (#id n2)
    fun eq (N n1, N n2) = (#id n1) = (#id n2)

    structure SCC = SCCUtilFun (type node = node val lt = lt val eq = eq)

    fun scc (l, fv, e) = let
	val root = N { id = ~1, function = NONE, edges = ref [], fv = fv }
	fun mkn (f as (_, v, vl, _, b)) =
	    N { id = v, function = SOME f, edges = ref [],
	        fv = xcl (mkset vl, live b) }
	val nodes = root :: map mkn l
	fun addif n n' = let
	    val N { edges, fv, ... } = n'
	    val N { edges = bedges, ... } = n
	in
	    case n of
		N { function = SOME (k, f, _, _, _), ... } =>
		    if inset fv f then
			(edges := n :: (!edges);
			 (* Add back edge for known functions. This forces
			  * the two nodes to be in the same scc, which is
			  * necessary because calls to known functions
			  * cannot go accross code segments *)
			 case k of
			     C.ESCAPE => ()
			   | C.CONT => ()
			   | _ => bedges := n' :: (!bedges))
		    else ()
	      | _ => ()
	end
	(* enter all edges *)
	val _ = app (fn n => (app (addif n) nodes)) nodes
	(* outgoing edges *)
	fun out (N { edges = ref e, ... }) = e
	(* calculate sccs of this graph;
	 * the top scc must contain the original root node (f = NONE)! *)
	val top :: sccs =
	    SCC.sccTop { root = root, outgoingEdgesOf = out }

	fun component l = let
	    fun xtr (N { function = SOME f, fv, ... }, (fl, lv, bv)) =
		(f :: fl, join (fv, lv), add (#2 f, bv))
	      | xtr (N { function = NONE, ... }, x) = x
	in
	    foldl xtr ([], [], []) l
	end

	val top' =
	    case top of
		[N { function = NONE, ... }] => NONE
	      | _ => SOME (component top)
    in
	{ components = map component sccs, top = top' }
    end

    (* don't keep type info about known functions, because they cannot
     * be passed to other codeunits anyway *)
    datatype tyinfo =
	NORMALTY of C.cty		(* ordinary C.cty *)
      | KNOWNTY				(* known function *)
      | CONTTY of C.cty list		(* argument types of cont. function *)

    type tymap = tyinfo M.intmap

    fun rectyn 0 = C.INTt
      | rectyn n = C.PTRt (C.RPT n)

    fun recty lv = rectyn (length lv)

    fun madd (v, t, m) = M.add (m, v, NORMALTY t)

    fun maddf ((C.ESCAPE, v, _, _, _), m) = M.add (m, v, NORMALTY C.FUNt)
      | maddf ((C.CONT, v, _, tl, _), m) = M.add (m, v, CONTTY tl)
      | maddf ((_, v, _, _, _), m) = M.add (m, v, KNOWNTY)

    fun maddal ([], [], m) = m
      | maddal (v :: vl, t :: tl, m) = maddal (vl, tl, madd (v, t, m))
      | maddal _ = raise Impossible

    fun reconst (exp, tymap, units) =
	case exp of
	    C.RECORD (k, l, v, e) => let
		val tymap' = madd (v, recty l, tymap)
		val (e', units', lv) = reconst (e, tymap', units)
		val lv' = lv_record (l, v, lv)
	    in
		(C.RECORD (k, l, v, e'), units', lv')
	    end
	  | C.SELECT (i, x, v, t, e) => let
		val tymap' = madd (v, t, tymap)
		val (e', units', lv) = reconst (e, tymap', units)
		val lv' = lv_xv (x, v, lv)
	    in
		(C.SELECT (i, x, v, t, e'), units', lv')
	    end
	  | C.OFFSET (i, x, v, e) => let
		val tymap' = madd (v, C.BOGt, tymap)
		val (e', units', lv) = reconst (e, tymap', units)
		val lv' = lv_xv (x, v, lv)
	    in
		(C.OFFSET (i, x, v, e'), units', lv')
	    end
	  | C.APP (x, l) => (exp, units, lv_app (x, l))
	  | C.FIX (fl, e) => reconst_fix (fl, e, tymap, units)
	  | C.SWITCH (x, v, el) => let
		fun r (e, (u, lv, el)) = let
		    val (e', u', lv') = reconst (e, tymap, u)
		in
		    (u', join (lv, lv'), e' :: el)
		end
		val (units', lv, el') = foldr r (units, [], []) el
	    in
		(C.SWITCH (x, v, el'), units', lv)
	    end
	  | C.BRANCH (b, l, v, e1, e2) => let
		val tymap' = madd (v, C.INTt, tymap)
		val (e1', units', lv1) = reconst (e1, tymap', units)
		val (e2', units'', lv2) = reconst (e2, tymap', units')
		val lv = lv_branch (l, v, lv1, lv2)
	    in
		(C.BRANCH (b, l, v, e1', e2'), units'', lv)
	    end
	  | C.SETTER (s, l, e) => let
		val (e', units', lv) = reconst (e, tymap, units)
		val lv' = lv_setter (l, lv)
	    in
		(C.SETTER (s, l, e), units', lv')
	    end
	  | C.LOOKER (p, l, v, t, e) => let
		val tymap' = madd (v, t, tymap)
		val (e', units', lv) = reconst (e, tymap', units)
		val lv' = lv_calc (l, v, lv)
	    in
		(C.LOOKER (p, l, v, t, e'), units', lv')
	    end
	  | C.ARITH (p, l, v, t, e) => let
		val tymap' = madd (v, t, tymap)
		val (e', units', lv) = reconst (e, tymap', units)
		val lv' = lv_calc (l, v, lv)
	    in
		(C.ARITH (p, l, v, t, e'), units', lv')
	    end
	  | C.PURE (p, l, v, t, e) => let
		val tymap' = madd (v, t, tymap)
		val (e', units', lv) = reconst (e, tymap', units)
		val lv' = lv_calc (l, v, lv)
	    in
		(C.PURE (p, l, v, t, e'), units', lv')
	    end

    and reconst_fix (fl, e, tymap, units) = let
	val tymap = foldl maddf tymap fl
	val (e, units, lv) = reconst (e, tymap, units)
	val { components, top } = scc (fl, lv, e)

	(* recursively apply reconstruction to continuations *)
	fun reconst_cont ((C.CONT, v, vl, tl, e), (u, fl)) = let
	        val tymap = maddal (vl, tl, tymap)
		val (e, u, _) = reconst (e, tymap, u)
	    in
		(u, (C.CONT, v, vl, tl, e) :: fl)
	    end
	  | reconst_cont (f, (u, fl)) = (u, f :: fl)
	fun reconst_comp (c, u) = foldl reconst_cont (u, []) c

	(* incorporate top component *)
	val (e, lv, units) =
	    case top of
		NONE => (e, lv, units)
	      | SOME (bfl, blv, bbv) => let
		    val (u, c) = reconst_comp (bfl, units)
		in
		    (C.FIX (c, e), xcl (bbv, join (blv, lv)), u)
		end

	(* a component is eligible to be put into its own unit if
	 *  - it doesn't contain C.CONT members
	 *  - none of its free variables refers to a known function *)
	fun stays (fl, fv) = let
	    fun isCont (C.CONT, _, _, _, _) = true | isCont _ = false
	    fun impossibleArg v =
		case M.lookup tymap v of
		    KNOWNTY => true
		  | NORMALTY C.CNTt => true
		  | _ => false
	in
	    List.exists isCont fl orelse List.exists impossibleArg fv
	end

	(* move a component into its own code unit *)
	fun movecomponent (fl, lv, xl, yl, e, units) = let

	    (* code for the new unit:
	     * (C.ESCAPE, unitvar,
	     *  [contvar, argvar], [C.CNTt, C.BOGt],
	     *  FIX ((ESCAPE, funvar,
	     *        [contvar2, exl...], [C.CNTt, extl...],
	     *        DECODESEND (exl..., xl...,
	     *                    FIX (fl,
	     *                         ENCODERCV (yl, eyl,
	     *                                    APP (contvar2, eyl)))))
	     *       RECORD ([argvar, funvar], resvar,
	     *               APP (contvar, [resvar]))))
	     *
	     * code that replaces the original occurence of the component:
	     *  FIX ((CONT, contvar2, eyl, [FUNt...],
	     *        DECODERCV (eyl, yl, e)),
	     *       ENCODESEND (xl, exl,
	     *                   APP (funvar, [contvar2, exl...])))
	     *)

	    val unitvar = A.mkLvar ()
	    val contvar = A.mkLvar ()
	    val argvar = A.mkLvar ()
	    val funvar = A.mkLvar ()
	    val contvar2 = A.mkLvar ()
	    val resvar = A.mkLvar ()

	    fun firstN (0, l) = ([], l)
	      | firstN (n, h :: t) = let
		    val (f, r) = firstN (n - 1, t)
		in
		    (h :: f, r)
		end
	      | firstN _ = raise Impossible

	    fun selectall (base, vl, tl, e) = let
		val base = C.VAR base
		fun s ([], [], _, e) = e
		  | s (h :: t, th :: tt, i, e) =
		    s (t, tt, i + 1, C.SELECT (i, base, h, th, e))
	    in
		s (vl, tl, 0, e)
	    end

	    fun funty _ = C.FUNt
	    fun recvar v = (C.VAR v, C.OFFp 0)

	    (* deal with received values (all of them are functions) *)
	    val ny = length yl
	    val (ysend, mk_yrcv) =
		if ny <= maxContArgs then
		    (C.APP (C.VAR contvar2, map C.VAR yl),
		     fn body =>
		     C.FIX ([(C.CONT, contvar2, yl, map funty yl, e)], body))
		else let
		    val npy = ny + 1 - maxContArgs
		    val (pyl, ryl) = firstN (npy, yl)
		    val v = A.mkLvar ()
		in
		    (C.RECORD (A.RK_RECORD, map recvar pyl, v,
			       C.APP (C.VAR contvar2,
				      (C.VAR v) :: map C.VAR ryl)),
		     fn body =>
		     C.FIX ([(C.CONT, contvar2, v :: ryl,
			      (recty pyl) :: map funty ryl,
			      selectall (v, pyl, map funty pyl, e))],
			    body))
		end

	    (* put the component in *)
	    val fix'n'ysend = C.FIX (fl, ysend)

	    (* Wrap a CNTt so it can be passed as a FUNt.
	     * tl lists argument types *)
	    fun wrapcnt (xvar, x'var, tl, e) = let
		val vl = map (fn _ => A.mkLvar ()) tl
		val ikvar = A.mkLvar ()
	    in
		C.FIX ([(C.ESCAPE, x'var, ikvar :: vl, C.CNTt :: tl,
			 C.APP (C.VAR xvar, map C.VAR vl))],
		       e)
	    end

	    (* unwrap FUNt so it can be used as a CNTt.
	     * Even though it ignores it our escaping version of the
	     * continuation expects a continuation of its own.  We have
	     * to pull one out of the air... contvar2 *)
	    fun unwrapcnt (x'var, xvar, tl, e) = let
		val vl = map (fn _ => A.mkLvar ()) tl
	    in
		C.FIX ([(C.CONT, xvar, vl, tl,
			 C.APP (C.VAR x'var, map C.VAR (contvar2 :: vl)))],
		       e)
	    end

	    fun wrap'gen other (v, (evl, etl, mkwE, mkuwE)) =
		case M.lookup tymap v of
		    KNOWNTY => raise Impossible
		  | CONTTY tl => let
			val ev = A.mkLvar ()
		    in
			(ev :: evl,
			 C.FUNt :: etl,
			 fn e => wrapcnt (v, ev, tl, mkwE e),
			 fn e => unwrapcnt (ev, v, tl, mkuwE e))
		    end
		  | NORMALTY C.CNTt => raise Impossible
		  | NORMALTY ct => other (v, ct, evl, etl, mkwE, mkuwE)

	    (* wrap a variable, so I can stick it into a record *)
	    val wrap'rec = let
		fun other (v, ct, evl, etl, mkwE, mkuwE) = let
		    fun w (wrap, unwrap) = let
			val ev = A.mkLvar ()
		    in
			(ev :: evl,
			 C.BOGt :: etl,
			 fn e => C.PURE (wrap, [C.VAR v], ev, C.BOGt, mkwE e),
			 fn e => C.PURE (unwrap, [C.VAR ev], v, ct, mkuwE e))
		    end
		in
		    case ct of
			C.INT32t => w (C.P.i32wrap, C.P.i32unwrap)
		      | C.FLTt => w (C.P.fwrap, C.P.funwrap)
		      | _ => (v :: evl, ct :: etl, mkwE, mkuwE)
		end
	    in
		wrap'gen other
	    end

	    (* wrap continuations only (for argument passing) *)
	    val wrap'cnt = let
		fun other (v, ct, evl, etl, mkwE, mkuwE) =
		    (v :: evl, ct :: etl, mkwE, mkuwE)
	    in
		wrap'gen other
	    end

	    val nx = length xl
	    val unitresult =
		C.RECORD (A.RK_RECORD,
			  [recvar argvar, recvar funvar],
			  resvar,
			  C.APP (C.VAR contvar, [C.VAR resvar]))
	    val (xsend, xrcv) =
		if nx = 0 then
		    (C.APP (C.VAR funvar, [C.VAR contvar2, C.INT 0]),
		     C.FIX ([(C.ESCAPE, funvar,
			      [contvar2, A.mkLvar ()],
			      [C.CNTt, C.INTt],
			      fix'n'ysend)],
			    unitresult))
		else if nx <= maxEscapeArgs then let
		    val (exl, etl, wrapper, unwrapper) =
			foldr wrap'cnt ([], [], fn e => e, fn e => e) xl
		in
		    (wrapper
		     (C.APP (C.VAR funvar,
			     (C.VAR contvar2) :: map C.VAR exl)),
		     C.FIX ([(C.ESCAPE, funvar,
			      contvar2 :: exl, C.CNTt :: etl,
			      unwrapper fix'n'ysend)],
			    unitresult))
		end
		else let
		    (* we need two rregisters for:
		     * 1. the continuation, 2. the record holding extra args *)
		    val npx = nx + 1 - maxEscapeArgs
		    val (pxl, rxl) = firstN (npx, xl)
		    val v = A.mkLvar ()
		    val (epxl, eptl, pwrapper, punwrapper) =
			foldr wrap'rec ([], [], fn e => e, fn e => e) pxl
		    val (erxl, ertl, rwrapper, runwrapper) =
			foldr wrap'cnt ([], [], fn e => e, fn e => e) rxl
		in
		    (pwrapper
		     (rwrapper
		      (C.RECORD (A.RK_RECORD, map recvar epxl, v,
				 C.APP (C.VAR funvar,
					(C.VAR contvar2) :: (C.VAR v) ::
					map C.VAR erxl)))),
		     C.FIX ([(C.ESCAPE, funvar,
			      contvar2 :: v :: erxl,
			      C.CNTt :: (recty epxl) :: ertl,
			      selectall (v, epxl, eptl,
					 runwrapper
					  (punwrapper fix'n'ysend)))],
			    unitresult))
		end

	    val newunit =
		(C.ESCAPE, unitvar, [contvar, argvar], [C.CNTt, C.BOGt],
		 xrcv)
	    val replacedcode = mk_yrcv xsend

	    val { uheader, curargvar, ul } = units
	    val newargvar = A.mkLvar ()
	    fun uheader' e =
		C.SELECT (0, C.VAR newargvar, curargvar, C.BOGt,
			  C.SELECT (1, C.VAR newargvar, funvar, C.FUNt,
				    uheader e))
	    val units' = { uheader = uheader', curargvar = newargvar,
			   ul = newunit :: ul }
	in
	    (units', replacedcode)
	end

	(* deal with one component at a time *)
	fun docomponent ((fl, lv, bv), (e, units, lv_rest)) = let
	    val fv = xcl (bv, lv)
	    val lv' = join (fv, xcl (bv, lv_rest))
	    val xl = fv
	    val yl = intersect (bv, lv_rest)
	in
	    case yl of
		[] => (e, units, lv_rest)
	      | _ =>
		    if stays (fl, fv) then let
			val (units, fl) = reconst_comp (fl, units)
		    in
			(C.FIX (fl, e), units, lv')
		    end
		    else let
			val (u, e) = movecomponent (fl, lv, xl, yl, e, units)
		    in
			(e, u, lv')
		    end
	end

    in
        (* now do them all *)
	foldl docomponent (e, units, lv) components
    end

    fun split (C.ESCAPE, name,
	       [contvar, argvar],  [C.CNTt, argty], body) = let
	val units = { uheader = fn e => e,
		      curargvar = argvar,
		      ul = [] }
	val tymap = M.add (madd (argvar, C.BOGt, M.empty),
			   contvar, CONTTY [C.BOGt])
	val (e, u, _) = reconst (body, tymap, units)
	val { uheader, curargvar, ul } = u
	val lastunit = (C.ESCAPE, name, [contvar, curargvar], [C.CNTt, C.BOGt],
			uheader e)
    in
	foldl (op ::) [lastunit] ul
    end

    fun cpsSplit f =
	case split f of
	    [_, _] => [f]  (* found only one extra piece... don't bother *)
	  | l => l

end
*)


(*
 * $Log: cps-split.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:46  george
 * Version 110.5
 *
 *)
