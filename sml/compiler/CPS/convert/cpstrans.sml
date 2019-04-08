(* cpstrans.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor CPStrans(MachSpec : MACH_SPEC) : sig

    val cpstrans : CPS.function -> CPS.function

  end = struct

    open CPS
    structure LV = LambdaVar

    fun bug s = ErrorMsg.impossible ("CPStrans: " ^ s)
    fun ident x = x
    val mkv = LV.mkLvar

    val unboxedfloat = MachSpec.unboxedFloats

  (**************************************************************************
   *                    TOP OF THE MAIN FUNCTION                            *
   **************************************************************************)
    fun cpstrans fe = let
        (* variable substitution table *)
	  exception CPSSUBST
	  val M : value IntHashTable.hash_table = IntHashTable.mkTable(32,CPSSUBST)
	  val addvl = IntHashTable.insert M
	  fun mapvl v = ((IntHashTable.lookup M v) handle CPSSUBST => VAR v)
        (* variable to type hash*)
	  exception CTYMAP
	  val CT : cty IntHashTable.hash_table = IntHashTable.mkTable(32,CTYMAP)
	  val addty = IntHashTable.insert CT
	  val getty = IntHashTable.lookup CT
	  fun grabty (VAR v) = ((getty v) handle _ => CPSUtil.BOGt)
	    | grabty (NUM{ty, ...}) = NUMt ty
	    | grabty (REAL{ty, ...}) = FLTt ty
	    | grabty _ = CPSUtil.BOGt

	(**************************************************************************
	 *          UTILITY FUNCTIONS THAT DO THE ARGUMENT SPILLING               *
	 **************************************************************************)

	(** the following values must be consistent with the choices made
	 ** in the closure or spilling phases
	 *)
	  val fpnum = Int.min(MachSpec.numFloatRegs-2, MachSpec.numArgRegs)
	  val nregs = MachSpec.numRegs - MachSpec.numCalleeSaves
	  val gpnum = Int.min(nregs - 3, MachSpec.numArgRegs)

	  fun argSpill (args, ctys) = let
		fun h ([], [], ngp, nfp, ovs, ots, [], [], []) = NONE
		  | h ([], [], ngp, nfp, ovs, ots, [x], [_], []) = NONE
		  | h ([], [], ngp, nfp, ovs, ots, gvs, gts, fvs) =
		      SOME(rev ovs, rev ots, rev gvs, rev gts, rev fvs)
		  | h(x::xs, ct::cts, ngp, nfp, ovs, ots, gvs, gts, fvs) = (case ct
		       of FLTt 64 => if nfp > 0
			    then h (xs, cts, ngp, nfp-1, x::ovs, ct::ots, gvs, gts, fvs)
			    else h (xs, cts, ngp, nfp, ovs, ots, gvs, gts, x::fvs)
			| FLTt n => raise Fail(concat[ (* REAL32: FIXME *)
			      "argSpill: FLTt ", Int.toString n, " is unsupported"
			    ])
			| _ => if ngp > 0
			    then h (xs, cts, ngp-1, nfp, x::ovs, ct::ots, gvs, gts, fvs)
			    else h (xs, cts, ngp, nfp, ovs, ots, x::gvs, ct::gts, fvs))
		  | h _ = bug "unexpected case in argSpill"
		val n = length args
		in
		  if (n > fpnum) orelse (n > gpnum)
		    then h (args, ctys, gpnum, fpnum, [], [], [], [], [])
		    else NONE
		end (* function argSpill *)

	  fun spillIn (origargs, origctys, spgvars, spgctys, spfvars) = let
		val (fhdr, spgvars, spgctys) = (case spfvars
		       of [] => (ident, spgvars, spgctys)
			| _ => let
			    val v = mkv()
			    val vs = map (fn x => (x, OFFp 0)) spfvars
			    val ct = PTRt(FPT (length vs))
			    val fh = fn e => RECORD(RK_FBLOCK, vs, v, e)
			    in
			      (fh, (VAR v)::spgvars, ct::spgctys)
			    end
		      (* end case *))
		val (spgv, ghdr) = (case spgvars
		       of [] => (NONE, fhdr)
			| [x] => (SOME x, fhdr)
			| _ => let
			    val v = mkv()
			    val vs = map (fn x => (x, OFFp 0)) spgvars
			    in
			      (SOME (VAR v), fn e => fhdr(RECORD(RK_RECORD, vs, v, e)))
			    end
		      (* end case *))
		in
		  case spgv
		   of SOME x => SOME(origargs@[x], ghdr)
		    | NONE => NONE
		  (* end case *)
		end (* spillIn *)

	  fun spillOut (origargs, origctys, spgvars, spgctys, spfvars) = let
		val (spfv, fhdr, spgvars, spgctys) = (case spfvars
		       of [] => (NONE, ident, spgvars, spgctys)
			| _ => let
			    val v = mkv()
			    val u = VAR v
			    fun g (sv, (i,hdr)) = (* REAL32: FIXME *)
				  (i+1, fn e => hdr(SELECT(i, u, sv, FLTt 64, e)))
			    val (n,fh) = foldl g (0, ident) spfvars
			    val ct = PTRt(FPT n)
			    in
			      (SOME v, fh, v::spgvars, ct::spgctys)
			    end
		      (* end case *))
		val (spgv, ghdr) = (case (spgvars, spgctys)
		       of ([], _) => (NONE, fhdr)
			| ([x], t::_) => (SOME (x,t), fhdr)
			| _ => let
			    val v = mkv()
			    val u = VAR v
			    fun g (sv, st, (i,hdr)) =
				  (i+1, fn e =>hdr(SELECT(i, u, sv, st, e)))
			    val (n, gh) = ListPair.foldl g (0, fhdr) (spgvars,spgctys)
			    val ct = PTRt(RPT n)
			    in
			      (SOME (v, ct), gh)
			    end
		      (* end case *))
		in
		  case spgv
		   of SOME(x, t) => SOME (origargs@[x], origctys@[t], ghdr)
		    | NONE => NONE
		  (* end case *)
		end (* spillOut *)

	(* mkargin : value list -> (cexp -> cexp * value list) option *)
	  fun mkargin (args : value list) =
		Option.mapPartial spillIn (argSpill (args, List.map grabty args))

	(* mkargout : lvar list -> (lvar list * cty list * cexp -> cexp) option *)
	  fun mkargout args =
		Option.mapPartial spillOut (argSpill (args, List.map getty args))

	(**************************************************************************
	 *              MAIN FUNCTIONS THAT TRANSLATE CPS CODE                    *
	 **************************************************************************)
	  fun cexptrans ce = (case ce
		 of RECORD(k,vl,w,ce) => RECORD(k,map rectrans vl,w,cexptrans ce)
		  | SELECT(i,v,w,t,ce) => let
		      val _ = addty(w,t)
		      val v' = vtrans v
		      val ce' = cexptrans ce
		      in
			SELECT(i, v', w, getty w, ce')
		      end
		  | OFFSET(i,v,w,ce) => OFFSET(i, vtrans v, w, cexptrans ce)
		  | APP(v,vl) => (case mkargin vl
		       of SOME (nvl, hdr) => cexptrans(hdr(APP(v, nvl)))
			| NONE =>  APP(vtrans v, map vtrans vl)
		      (* end case *))
		  | FIX(l,ce) => FIX(map functrans l, cexptrans ce)
		  | SWITCH(v,c,l) => SWITCH(vtrans v,c,map cexptrans l)
		  | LOOKER(p,vl,w,t,ce) => let
		      val _ = addty(w,t)
		      val vl' = map vtrans vl
		      val ce' = cexptrans ce
		      in
			LOOKER(p, vl', w, getty w, ce')
		      end
		  | SETTER(p,vl,ce) => SETTER(p, map vtrans vl, cexptrans ce)
		  | ARITH(p,vl,w,t,ce) => (
		      addty(w,t);
		      ARITH(p, map vtrans vl, w, t, cexptrans ce))
		  | RCC(k,l,p,vl,wtl,ce) => (
		      List.app addty wtl;
		      RCC(k, l, p, map vtrans vl, wtl, cexptrans ce))
		  | PURE(P.box,[u],w,t,ce) => (addvl(w,vtrans u); cexptrans ce)
		  | PURE(P.unbox,[u],w,t,ce) => (
		      case u of VAR z => addty(z, t) | _ => ();
		      addvl(w,vtrans u); cexptrans ce)
		  | PURE(p as P.wrap(P.INT sz), [u], w, t, ce) =>
		      if (sz <= Target.defaultIntSz)
			then (  (* remove wrapping of tagged ints *)
			  addvl(w, vtrans u);
			  cexptrans ce)
			else (
			  addty(w,t);
			  PURE(p, [vtrans u], w, t, cexptrans ce))
		  | PURE(p as P.unwrap(P.INT sz), [u], w, t, ce) =>
		      if (sz <= Target.defaultIntSz)
			then (  (* remove unwrapping of tagged ints *)
			  addvl(w,vtrans u);
			  cexptrans ce)
			else (
			  addty(w,t);
			  PURE(p, [vtrans u], w, t, cexptrans ce))
		  | PURE(p as P.wrap(P.FLOAT _), [u], w, t, ce) =>
		      if unboxedfloat
			then (addty(w,t); PURE(p, [vtrans u], w, t, cexptrans ce))
			else (addvl(w,vtrans u); cexptrans ce)
		  | PURE(p as P.unwrap(P.FLOAT _), [u], w, t, ce) =>
		      if unboxedfloat
			then (addty(w,t); PURE(p, [vtrans u], w, t, cexptrans ce))
			else (addvl(w,vtrans u); cexptrans ce)
		  | PURE(P.getcon,[u],w,t,ce) => (
		      addty (w, t);
		      SELECT(0,vtrans u,w,t,cexptrans ce))
		  | PURE(P.getexn,[u],w,t,ce) => (
		      addty (w, t);
		      SELECT(0,vtrans u,w,t,cexptrans ce))
		  | PURE(p,vl,w,t,ce) => let
		      val _ = addty(w,t)
		      val vl' = map vtrans vl
		      val ce' = cexptrans ce
		      in
			PURE(p, vl', w, getty w, ce')
		      end
		  | BRANCH(p,vl,c,e1,e2) =>
		      BRANCH(p, map vtrans vl, c, cexptrans e1, cexptrans e2)
		(* end case *))

	  and functrans (fk, v, args, cl, ce) = let
		val _ = ListPair.app addty (args,cl)
		val ce' = cexptrans ce
		in
		  case mkargout args
		   of SOME (nargs, nctys, fhdr) => (fk, v, nargs, nctys, fhdr ce')
		    | NONE => (fk, v, args, cl, ce')
		  (* end case *)
		end

	  and rectrans(v, acp) = (vtrans v, acp)

	  and vtrans (VAR v) = (mapvl v)
            | vtrans u = u

          in
	    functrans fe
	  end (* cpstrans *)

  end (* structure CPStrans *)

