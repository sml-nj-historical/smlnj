(*
 * Build the dependency graph for one group/library.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BUILDDEPEND = sig
    type impexp = DependencyGraph.impexp

    val build :
	{ imports: impexp SymbolMap.map,
	  gimports: impexp SymbolMap.map,
	  smlfiles: SmlInfo.info list,
	  localdefs: SmlInfo.info SymbolMap.map }
	* SymbolSet.set option		(* filter *)
	* (string -> unit)		(* error *)
	->
	impexp SymbolMap.map		(* exports *)
end

structure BuildDepend :> BUILDDEPEND = struct

    structure S = Symbol
    structure SS = SymbolSet
    structure SM = SymbolMap
    structure SK = Skeleton
    structure DG = DependencyGraph
    structure DE = DAEnv
    structure EM = GenericVC.ErrorMsg
    structure SP = GenericVC.SymPath

    type impexp = DG.impexp

    fun look otherwise DE.EMPTY s = otherwise s
      | look otherwise (DE.BINDING (s', v)) s =
	if S.eq (s, s') then v else otherwise s
      | look otherwise (DE.LAYER (e, e')) s = look (look otherwise e') e s
      | look otherwise (DE.FCTENV { looker, domain }) s =
	(case looker s of NONE => otherwise s | SOME v => v)
      | look otherwise (DE.FILTER (ss, e)) s =
	if SymbolSet.member (ss, s) then look otherwise e s else otherwise s

    (* get the description for a symbol *)
    fun symDesc (s, r) =
	S.nameSpaceToString (S.nameSpace s) :: " " ::
	S.name s :: r

    fun build (coll, fopt, error) = let
	val { imports, gimports, smlfiles, localdefs } = coll

	(* the "blackboard" where analysis results are announced *)
	(* (also used for cycle detection) *)
	val bb = ref AbsPathMap.empty
	fun lock i = bb := AbsPathMap.insert (!bb, SmlInfo.sourcepath i, NONE)
	fun release (i, r) =
	    (bb := AbsPathMap.insert (!bb, SmlInfo.sourcepath i, SOME r); r)
	fun fetch i = AbsPathMap.find (!bb, SmlInfo.sourcepath i)

	(* - get the result from the blackboard if it is there *)
	(* - otherwise trigger analysis *)
	(* - detect cycles using locking *)
	(* - maintain root set *)
	fun getResult (i, history) =
	    case fetch i of
		NONE => (lock i; release (i, analyze (i, history)))
	      | SOME (SOME r) => r
	      | SOME NONE => let	(* cycle found --> error message *)
		    val f = SmlInfo.sourcepath i
		    fun pphist pps = let
			fun recur (_, []) = () (* shouldn't happen *)
			  | recur (n'', (s, i') :: r) = let
				val f' = SmlInfo.sourcepath i'
				val n' = AbsPath.spec f'
				val _ =
				    if SmlInfo.eq (i, i') then ()
				    else recur (n', r)
				val l =
				    n' :: " refers to " ::
				    symDesc (s, [" defined in ", n''])
			    in
				app (PrettyPrint.add_string pps) l;
				PrettyPrint.add_newline pps
			    end
		    in
			PrettyPrint.add_newline pps;
			recur (AbsPath.spec f, history)
		    end
		in
		    SmlInfo.error i "cyclic ML dependencies" pphist;
		    release (i, (DG.SNODE { smlinfo = i,
					    localimports = [],
					    globalimports = [] },
				 DE.EMPTY))
		end

	(* do the actual analysis of an ML source and generate the
	 * corresponding node *)
	and analyze (i, history) = let
	    val li = ref []
	    val gi = ref []

	    (* register a local import *)
	    fun localImport n =
		if List.exists (fn n' => DG.seq (n, n')) (!li) then ()
		else li := n :: !li

	    (* register a global import, maintain filter sets *)
	    fun globalImport (f, n) = let
		fun sameN (_, n') = DG.sbeq (n, n')
	    in
		case List.find sameN (!gi) of
		    NONE => gi := (f, n) :: !gi (* brand new *)
		  | SOME (NONE, n') => () (* no filter -> no change *)
		  | SOME (SOME f', n') => let
			(* there is a filter...
			 *  calculate "union", see if there is a change,
			 *  and if so, replace the filter *)
			fun replace filt =
			    gi := (filt, n) :: List.filter (not o sameN) (!gi)
		    in
			case f of
			    NONE => replace NONE
			  | SOME f =>
				if SS.equal (f, f') then ()
				else replace (SOME (SS.union (f, f')))
		    end
	    end

	    val f = SmlInfo.sourcepath i
	    fun isSelf i' = SmlInfo.eq (i, i')

	    exception Lookup

	    (* lookup function for things not defined in the same ML file.
	     * As a side effect, this function registers local and
	     * global imports. *)
	    fun lookimport s = let
		fun lookfar () =
		    case SM.find (imports, s) of
			SOME (farn, e) => (globalImport farn; e)
		      | NONE => (SmlInfo.error i
				  (concat (AbsPath.spec f ::
					   ": reference to unknown " ::
					   symDesc (s, [])))
				  EM.nullErrorBody;
				 raise Lookup)
	    in
		case SM.find (localdefs, s) of
		    SOME i' =>
			if isSelf i' then lookfar ()
			else let
			    val (n, e) = getResult (i', (s, i) :: history)
			in
			    localImport n;
			    e
			end
		  | NONE => lookfar ()
	    end

	    (* build the lookup function for DG.env *)
	    val lookup_exn = look lookimport

	    fun lookSymPath e (SP.SPATH []) = DE.EMPTY
	      | lookSymPath e (SP.SPATH (p as (h :: t))) = let
		    fun dotPath [] = []
		      | dotPath [s] = [S.name s]
		      | dotPath (h :: t) = S.name h :: "." :: dotPath t
		    fun complain s =
			(SmlInfo.error i
			  (concat
			   (AbsPath.spec f ::
			    ": undefined " ::
			    symDesc (s, " in path " :: dotPath p)))
			  EM.nullErrorBody;
			 raise Lookup)
		    val lookup_exn' = look complain
		    fun loop (e, []) = e
		      | loop (e, h :: t) = loop (lookup_exn' e h, t)
		in
		    loop (lookup_exn e h, t) handle Lookup => DE.EMPTY
		end

	    fun lookup e s = lookup_exn e s handle Lookup => DE.EMPTY

	    (* "eval" -- compute the export environment of a skeleton *)
	    fun eval sk = let
		fun layer' f [] = DE.EMPTY
		  | layer' f [x] = f x
		  | layer' f (h :: t) =
		    foldl (fn (x, r) => DE.LAYER (f x, r)) (f h) t

		fun evalDecl e (SK.Bind (name, def)) =
		    DE.BINDING (name, evalModExp e def)
		  | evalDecl e (SK.Local (d1, d2)) =
		    evalDecl (DE.LAYER (evalDecl e d1, e)) d2
		  | evalDecl e (SK.Seq l) =
		    foldl (fn (d, e') =>
			   DE.LAYER (evalDecl (DE.LAYER (e', e)) d, e'))
		          DE.EMPTY l
		  | evalDecl e (SK.Par l) = layer' (evalDecl e) l
		  | evalDecl e (SK.Open s) = evalModExp e s
		  | evalDecl e (SK.Ref s) =
		    (SS.app (ignore o lookup e) s; DE.EMPTY)

		and evalModExp e (SK.Var sp) = lookSymPath e sp
		  | evalModExp e (SK.Decl d) = evalDecl e d
		  | evalModExp e (SK.App (sp, l)) =
		    (app (ignore o evalModExp e) l; lookSymPath e sp)
		  | evalModExp e (SK.Let (d, m)) =
		    evalModExp (DE.LAYER (evalDecl e d, e)) m
		  | evalModExp e (SK.Con (m1, m2)) =
		    (ignore (evalModExp e m1); evalModExp e m2)
	    in
		evalDecl DE.EMPTY sk
	    end

	    val e = eval (SmlInfo.skeleton i)
	    val n = DG.SNODE { smlinfo = i,
			       localimports = !li,
			       globalimports = !gi }
	in
	    (n, e)
	end

	(* run the analysis on one ML file -- causing the blackboard
	 * to be updated accordingly *)
	fun doSmlFile i = ignore (getResult (i, []))

	(* converting smlinfos to sbnodes * env *)
	fun i2sbn i = let
	    val (sn, e) = valOf (valOf (fetch i))
	in
	    (DG.SB_SNODE sn, e)
	end

	(* run the analysis *)
	val _ = app doSmlFile smlfiles

	fun addDummyFilt (sbn, e) = ((NONE, sbn), e)

	(* First we make a map of all locally defined symbols to
	 * the local "far sb node"
	 * but with only a dummy filter attached.
	 * This makes it consistent with the current state
	 * of "imports" and "gimports" where there can be filters, but
	 * where those filters are not yet strengthened according to fopt *)
	val localmap = SM.map (addDummyFilt o i2sbn) localdefs

	val exports =
	    case fopt of
		NONE =>
		    (* There is no filter -- so we are in an ordinary
		     * group and should export all gimports as well as
		     * all local definitions.
		     * No filter strengthening is necessary. *)
		    SM.unionWith #1 (localmap, gimports)
	      | SOME ss => let
		    (* There is a filter.
		     * We export only the things in the filter.
		     * They can be taken from either localmap or else from
		     * imports.  In either case, it is necessary to strengthen
		     * the filter attached to each node. *)
		    fun strengthen ((fopt', sbn), e) = let
			val new_fopt =
			    case fopt' of
				NONE => fopt
			      | SOME ss' => SOME (SS.intersection (ss, ss'))
		    in
			((new_fopt, sbn), DE.FILTER (ss, e))
		    end
		    val availablemap = SM.unionWith #1 (localmap, imports)
		    fun addNodeFor (s, m) =
			case SM.find (availablemap, s) of
			    SOME n => SM.insert (m, s, strengthen n)
			  | NONE => (error 
				      (concat ("exported " ::
					       symDesc (s, [" not defined"])));
				     m)
		in
		    SS.foldl addNodeFor SM.empty ss
		end
    in
	exports
    end
end
