signature BUILDDEPEND = sig
    val build : { subexports: (DependencyGraph.farnode * DependencyGraph.env)
		                SymbolMap.map,
		  smlfiles: SmlInfo.info list,
		  localdefs: SmlInfo.info SymbolMap.map }
	-> { nodemap: DependencyGraph.node SymbolMap.map,
	     rootset: DependencyGraph.node list }
end

structure BuildDepend :> BUILDDEPEND = struct

    structure S = Symbol
    structure SS = SymbolSet
    structure SM = SymbolMap
    structure SK = Skeleton
    structure DG = DependencyGraph
    structure EM = GenericVC.ErrorMsg
    structure SP = GenericVC.SymPath

    fun look otherwise DG.EMPTY s = otherwise s
      | look otherwise (DG.BINDING (s', v)) s =
	if S.eq (s, s') then v else otherwise s
      | look otherwise (DG.LAYER (e, e')) s = look (look otherwise e') e s
      | look otherwise (DG.FCTENV { looker, domain }) s =
	(case looker s of NONE => otherwise s | SOME v => v)

    (* get the description for a symbol *)
    fun symDesc (s, r) =
	S.nameSpaceToString (S.nameSpace s) :: " " ::
	S.name s :: r

    fun build { subexports, smlfiles, localdefs } = let

	(* the "blackboard" where analysis results are announced *)
	(* (also used for cycle detection) *)
	val bb = ref AbsPathMap.empty
	fun lock i = bb := AbsPathMap.insert (!bb, SmlInfo.sourcepath i, NONE)
	fun release (i, r) =
	    (bb := AbsPathMap.insert (!bb, SmlInfo.sourcepath i, SOME r); r)
	fun fetch i = AbsPathMap.find (!bb, SmlInfo.sourcepath i)

	(* the "root set" *)
	val rs = ref AbsPathMap.empty
	fun addRoot i =
	    rs := AbsPathMap.insert (!rs, SmlInfo.sourcepath i, i)
	fun delRoot i =
	    (rs := #1 (AbsPathMap.remove (!rs, SmlInfo.sourcepath i)))
	    handle LibBase.NotFound => ()

	(* - get the result from the blackboard if it is there *)
	(* - otherwise trigger analysis *)
	(* - detect cycles using locking *)
	(* - maintain root set *)
	fun getResult (i, history) =
	    case fetch i of
		NONE => (lock i; addRoot i; release (i, analyze (i, history)))
	      | SOME (SOME r) => (delRoot i; r)
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
			recur (AbsPath.spec f, history);
			PrettyPrint.add_string pps "...";
			PrettyPrint.add_newline pps
		    end
		in
		    SmlInfo.error i "cyclic ML dependencies" pphist;
		    release (i, (DG.NODE { smlinfo = i,
					   localimports = [],
					   globalimports = [] },
				 DG.EMPTY))
		end

	(* do the actual analysis of an ML source and generate the
	 * corresponding node *)
	and analyze (i, history) = let
	    val li = ref []
	    val gi = ref []

	    (* register a local import *)
	    fun localImport (n as DG.NODE { smlinfo = i, ... }) = let
		fun sameNode (DG.NODE { smlinfo = i', ... }) =
		    SmlInfo.eq (i, i')
	    in
		if List.exists sameNode (!li) then ()
		else li := n :: !li
	    end

	    (* register a global import, maintain filter sets *)
	    fun globalImport (farn as DG.PNODE p) = let
		    fun sameFarNode (DG.FARNODE _) = false
		      | sameFarNode (DG.PNODE p') = Primitive.eq (p, p')
		in
		    if List.exists sameFarNode (!gi) then ()
		    else gi := farn :: !gi
		end
	      | globalImport (farn as DG.FARNODE (f, n)) = let
		    fun sameFarNode (DG.PNODE _) = false
		      | sameFarNode (DG.FARNODE (_, n')) = let
			    val DG.NODE { smlinfo = i, ... } = n
			    val DG.NODE { smlinfo = i', ... } = n'
			in
			    SmlInfo.eq (i, i')
			end
		in
		    case List.find sameFarNode (!gi) of
			NONE => gi := farn :: !gi (* brand new *)
		      | SOME (DG.FARNODE (NONE, n')) => ()
		        (* no filter before -> no change *)
		      | SOME (DG.FARNODE (SOME f', n')) => let
			(* there is a filter ...
			 *   calculate "union-filter", see if there is
			 *   a change, and if so, replace the filter *)
			    fun replace filt =
				gi :=
				   (DG.FARNODE (filt, n)) ::
				   (List.filter (not o sameFarNode) (!gi))
			in
			    case f of
				NONE => replace NONE
			      | SOME f =>
				    if SS.equal (f, f') then ()
				    else replace (SOME (SS.union (f, f')))
			end
			     
		      | SOME (DG.PNODE _) => ()	(* cannot happen *)
		end

	    val f = SmlInfo.sourcepath i
	    fun isSelf i' = SmlInfo.eq (i, i')

	    (* lookup function for things not defined in the same ML file.
	     * As a side effect, this function registers local and
	     * global imports. *)
	    fun lookimport s = let
		fun lookfar () =
		    case SM.find (subexports, s) of
			SOME (farn, e) => (globalImport farn; e)
		      | NONE => (SmlInfo.error i
				  (concat (AbsPath.spec f ::
					   ": reference to unknown " ::
					   symDesc (s, [])))
				  EM.nullErrorBody;
				 DG.EMPTY)
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
	    val lookup = look lookimport

	    fun lookSymPath e (SP.SPATH []) = DG.EMPTY
	      | lookSymPath e (SP.SPATH (p as (h :: t))) = let
		    fun dotPath [] = []
		      | dotPath [s] = [S.name s]
		      | dotPath (h :: t) = S.name h :: "." :: dotPath t
		    val firstTime = ref true
		    fun complain s =
			if !firstTime then
			    (SmlInfo.error i
			     (concat
			      ("undefined " ::
			       symDesc (s, " in path " :: dotPath p)))
			     EM.nullErrorBody;
			     firstTime := false;
			     DG.EMPTY)
			else DG.EMPTY
		    val lookup' = look complain
		    fun loop (e, []) = e
		      | loop (e, h :: t) = loop (lookup' e h, t)
		in
		    loop (lookup e h, t)
		end

	    (* "eval" -- compute the export environment of a skeleton *)
	    fun eval sk = let
		fun layer' f [] = DG.EMPTY
		  | layer' f [x] = f x
		  | layer' f (h :: t) =
		    foldl (fn (x, r) => DG.LAYER (f x, r)) (f h) t

		fun evalDecl e (SK.StrDecl l) = let
		        fun one { name, def, constraint = NONE } =
			    DG.BINDING (name, evalStrExp e def)
			  | one { name, def, constraint = SOME constr } =
			    (ignore (evalStrExp e def);
			     DG.BINDING (name, evalStrExp e constr))
		    in
			layer' one l
		    end
		  | evalDecl e (SK.FctDecl l) = let
			fun one { name, def } =
			    DG.BINDING (name, evalFctExp e def)
		    in
			layer' one l
		    end
		  | evalDecl e (SK.LocalDecl (d1, d2)) =
		    evalDecl (DG.LAYER (evalDecl e d1, e)) d2
		  | evalDecl e (SK.SeqDecl l) =
		    foldl (fn (d, e') =>
			   DG.LAYER (evalDecl (DG.LAYER (e', e)) d, e'))
		          DG.EMPTY l
		  | evalDecl e (SK.OpenDecl l) = layer' (evalStrExp e) l
		  | evalDecl e (SK.DeclRef s) =
		    (SS.app (ignore o lookup e) s; DG.EMPTY)

		and evalStrExp e (SK.VarStrExp sp) = lookSymPath e sp
		  | evalStrExp e (SK.BaseStrExp d) = evalDecl e d
		  | evalStrExp e (SK.AppStrExp (sp, l)) =
		    (app (ignore o evalStrExp e) l; lookSymPath e sp)
		  | evalStrExp e (SK.LetStrExp (d, se)) =
		    evalStrExp (DG.LAYER (evalDecl e d, e)) se
		  | evalStrExp e (SK.ConStrExp (se1, se2)) =
		    (ignore (evalStrExp e se1); evalStrExp e se2)

		and evalFctExp e (SK.VarFctExp (sp, feopt)) =
		    getOpt (Option.map (evalFctExp e) feopt,
			    lookSymPath e sp)
		  | evalFctExp e (SK.BaseFctExp x) = let
			val { params, body, constraint } = x
			val parame = evalDecl e params
			val bodye = DG.LAYER (parame, e)
		    in
			getOpt (Option.map (evalStrExp bodye) constraint,
				evalStrExp bodye body)
		    end
		  | evalFctExp e (SK.AppFctExp (sp, l, feopt)) =
		    (app (ignore o evalStrExp e) l;
		     getOpt (Option.map (evalFctExp e) feopt,
			     lookSymPath e sp))
		  | evalFctExp e (SK.LetFctExp (d, fe)) =
		    evalFctExp (DG.LAYER (evalDecl e d, e)) fe
	    in
		evalDecl DG.EMPTY sk
	    end

	    val e = eval (SmlInfo.skeleton i)
	    val n = DG.NODE { smlinfo = i,
			      localimports = !li,
			      globalimports = !gi }
	in
	    (n, e)
	end

	(* run the analysis on one ML file -- causing the blackboard
	 * and the root set to be updated accordingly *)
	fun doSmlFile i = ignore (getResult (i, []))

	(* converting smlinfos to nodes *)
	val i2n = #1 o valOf o valOf o fetch
    in
	(* run the analysis *)
	app doSmlFile smlfiles;
	(* generate map from export symbol to node and
	 * also return the root set *)
	{ nodemap = SM.map i2n localdefs,
	  rootset = map i2n (AbsPathMap.listItems (!rs)) }
    end
end
