structure BuildDepend = struct

    structure S = Symbol
    structure SS = SymbolSet
    structure SM = SymbolMap
    structure SK = Skeleton
    structure DG = DependencyGraph

    fun look otherwise DG.EMPTY s = otherwise s
      | look otherwise (DG.BINDING (s', v)) s =
	if S.eq (s, s') then v else otherwise s
      | look otherwise (DG.LAYER (e, e')) s = look (look otherwise e') e s
      | look otherwise (DG.FCTENV { looker, domain }) s =
	(case looker s of NONE => otherwise s | SOME v => v)
		 
    fun build { subexports, smlfiles, localdefs } = let

	(* the "blackboard" where analysis results are announced *)
	(* (also used for cycle detection) *)
	val bb = ref AbsPathMap.empty
	fun lock i = bb := AbsPathMap.insert (!bb, SmlInfo.sourcepath i, NONE)
	fun release (i, r) =
	    (bb := AbsPathMap.insert (!bb, SmlInfo.sourcepath i, SOME r); r)
	fun fetch i = AbsPathMap.find (!bb, SmlInfo.sourcepath i)

	(* the "root set" *)
	val rs = ref AbsPathSet.empty
	fun addRoot i = rs := AbsPathSet.add (!rs, SmlInfo.sourcepath i)
	fun delRoot i =
	    (rs := AbsPathSet.delete (!rs, SmlInfo.sourcepath i))
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
		    fun symDesc (s, r) =
			S.nameSpaceToString (S.nameSpace s) :: " " ::
			S.name s :: r
		    fun pphist pps = let
			fun recur [] = () (* shouldn't happen *)
			  | recur ((s, i') :: r) = let
				val f' = SmlInfo.sourcepath i'
				val _ =
				    if AbsPath.compare (f, f') = EQUAL then ()
				    else recur r
				val n' = AbsPath.name f'
				val l =
				    n' :: " refers to " ::
				    symDesc (s, [" defined in ..."])
			    in
				app (PrettyPrint.add_string pps) l;
				PrettyPrint.add_newline pps
			    end
		    in
			recur history;
			PrettyPrint.add_string pps (AbsPath.name f);
			PrettyPrint.add_newline pps
		    end
		in
		    SmlInfo.error i "cyclic ML dependencies" pphist
		end

	and analyze (i, history) = let
(*	    fun lookimport s =
		case SM.find (localdefs, s) of
		    SOME i' => let
			val (_, e) = getResult (i', (s, i) :: history)
		    in
			e
		    end
		  | NONE => 

	    val lookup = look lookimport *)
	in
	    Dummy.f ()
	end

	(* run the analysis on one ML file -- causing the blackboard
	 * and the root set to be updated accordingly *)
	fun doSmlFile i = ignore (getResult (i, []))
    in
	Dummy.f ()
    end
end
