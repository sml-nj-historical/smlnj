structure BuildDepend = struct

    structure S = GenericVC.Symbol
    structure SS = SymbolSet
    structure SK = Skeleton
    structure DG = DependencyGraph

    datatype env =
	IMPORTS
      | FCTENV of { looker: S.symbol -> value option,
		    domain: SS.set }
      | BINDING of S.symbol * value
      | LAYER of env * env
    withtype value = env

    fun build { subexports, smlfiles, localdefs } = let
	val results = ref AbsPathMap.empty
	fun lock i =
	    results :=
	    AbsPathMap.insert (!results, SmlInfo.sourcepath i, NONE)
	fun release (i, r) =
	    (results :=
	        AbsPathMap.insert (!results, SmlInfo.sourcepath i, SOME r);
	    r)
	fun fetch i = AbsPathMap.find (!results, SmlInfo.sourcepath i)

	fun getResult (i, history) =
	    case fetch i of
		NONE => (lock i; release (i, doSmlfile (i, history)))
	      | SOME NONE => let
		    val f = SmlInfo.sourcepath i
		    fun symDesc (s, r) =
			S.nameSpaceToString (S.nameSpace s) :: " " ::
			S.name s :: r
		    fun pphist pps = let
			fun recur [] = () (* shouldn't happen *)
			  | recur ((s, i') :: r) = let
				val f' = SmlInfo.sourcepath i'
				val () =
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
	      | SOME (SOME r) => r

	and doSmlfile (i, history) = Dummy.f ()
    in
	Dummy.f ()
    end
end
