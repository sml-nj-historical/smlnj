functor CompileGenericFn (structure CT: COMPILATION_TYPE) :> sig
    type envdelta = CT.envdelta
    val bnode : Primitive.configuration -> DependencyGraph.bnode -> envdelta
end = struct

    structure DG = DependencyGraph

    type envdelta = CT.envdelta

    infix o'
    fun (f o' g) (x, y) = f (g x, y)

    fun farnode near (NONE, n) = CT.nofilter (near n)
      | farnode near (SOME s, n) = CT.filter (near n, s)

    fun bnode c (DG.PNODE p) = CT.primitive c p
      | bnode c (DG.BNODE { bininfo, localimports = li, globalimports = gi }) =
	case CT.lookstable bininfo of
	    SOME e => e
	  | NONE => let
		val ge = foldl (CT.layer o' farbnode c) (CT.pervasive c) gi
		val le = foldl (CT.layer o' (CT.nofilter o bnode c)) ge li
		val e = CT.dostable (bininfo, le, c)
	    in
		CT.memostable (bininfo, e);
		e
	    end

    and farbnode c = farnode (bnode c)

    fun snode c (DG.SNODE { smlinfo, localimports = li, globalimports = gi }) =
	case CT.looksml smlinfo of
	    SOME e => e
	  | NONE => let
		val ge = foldl (CT.layer o' farsbnode c) (CT.pervasive c) gi
		val le = foldl (CT.layer o' (CT.nofilter o snode c)) ge li
		val e = CT.dosml (smlinfo, le, c)
	    in
		CT.memosml (smlinfo, e);
		e
	    end

    and sbnode c (DG.SB_BNODE b) = bnode c b
      | sbnode c (DG.SB_SNODE s) = snode c s

    and farsbnode c = farnode (sbnode c)
end
