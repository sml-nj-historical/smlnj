signature TOOLS = sig

    type primitive = Dummy.t

    type smlsource =
	{ sourcepath: AbsPath.t, history: string, share: bool option }

    datatype expansion =
	PRIMITIVE of primitive
      | SMLSOURCE of smlsource
      | GROUP of AbsPath.t

    val expand : AbsPath.t * string option -> expansion list
end

structure Tools :> TOOLS = struct

    type primitive = Dummy.t

    type smlsource =
	{ sourcepath: AbsPath.t, history: string, share: bool option }

    datatype expansion =
	PRIMITIVE of primitive
      | SMLSOURCE of smlsource
      | GROUP of AbsPath.t

    fun expand (p, c) = Dummy.f ()
end
