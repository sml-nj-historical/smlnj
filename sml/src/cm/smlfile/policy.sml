signature POLICY = sig

    type policy

    val default : policy

    val mkBinPath : policy -> AbsPath.t -> AbsPath.t
    val mkSkelPath : policy -> AbsPath.t -> AbsPath.t
end

structure Policy :> POLICY = struct

    type policy = Dummy.t

    val default = Dummy.v

    fun cmpath (d, s) = let
	val { dir = d0, file = f } = AbsPath.splitDirFile s
	val d1 = AbsPath.joinDirFile { dir = d0, file = "CM" }
	val d2 = AbsPath.joinDirFile { dir = d1, file = d }
    in
	AbsPath.joinDirFile { dir = d2, file = f }
    end

    fun mkBinPath _ s = cmpath ("bin", s)
    fun mkSkelPath _ s = cmpath ("SKEL", s)
end
