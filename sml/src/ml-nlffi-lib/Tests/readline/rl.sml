structure RL : sig
    type prompt
    val noprompt : prompt
    val prompt : string -> prompt
    val rl : prompt -> string
end = struct
    type prompt = (C.uchar, C.ro) C.obj C.ptr'
    val noprompt = C.Ptr.null'
    val prompt = ZString.dupML'
    fun rl p = let
      val cres = F_readline.f' p
      val res = ZString.toML' cres
    in
      C.free' cres;
      res
    end
end
