functor DummyCCallsFn (T: MLTREE) :> C_CALLS where T = T = struct
    structure T = T
    datatype c_arg 
      = ARG of T.rexp	
      | FARG of T.fexp
      | ARGS of c_arg list

    fun genCall { name, proto, structRet, args } =
	ErrorMsg.impossible "C-calls not implemented"
end
