functor DummyCCallsFn (T: MLTREE) :> C_CALLS where T = T = struct
    structure T = T
    datatype c_arg 
      = ARG of T.rexp	
      | FARG of T.fexp
      | ARGS of c_arg list

    local
	fun unimplemented _ = ErrorMsg.impossible "C-calls not implemented"
    in
        val genCall = unimplemented
        val tmpsp_genCall = unimplemented
    end
end
