(* c-calls.sig
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *)

signature C_CALLS =
  sig

    structure T : MLTREE

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)
      | ARGS of c_arg list
	  (* list of arguments corresponding to the contents of a C struct *)

  (* translate a C function call with the given argument list into
   * a MLRISC statement list.  The structRet function is called
   * to allocate space for returning a C struct.  The result of
   * genCall is a mlrisc list specifying where the result is and the
   * MLRisc statements that implement the calling sequence.  Functions
   * with void return type have no result, most others have one result,
   * but some conventions may flatten larger arguments into multiple
   * registers (e.g., a register pair for long long results).
   *)
    val genCall : {
	    name  : T.rexp,
            proto : CTypes.c_proto,
            structRet : {szb : int, align : int} -> T.rexp,
            args : c_arg list
	  } -> {
	    callseq : T.stm list,
	    result: T.mlrisc list
	  }

  end
