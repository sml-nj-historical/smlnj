signature C_CALL = 
  sig

    structure T : MLTREE
    structure SA : STAGED_ALLOCATION

    datatype c_arg 
      = ARG of T.rexp	
	  (* rexp specifies integer or pointer; if the 
           * corresponding parameter is a C struct, then 
	   * this argument is the address of the struct. 
	   *)
      | FARG of T.fexp
	  (* fexp specifies floating-point argument *)

    val layout : CTypes.c_proto -> {
	    argLocs : SA.loc list list,	        (* argument/parameter assignment; nested lists are for passing structs *)
	    argMem : {szb : int, align : int},	(* memory requirements for stack-allocated *)
						(* arguments; this value can be passed to *)
						(* the paramAlloc callback. *)
	    resLocs : SA.loc list,	        (* result location *)
	    structRetLoc : {szb : int, align : int} option
	  }

  (* translate a C function call with the given argument list into
   * a MLRISC statement list.  The arguments are as follows:
   *
   *	name			-- an expression that speficies the function.
   *	proto			-- the function's prototype
   *	paramAlloc		-- this callback takes the size and alignment
   *				   constraints on the parameter-passing area
   *				   in the stack.  If it returns true, then the
   *				   space for the parameters is allocated by
   *				   client; otherwise genCall allocates the space.
   *    structRet		-- this callback takes the size and alignment
   *				   of space required for returning a struct
   *				   value.  It returns the address of the
   *				   reserved space.
   *	saveRestoreDedicated	-- this callback takes a list of registers
   *				   that the call kills and should return an
   *				   instruction sequence to save/restore any
   *				   registers that the client run-time model
   *				   expects to be preserved (e.g., allocation
   *				   pointers).
   *    callComment		-- if present, the comment string is attached
   *				   the CALL instruction as a COMMENT annotation.
   *    args			-- the arguments to the call.  The assumption is
   *				   that any required sign or zero extension has
   *				   already been done.
   *
   * The result of genCall is a mlrisc list specifying where the result
   * is and the MLRisc statements that implement the calling sequence.
   * Functions with void return type have no result, most others have
   * one result, but some conventions may flatten larger arguments into
   * multiple registers (e.g., a register pair for long long results).
   *
   * The implementation of genCall will return a statement sequence with the
   * following order:
   *
   *	<argument area allocation>
   *	<setup arguments>
   *	<save dedicated registers>
   *	<call C function>
   *	<restore dedicated registers>
   *	<free argument area>
   *	<copy result into fresh registers>
   *
   * WARNING: if the client's implementation of structRet uses the stack
   * pointer to address the struct-return area, then paramAlloc should always
   * handle allocating space for the parameter area (i.e., return true).
   *)
    val genCall : {
	    name  : T.rexp,
            proto : CTypes.c_proto,
	    paramAlloc : {szb : int, align : int} -> bool,
            structRet : {szb : int, align : int} -> T.rexp,
	    saveRestoreDedicated :
	      T.mlrisc list -> {save: T.stm list, restore: T.stm list},
	    callComment : string option, 
            args : c_arg list
	  } -> {
	    callseq : T.stm list,
	    result: T.mlrisc list
	  }

  (* Callee-save registers as defined in the C calling convention.  Note that
   * these do not include special registers (e.g., stack and frame-pointers)
   * that are preserved across calls.
   *)
    val calleeSaveRegs : T.reg list	(* C callee-save registers *)
    val calleeSaveFRegs : T.reg list	(* C callee-save floating-point registers *)

    val callerSaveRegs : T.reg list	(* C caller-save registers *)
    val callerSaveFRegs : T.reg list	(* C caller-save floating-point registers *)

  end (* C_CALL *)
