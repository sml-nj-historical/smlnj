(* c-call-stk-fn.sml
 *
 * Common functions for machines that pass arguments in registers and 
 * on the stack.
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor CCallStkFn (
    structure C : CELLS
    structure T : MLTREE
    val spReg : T.rexp
    val wordTy : T.ty
  )  = 
  struct

    structure CTy = CTypes

  end (* CCallFn *)
