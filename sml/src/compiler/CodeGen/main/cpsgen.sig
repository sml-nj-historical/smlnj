signature CPSGEN = sig
  structure MachSpec : MACH_SPEC
  val codegen : CPS.function list * (CPS.lvar -> (int * int))
                * ErrorMsg.complainer -> unit
end (* signature CPSGEN *)


(*
 * $Log: cpsgen.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
