signature CPSGEN = sig
  structure MachSpec : MACH_SPEC
  val codegen : CPS.function list * (CPS.lvar -> (int * int))
                * ErrorMsg.complainer -> unit
end (* signature CPSGEN *)

