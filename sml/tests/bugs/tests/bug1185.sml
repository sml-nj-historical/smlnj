(* bug1185.sml *)

structure A = 
struct

datatype context 
  = NoContext
  | Local of unit

fun pp_context (Local a1) = ()
  | pp_context _ = ()

end      
