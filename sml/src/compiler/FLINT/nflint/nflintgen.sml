(* nflintgen.sml *)
signature NFLINTGEN = 
sig
  val nflintgen : FLINT.prog -> NFlint.prog list
end

structure NFlintGen : NFLINTGEN = struct

local 
  structure F2N = Flint2NFlint
  structure N2C = NFlint2CPS
  structure C2C = CPS2Clos
in

 (* generating certified closure-converted nflint code *)
 fun nflintgen p = 
   let val p1 = F2N.tonflint p
       val p2 = N2C.transProg p1
       val p3 = C2C.transProg p2
    in []
   end (* nflintgen *)
   

end (* toplevel local *)
end (* structure Flint2CPS *)
