(* tonflint.sml *)
signature FLINT2NFLINT = 
sig
  val tonflint : FLINT.prog -> NFlint.prog
end

structure Flint2NFlint : FLINT2NFLINT = struct

local 
  open FLINT
  structure NF = NFlint
in

(* generating certified closure-converted nflint code *)
fun tonflint (fk, fv, vl, e) = 
  let val p = (NF.ESCAPE(NONE), fv, [], [], NF.APP(NF.VAR fv, [], []))
   in p
  end (* tonflint *)

end (* toplevel local *)
end (* structure Flint2NFlint *)
