signature PEEPHOLE =
sig
   structure I : INSTRUCTIONS

   (* Instructions are in reversed order *)
   val peephole : (I.C.cell -> I.C.cell)  (* regmap *)
                  -> I.instruction list -> I.instruction list

end
