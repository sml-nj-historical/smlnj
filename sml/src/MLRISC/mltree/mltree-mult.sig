(*
 * Let's generate good multiplication/division code!
 *
 * -- Allen 
 *)
signature MLTREE_MULT_DIV =
sig

   structure T : MLTREE
   structure I : INSTRUCTIONS
   structure C : CELLS
      sharing C = I.C
      sharing I.Constant = T.Constant

   exception TooComplex

   val multiply : {r:C.cell,i:int,d:C.cell} -> I.instruction list 

   val divide   : { mode:T.Basis.rounding_mode,
                    stm :('s,'r,'f,'c) T.stm -> unit
                  } -> {r:C.cell,i:int,d:C.cell} -> I.instruction list

end
