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

   (* The semantics of roundToZero{r,i,d} is:
    *   if r >= 0 then d <- r
    *   else d <- r + i
    *)

   val divide   : { mode:T.rounding_mode,
                    roundToZero : {ty:T.ty,r:C.cell,i:int,d:C.cell} 
                                    -> unit
                  } -> 
                  {r:C.cell,i:int,d:C.cell} -> I.instruction list

end
