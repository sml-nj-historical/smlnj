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

   val multiply : {r:C.register,i:int,d:C.register} -> I.instruction list 

   (* The semantics of roundToZero{r,i,d} is:
    *   if r >= 0 then d <- r
    *   else d <- r + i
    *)

   val divide   : { mode:T.rounding_mode,
                    roundToZero : {ty:T.ty,r:C.register,i:int,d:C.register} 
                                    -> unit
                  } -> 
                  {r:C.register,i:int,d:C.register} -> I.instruction list

end
