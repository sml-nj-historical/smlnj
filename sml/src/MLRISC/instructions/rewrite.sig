(*
 * Signature for rewriting (renaming) instructions.
 *)

signature REWRITE_INSTRUCTIONS =
sig

   structure I : INSTRUCTIONS

   val rewriteDef : 
        (I.C.cell -> I.C.cell) * I.instruction * I.C.cell * I.C.cell ->
           I.instruction
   val rewriteUse : 
        (I.C.cell -> I.C.cell) * I.instruction * I.C.cell * I.C.cell ->
           I.instruction
   val frewriteDef : 
        (I.C.cell -> I.C.cell) * I.instruction * I.C.cell * I.C.cell ->
           I.instruction
   val frewriteUse : 
        (I.C.cell -> I.C.cell) * I.instruction * I.C.cell * I.C.cell ->
           I.instruction
end
