(*
 * Signature for rewriting (renaming) cells inside instructions.
 *)

signature REWRITE_INSTRUCTIONS =
sig

   structure I : INSTRUCTIONS

                                    (* from      to *) 
   val rewriteDef : I.instruction * I.C.cell * I.C.cell -> I.instruction
   val rewriteUse : I.instruction * I.C.cell * I.C.cell -> I.instruction
   val frewriteDef : I.instruction * I.C.cell * I.C.cell -> I.instruction
   val frewriteUse : I.instruction * I.C.cell * I.C.cell -> I.instruction
end
