(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)
signature MLTREECOMP = sig
    structure T : MLTREE
    structure I : INSTRUCTIONS
    structure S : INSTRUCTION_STREAM
      sharing S.B = T.BNames
      sharing S.P = T.PseudoOp

    (* 
     * The instruction selection phase converts an instruction stream
     * into a mltree stream.
     *)
    val selectInstructions : 
        (I.instruction,
         T.mlrisc list,
         I.C.regmap * Annotations.annotations) S.stream ->
        {  mltreeComp : T.mltree -> unit,
           mlriscComp : T.stm -> unit,
           emitInstr  : I.instruction -> unit
        }
end

