(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)
signature MLTREECOMP = 
sig
    structure T : MLTREE 
    structure I : INSTRUCTIONS
       sharing T.Constant = I.Constant

    (* 
     * The instruction selection phase converts an instruction stream
     * into a mltree stream.  Please see the file "instructions/stream.sig"
     * for description of the stream interface.
     *
     * Note: the mltree stream does NOT support direct instruction emission.
     * For equivalent functionality, you can use the emit method 
     * of the instruction stream instead.
     *)
    val selectInstructions : 
        (I.instruction,I.C.regmap) T.stream ->   (* instruction stream *)
        (T.stm,I.C.regmap) T.stream              (* mltree stream *)
end
