(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)
signature MLTREECOMP = 
sig
   structure T : MLTREE 
   structure I : INSTRUCTIONS
   structure C : CELLS
   structure Gen : MLTREEGEN
      sharing T = Gen.T
      sharing T.LabelExp = I.LabelExp
      sharing I.C = C

   type instrStream = (I.instruction,C.regmap,C.cellset) T.stream  
   type ('s,'r,'f,'c) mltreeStream = 
        (('s,'r,'f,'c) T.stm,C.regmap,('s,'r,'f,'c) T.mlrisc list) T.stream 
   type ('s,'r,'f,'c) reducer = 
     (I.instruction,C.regmap,C.cellset,I.operand,I.addressing_mode,'s,'r,'f,'c) 
       T.reducer
   type ('s,'r,'f,'c) extender = 
     (I.instruction,C.regmap,C.cellset,I.operand,I.addressing_mode,'s,'r,'f,'c) 
       T.extender

    (* 
     * The instruction selection phase converts an instruction stream
     * into a mltree stream.  Please see the file "instructions/stream.sig"
     * for description of the stream interface.
     *
     * Note: the mltree stream does NOT support direct instruction emission.
     * Fo equivalent functionality, you can use the emit method 
     * of the instruction stream instead.
     *)
   val selectInstructions : 
        ('s,'r,'f,'c) extender -> instrStream -> ('s,'r,'f,'c) mltreeStream
end
