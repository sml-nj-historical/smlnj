(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

(*
 * This signature describes how MLTree extensions are compiled.
 *)
signature MLTREE_EXTENSION_COMP =
sig
   structure T : MLTREE
   structure I : INSTRUCTIONS
   structure C : CELLS
      sharing T.LabelExp = I.LabelExp
      sharing I.C = C

   (* 
    * The reducer is given to the client during the compilation of
    * the user extensions.
    *)
   type reducer = 
     (I.instruction,C.cellset,I.operand,I.addressing_mode) T.reducer

   val compileSext : reducer -> {stm:T.sext, an:T.an list} -> unit
   val compileRext : reducer -> {e:T.ty * T.rext, rd:C.cell, an:T.an list} -> unit
   val compileFext : reducer -> {e:T.ty * T.fext, fd:C.cell, an:T.an list} -> unit
   val compileCCext : reducer -> {e:T.ty * T.ccext, ccd:C.cell, an:T.an list} -> unit
end

signature MLTREECOMP = 
sig
   structure T : MLTREE 
   structure I : INSTRUCTIONS
   structure C : CELLS
   structure Gen : MLTREEGEN
      sharing T = Gen.T
      sharing T.LabelExp = I.LabelExp
      sharing I.C = C

   type instrStream = (I.instruction,C.cellset) T.stream  
   type mltreeStream = (T.stm,T.mlrisc list) T.stream 

    (* 
     * The instruction selection phase converts an instruction stream
     * into a mltree stream.  Please see the file "instructions/stream.sig"
     * for description of the stream interface.
     *
     * Note: the mltree stream does NOT support direct instruction emission.
     * Fo equivalent functionality, you can use the emit method 
     * of the instruction stream instead.
     *)
   val selectInstructions : instrStream -> mltreeStream
end
