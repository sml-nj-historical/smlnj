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
   structure CFG : CONTROL_FLOW_GRAPH

   sharing I.T = T
   sharing CFG.I = I
   sharing CFG.P = T.PseudoOp

   (* 
    * The reducer is given to the client during the compilation of
    * the user extensions.
    *)
   type reducer = 
     (I.instruction,I.C.cellset,I.operand,I.addressing_mode,CFG.cfg) T.reducer

   val compileSext : reducer -> {stm:T.sext, an:T.an list} -> unit
   val compileRext : reducer -> {e:T.ty * T.rext, rd:CellsBasis.cell, an:T.an list} -> unit
   val compileFext : reducer -> {e:T.ty * T.fext, fd:CellsBasis.cell, an:T.an list} -> unit
   val compileCCext : reducer -> {e:T.ty * T.ccext, ccd:CellsBasis.cell, an:T.an list} -> unit
end

signature MLTREECOMP = 
sig
   structure T : MLTREE 
   structure I : INSTRUCTIONS where T = T
   structure Gen : MLTREEGEN where T = T
(*
      sharing T = Gen.T = I.T
*)

   structure CFG : CONTROL_FLOW_GRAPH 
      where I = I and P = T.PseudoOp
(*
	  sharing CFG.I = I
	  sharing CFG.P = Gen.T.PseudoOp
*)

   type instrStream = (I.instruction, I.C.cellset, CFG.cfg) T.stream  
   type mltreeStream = (T.stm, T.mlrisc list, CFG.cfg) T.stream 

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
