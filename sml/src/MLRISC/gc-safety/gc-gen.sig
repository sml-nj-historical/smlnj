(*
 * This module is reponsible for generating garbage collection 
 * code for all gc-points in the program.  That is, we delay the generation
 * of garbage collection code until all optimizations have been performed.
 * A callback is invoked at all GC safe-points with the appropriate type 
 * information.  The callback is responsible for generating the appropriate
 * code to save and restore all roots and call the garbage collector.
 *)
signature GC_GEN =
sig

   structure C  : CELLS
   structure T  : MLTREE
   structure IR : MLRISC_IR
   structure GC : GC_TYPE
     sharing T.Constant = IR.I.Constant
     sharing T.PseudoOp = IR.CFG.P
     sharing C          = IR.I.C

   type callgcCallback =
        { id          : int,                        (* basic block id *)
          gcLabel     : Label.label,                (* label of gc block *)
          returnLabel : Label.label,                (* label of return block *)
          roots       : (C.cell * GC.gctype) list,  (* root set *)
          stream      : (T.stm,C.regmap) T.stream   (* code generator *)
        } -> unit

   val gcGen : {callgc : callgcCallback} -> IR.IR -> IR.IR

end
