(*
 * Extract properties from RTLs
 *)
signature RTL_PROPERTIES =
sig

   structure I   : INSTRUCTIONS
   structure C   : CELLS
   structure RTL : MLTREE_RTL
   structure T   : MLTREE
      sharing I.C   = C
      sharing RTL.T = T

   datatype opnkind = 
     IMM (* immed value *)
   | REG (* normal register *)
   | FIX (* fixed register *)
   | MEM (* memory *)
   | CTRL (* control dependence *)

   (* Return the RTL describing the semantics of an instruction *)
   val rtl : I.instruction -> RTL.rtl  

   (* Return the def/use of an instruction *) 
   val defUse : { immed   : int -> C.cell, 
                  label   : Label.label -> C.cell,
                  operand : I.operand -> C.cell
                } -> 
                I.instruction -> 
                C.cell list * C.cell list (* dst/src *)

   (* Return the def/use of an instruction with cellkind information *) 
   val defUseWithCellKind : 
                { immed   : int -> C.cell, 
                  label   : Label.label -> C.cell,
                  operand : I.operand -> C.cell
                } -> 
                I.instruction -> 
                (C.cell * C.cellkind) list * (C.cell * C.cellkind) list 


   (* Return the operand kinds of an instruction. *)
   val opnKind : I.instruction -> opnkind list * opnkind list (* dst/src *)

   (* Initialize the cellkinds of variables *)
   val updateCellKind : {update:C.cell * C.cellkind -> unit} -> 
                        I.instruction -> unit

end

