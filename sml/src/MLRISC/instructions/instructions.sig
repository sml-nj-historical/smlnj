(*
 *  This signature specifies the abstract view of an instruction.
 *)
signature INSTRUCTIONS =
sig
   structure C   : CELLS
   structure CB  :CELLS_BASIS = CellsBasis
   type operand             (* operands supported by architecture *)
   type ea              (* effective address for accessing memory *)
   type addressing_mode                        (* addressing mode *)
   type instr                       (* architecture instructions  *)
   
   datatype instruction =                   (* partially abstract *)
       LIVE of {regs: C.cellset, spilled: C.cellset}
     | KILL of {regs: C.cellset, spilled: C.cellset}
     | COPYXXX of {k: CB.cellkind, dst: CB.cell list, src: CB.cell list}
     | ANNOTATION of {i: instruction, a: Annotations.annotation}
     | INSTR of instr
end
