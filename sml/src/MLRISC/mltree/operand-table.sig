(*
 * A table for storing operands for a compilation unit.
 * We give each distinct operand a unique (negative) value number.
 *)
signature OPERAND_TABLE =
sig

   structure I : INSTRUCTIONS

   type operandTable
   type value = int

   datatype const =
     IMMED of int           (* integer operand *)
   | OPERAND of I.operand   (* other operand *)
   | LABEL of Label.label   (* a label *)

   exception NoLabel
   exception NoOperand
   exception NoConst

   (* Create a new table *)
   val create  : int ref -> operandTable 

   (* Lookup/create *)
   val immed   : operandTable -> int -> value
   val operand : operandTable -> I.operand -> value
   val label   : operandTable -> Label.label -> value 

   (* Value number -> int/operand/label *)
   val const    : operandTable -> value -> const

end
