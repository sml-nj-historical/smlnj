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
     INT of int                        (* small integer operand *)
   | INTINF of MachineInt.machine_int  (* large integer operand *)
   | OPERAND of I.operand              (* other operand *)

   type valueNumber =
      { int     : int -> value,
        word    : word -> value,
        word32  : Word32.word -> value,
        int32   : Int32.int -> value,
        intinf  : IntInf.int -> value,
        operand : I.operand -> value
      }

   exception NoOperand
   exception NoInt
   exception NoIntInf
   exception NoConst

   (* Create a new table *)
   val create  : int ref -> operandTable 

   (* Lookup methods *)

   (* Value number -> int/operand/label *)
   val const       : operandTable -> value -> const
   val int         : operandTable -> int -> value
   val word        : operandTable -> word -> value
   val int32       : operandTable -> Int32.int -> value
   val word32      : operandTable -> Word32.word -> value
   val intinf      : operandTable -> IntInf.int -> value
   val operand     : operandTable -> I.operand -> value

   (* Create new value numbers *)
   val makeNewValueNumbers : operandTable -> valueNumber

   (* Lookup but don't create *)
   val lookupValueNumbers : operandTable -> valueNumber


end
