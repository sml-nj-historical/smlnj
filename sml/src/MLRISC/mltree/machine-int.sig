(*
 * How to evaluate constants for various widths.  
 * The widths are dynamic.  
 *)
signature MACHINE_INT =
sig

   type machine_int = IntInf.int
   type ty = int

   exception MLTreeArith

   (* some common constants *)
   val int_0   : machine_int
   val int_1   : machine_int
   val int_2   : machine_int
   val int_3   : machine_int
   val int_4   : machine_int
   val int_7   : machine_int
   val int_8   : machine_int
   val int_m1  : machine_int (* ~1 *)
   val int_m2  : machine_int (* ~2 *)
   val int_m3  : machine_int (* ~3 *)
   val int_m4  : machine_int (* ~4 *)
   val int_15  : machine_int
   val int_16  : machine_int
   val int_31  : machine_int
   val int_32  : machine_int
   val int_63  : machine_int
   val int_64  : machine_int
   val int_0xff : machine_int
   val int_0x100 : machine_int
   val int_0xffff : machine_int
   val int_0x10000 : machine_int
   val int_0xffffffff : machine_int
   val int_0x100000000 : machine_int

   (* machine_int <-> other types *)
   val fromString  : ty * string -> machine_int (* raises MLTreeArith *)
   val fromInt     : ty * int -> machine_int
   val fromWord    : ty * word -> machine_int
   val fromWord32  : ty * Word32.word -> machine_int
   val toString    : ty * machine_int -> string
   val toHexString : ty * machine_int -> string
   val toBinString : ty * machine_int -> string
   val toInt       : ty * machine_int -> int
   val toWord      : ty * machine_int -> word
   val toWord32    : ty * machine_int -> Word32.word

    (* when in doubt, use this! *)
   val narrowToType : ty * IntInf.int -> machine_int

   (* queries *)
   val isNeg    : machine_int -> bool
   val isPos    : machine_int -> bool
   val isZero   : machine_int -> bool
   val isNonNeg : machine_int -> bool
   val isNonPos : machine_int -> bool
   val isEven   : machine_int -> bool
   val isOdd    : machine_int -> bool

   (* two's complement operators *)
   val NEG   : ty * machine_int -> machine_int
   val ABS   : ty * machine_int -> machine_int
   val ADD   : ty * machine_int * machine_int -> machine_int
   val SUB   : ty * machine_int * machine_int -> machine_int
   val MULS  : ty * machine_int * machine_int -> machine_int
   val DIVS  : ty * machine_int * machine_int -> machine_int
   val QUOTS : ty * machine_int * machine_int -> machine_int
   val REMS  : ty * machine_int * machine_int -> machine_int
   val MULU  : ty * machine_int * machine_int -> machine_int
   val DIVU  : ty * machine_int * machine_int -> machine_int
   val QUOTU : ty * machine_int * machine_int -> machine_int
   val REMU  : ty * machine_int * machine_int -> machine_int
   val ABST  : ty * machine_int -> machine_int
   val NEGT  : ty * machine_int -> machine_int
   val ADDT  : ty * machine_int * machine_int -> machine_int
   val SUBT  : ty * machine_int * machine_int -> machine_int
   val MULT  : ty * machine_int * machine_int -> machine_int
   val DIVT  : ty * machine_int * machine_int -> machine_int
   val QUOTT : ty * machine_int * machine_int -> machine_int
   val REMT  : ty * machine_int * machine_int -> machine_int

   (* bit operators *)
   val NOTB  : ty * machine_int -> machine_int
   val ANDB  : ty * machine_int * machine_int -> machine_int
   val ORB   : ty * machine_int * machine_int -> machine_int
   val XORB  : ty * machine_int * machine_int -> machine_int
   val EQVB  : ty * machine_int * machine_int -> machine_int
   val SLL   : ty * machine_int * machine_int -> machine_int
   val SRL   : ty * machine_int * machine_int -> machine_int
   val SRA   : ty * machine_int * machine_int -> machine_int
   val BITSLICE : ty * (int * int) list * machine_int -> machine_int

   (* Other useful operators *)
   val Sll       : ty * machine_int * int -> machine_int
   val Srl       : ty * machine_int * int -> machine_int
   val Sra       : ty * machine_int * int -> machine_int
   val pow2      : int -> machine_int
   val maxOfType : ty -> machine_int
   val minOfType : ty -> machine_int
   val isInTypeRange : ty * machine_int -> bool

   val bitOf     : ty * machine_int * int -> word        (* 0w0 or 0w1 *)
   val byteOf    : ty * machine_int * int -> word        (* 8 bits *)
   val halfOf    : ty * machine_int * int -> word        (* 16 bits *)
   val wordOf    : ty * machine_int * int -> Word32.word (* 32 bits *)
  
   (* type promotion *)
   val SX    : ty * ty * machine_int -> machine_int
   val ZX    : ty * ty * machine_int -> machine_int

   (* comparisions *)
   val EQ  : ty * machine_int * machine_int -> bool
   val NE  : ty * machine_int * machine_int -> bool
   val GT  : ty * machine_int * machine_int -> bool
   val GE  : ty * machine_int * machine_int -> bool
   val LT  : ty * machine_int * machine_int -> bool
   val LE  : ty * machine_int * machine_int -> bool
   val LTU : ty * machine_int * machine_int -> bool
   val GTU : ty * machine_int * machine_int -> bool
   val LEU : ty * machine_int * machine_int -> bool
   val GEU : ty * machine_int * machine_int -> bool
end
