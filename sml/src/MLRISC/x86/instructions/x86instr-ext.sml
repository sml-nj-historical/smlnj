(* x86instr-ext.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * extensions to the x86 instruction set.
 *)

structure X86InstrExt = struct
  datatype fsz = single | double | extended

  datatype ('r, 'f) sext 
    = PUSHL of 'r
	(* push a 32 bit value onto the H/W stack *)
    | PUSHf of {sz:fsz, fexp: 'f}
	(* push a floating point value onto the H/W stack *)
end
