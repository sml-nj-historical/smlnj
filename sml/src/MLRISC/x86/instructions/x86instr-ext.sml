(* x86instr-ext.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * extensions to the x86 instruction set.
 *)

structure X86InstrExt = struct
  datatype fsz = single | double | extended

  datatype ('s, 'r, 'f, 'c) sext 
    (* push an integer value onto the H/W stack *)
    = PUSHL of 'r
    | POP of 'r

    (* FSTPS/L/T is a way of pulling things off the floating point 
     * stack and must therefore take FREG f as argument 
     *)
    | FSTPS of 'f
    | FSTPL of 'f
    | FSTPT of 'f

    | LEAVE
    | RET of 'r

    | LOCK_CMPXCHGL of ('r * 'r)

end
