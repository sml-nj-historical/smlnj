(* pre-basis-structs.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * These are basis structures with only types, so that the basis signatures
 * can compile.
 *
 *)

structure Int =
  struct
    type int = PrimTypes.int
  end;

structure Int31 =
  struct
    type int = PrimTypes.int
  end;

structure Int32 =
  struct
    type int = PrimTypes.int32
  end;

structure Word =
  struct
    type word = PrimTypes.word
  end;

structure Word8 =
  struct
    type word = PrimTypes.word8
  end;

structure Word31 =
  struct
    type word = PrimTypes.word
  end;

structure Word32 =
  struct
    type word = PrimTypes.word32
  end;

structure Real64 =
  struct
    type real = PrimTypes.real
  end;

structure StringCvt =
  struct
    datatype radix = BIN | OCT | DEC | HEX
    datatype realfmt
      = EXACT
      | SCI of int option
      | FIX of int option
      | GEN of int option
    type ('a, 'b) reader = 'b -> ('a * 'b) option
  end;

structure String =
  struct
    type string = PrimTypes.string
  end

(*
 * $Log$
 *)
