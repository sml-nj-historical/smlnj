(* bool-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from bool.mldoc (v. 1.9; 2000-05-25)
 *)

signature BOOL =
  sig
    datatype bool = datatype bool
    val not : bool -> bool
    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
    val fromString : string -> bool option
    val toString : bool -> string
    
  end
