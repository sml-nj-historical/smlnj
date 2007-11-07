(* pre-os.sml
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 * All rights reserved.
 *
 * This is the OS structure(s) with only types, so that the signatures
 * can compile.
 *)

structure OS =
  struct
    (* the integer code; we may need to beef this up *)
    type syserror = int

    structure Process =
      struct
	type status = int (* should this be Word8.word ?*)
      end

    structure IO =
      struct
        datatype iodesc = IODesc of SMLBasis.ML_iodesc_t
      end

  end;

structure PreOS = OS;


