(* code-obj.sig
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * An interface for manipulating code objects.
 *)

signature CODE_OBJ =
  sig

    type code_object

    type csegments = {
	c0 : code_object,
	cn : code_object list, 
	data : Word8Vector.vector
      }

    type executable = Unsafe.Object.object -> Unsafe.Object.object

    exception FormatError
	(* raised by input when there are insufficient bytes *)

    val alloc : (int * string option) -> code_object
	(* Allocate an unintialized code object of the given number of bytes.
	 * The second argument is the optional name of the object.
	 *)
    val input : (BinIO.instream * int * string option) -> code_object
	(* Allocate a code object of the given size and initialize it
	 * from the input stream.  The third argument is the optional
	 * name of the object.
	 *)
    val output : (BinIO.outstream * code_object) -> unit
	(* Output a code object to the given output stream *)

    val bytes : code_object -> Word8Array.array
	(* View the code object as an updatable array of bytes. *)

    val exec : code_object -> executable
	(* View the code object as an executable.  This has the side-effect
	 * of flushing the instruction cache.
	 *)

    val size : code_object -> int
	(* return the size of the code object *)

    val mkLiterals : Word8Vector.vector -> Unsafe.Object.object
	(* use the run-time system interpreter to generate a literals
	 * vector from a literal bytecode program.
	 *)

  end;

(*
 * $Log$
 *)

