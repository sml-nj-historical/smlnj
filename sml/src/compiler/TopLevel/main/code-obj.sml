(* code-obj.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * An interface for manipulating code objects.
 *)

structure CodeObj :> CODE_OBJ =
  struct

    structure W8A = Word8Array
    structure W8V = Word8Vector
    type object = Unsafe.Object.object

    datatype code_object = C of {
	obj : Word8Array.array,
	name : string option
      }

    type csegments = {
	c0 : code_object,
	cn : code_object list, 
	data : Word8Vector.vector
      }

    type executable = object -> object

  (* raised by input when there are insufficient bytes *)
    exception FormatError

    local
      structure CI = Unsafe.CInterface
    in
    val allocCode : (int * string option) -> W8A.array =
	  CI.c_function "SMLNJ-RunT" "allocCode"
    val mkLiterals : W8V.vector -> object = CI.c_function "SMLNJ-RunT" "mkLiterals"
    val mkExec : W8A.array -> executable = CI.c_function "SMLNJ-RunT" "mkExec"
    end (* local *)

  (* Allocate an unintialized code object of the given number of bytes.
   * The second argument is the optional name of the object.
   *)
    fun alloc (n, optName) = (
	  if (n <= 0) then raise Size else ();
	  C{obj = allocCode (n, optName), name = optName})

  (* Allocate a code object of the given size and initialize it
   * from the input stream.  The third argument is the optional
   * name of the object.
   * NOTE: someday, we might read the data directly into the code
   * object, but this will require hacking around with the reader.
   *)
    fun input (inStrm, sz, optName) = let
	  val (co as C{obj, ...}) = alloc (sz, optName)
	  val data = BinIO.inputN (inStrm, sz)
	  in
	    if (W8V.length data < sz)
	      then (
		Control.Print.say(concat[
		    "Bin file format error: expected ", Int.toString sz,
		    " bytes, but only found ", Int.toString(W8V.length data)
		  ]);
		raise FormatError)
	      else ();
	    W8A.copyVec{src=data, si=0, len=NONE, dst=obj, di=0};
	    co
	  end

  (* Output a code object to the given output stream *)
    fun output (outStrm, C{obj, ...}) = (
	  BinIO.output(outStrm, Unsafe.cast obj);
	  BinIO.flushOut outStrm)

  (* View the code object as an updatable array of bytes. *)
    fun bytes (C{obj, ...}) = obj

  (* View the code object as an executable.  This has the side-effect
   * of flushing the instruction cache.
   *)
    fun exec (C{obj, ...}) = mkExec obj

  (* return the size of the code object *)
    fun size (C{obj, ...}) = W8A.length obj

  end;

(*
 * $Log$
 *)

