(* code-string.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *)

structure CodeString : CODE_STRING =
  struct

    type code_string = CodeObj.code_object

    val obj = ref (NONE : CodeObj.code_object option)
    val arr = ref (Word8Array.array(0, 0w0))

    fun init sz = let
	  val co = CodeObj.alloc sz
	  in
	    obj := SOME co;
	    arr := CodeObj.bytes co
	  end

    fun update (i, b) = Word8Array.update (!arr, i, b)

    fun getCodeString () = let
	  val co = valOf(!obj)
	  in
	    arr := Word8Array.array(0, 0w0);
	    obj := NONE;
	    co
	  end

  end;

