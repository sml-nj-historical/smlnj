structure VarargCCall =
  struct

    structure DL = DynLinkage
    structure Consts = VarargCCallConstants

    datatype argument = I of int | R of real | B of bool | S of string

    fun main's s = DL.lib_symbol (DL.main_lib, s)
    val malloc_h = main's "malloc"
    val free_h = main's "free"

    exception OutOfMemory

    fun sys_malloc (n : Word32.word) =
	let val w_p = RawMemInlineT.rawccall :
		      Word32.word * Word32.word * (unit * word -> string) list
		      -> Word32.word
	    val a = w_p (DL.addr malloc_h, n, [])
	in if a = 0w0 then raise OutOfMemory else a
	end

    fun sys_free (a : Word32.word) =
	let val p_u = RawMemInlineT.rawccall :
		      Word32.word * Word32.word * (unit * string -> unit) list
		      -> unit
	in p_u (DL.addr free_h, a, [])
	end

    fun alloc bytes = sys_malloc (Word32.toLargeWord bytes)
    fun free a = sys_free a

    type addr = Word32.word
    infix ++ 
    fun (a: addr) ++ i = a + Word32.fromInt i

    fun set' (p, w) = RawMemInlineT.w32s (p, w)
    fun nxt' p = p ++ 1

    fun cpML' { from, to } = let
	val n = String.size from
	fun loop (i, p) =
	    if i >= n then set' (p, 0w0)
	    else (set' (p, Word32.fromInt (Char.ord
					       (String.sub (from, i))));
		  loop (i+1, nxt' p))
    in
	loop (0, to)
    end

    fun dupML' s = let
	    val z = alloc (Word32.fromInt (String.size s + 1))
	in
	    cpML' { from = s, to = z };
	    z
	end

  (* default width of a field in a zipped argument *)
    val defaultWidthB = Word32.fromInt Consts.defaultWidthB
    val argOffB = Word32.fromInt Consts.argOff * defaultWidthB
    val kindOffB = Word32.fromInt Consts.kindOff * defaultWidthB
    val locOffB = Word32.fromInt Consts.locOff * defaultWidthB
    val tyOffB = Word32.fromInt Consts.tyOff * defaultWidthB
    val zippedArgSzB = Word32.fromInt Consts.zippedArgSzB

    fun set (p, off, v) = set'(p+off, v)

  (* track strings allocated for the call *)
    local
	val allocatedStrs = ref ([] : Word32.word list)
    in
	fun freeStrs () = (
	       List.app free (!allocatedStrs);
	       allocatedStrs := [])
	fun addStr s = allocatedStrs := s :: !allocatedStrs
    end

  (* encode the argument field *)
    fun encodeArg (arrPtr, I i) = set(arrPtr, argOffB, Word32.fromInt i)
      | encodeArg (arrPtr, S s) = let
	    val strPtr = dupML' s
	    in 
	       addStr strPtr;
	       set(arrPtr, argOffB, strPtr)
	    end
      | encodeArg (arrPtr, B b) = set(arrPtr, argOffB, if b then 0w1 else 0w0)
      | encodeArg (arrPtr, R r) = RawMemInlineT.f64s (arrPtr+argOffB, r)

  (* encode a zipped argument *)
    fun encodeZippedArg ((arg, k, l, ty), arrPtr) = (
	    encodeArg(arrPtr, arg);
	    set(arrPtr, kindOffB, Word32.fromInt k);
	    set(arrPtr, locOffB, Word32.fromInt l);
	    set(arrPtr, tyOffB, Word32.fromInt ty);
	  (* advance the pointer by one zipped argument *)
	    arrPtr + zippedArgSzB
        )

  (* encode an array of zipped arguments *)
    fun encodeZippedArgs args = let
	    val nArgs = List.length args
	    val argsSzB = Word32.fromInt nArgs * zippedArgSzB
	    val arrPtr = alloc argsSzB
            in
	        List.foldl encodeZippedArg arrPtr args;
	        {startCArr=arrPtr, endCArr=argsSzB+arrPtr}
	    end

    fun vararg's s = let
	val lh = DynLinkage.open_lib
		     { name = "./vararg", global = true, lazy = true }
        in 
	    DL.lib_symbol (lh, s)
        end

  (* call the vararg interpreter *)
    fun vararg (cFun, zippedArgs) = let
	    val vararg_h = vararg's Consts.varargInterpreter
	    val callInterp = RawMemInlineT.rawccall :
		      Word32.word * (Word32.word * Word32.word * Word32.word) * 
		      (unit * Word32.word * Word32.word * Word32.word -> Word32.word) list
		      -> Word32.word
	    val cFunAddr = DL.addr (vararg's cFun)
	    val {startCArr, endCArr} = encodeZippedArgs zippedArgs
	 (* call the interpreter *)
	    val x = callInterp (DL.addr vararg_h, (cFunAddr, startCArr, endCArr), [])
	    in 
	        freeStrs();
	        free startCArr;
	        x
	    end

  end
