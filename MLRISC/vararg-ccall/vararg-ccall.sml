structure VarargCCall =
  struct

    structure DL = DynLinkage
    structure V = VarArgs
    structure Consts = VarargCCallConstants

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

    fun encodeArg (V.I i) = Word32.fromInt i
      | encodeArg (V.S s) = dupML' s
      | encodeArg (V.B b) = if b then 0w1 else 0w0
      | encodeArg (V.R r) = raise Fail "todo"

    val defaultWidthB = Word32.fromInt Consts.defaultWidthB
    val argOffB = Word32.fromInt Consts.argOff * defaultWidthB
    val kindOffB = Word32.fromInt Consts.kindOff * defaultWidthB
    val locOffB = Word32.fromInt Consts.locOff * defaultWidthB
    val tyOffB = Word32.fromInt Consts.tyOff * defaultWidthB

    fun set (p, off, v) = set'(p+off, v)

    fun encodeZippedArg (arg, k, l, ty) = let
	  (* 4 elements x 8 bytes per element *)
	    val x = alloc (0w4 * defaultWidthB)
	    in
	       set(x, argOffB, encodeArg arg);
	       set(x, kindOffB, Word32.fromInt k);
	       set(x, locOffB, Word32.fromInt l);
	       set(x, tyOffB, Word32.fromInt ty);
	       x
	    end

    val hdOffB = Word32.fromInt Consts.HD * defaultWidthB
    val tlOffB = Word32.fromInt Consts.TL * defaultWidthB

    fun encodeZippedArgList args = let
	    fun loop [] = Word32.fromInt Consts.NIL
	      | loop (za :: zas) = let
		    val l = alloc(0w2 * defaultWidthB)
		    in
		        set(l, hdOffB, za);
			set(l, tlOffB, loop(zas));
			l
		    end
	    in
	        loop (List.map encodeZippedArg args)
	    end

    fun vararg's s = let
	val lh = DynLinkage.open_lib
		     { name = "./vararg", global = true, lazy = true }
        in 
	    DL.lib_symbol (lh, s)
        end

  (* call the vararg interpreter *)
    fun vararg (cFun, zippedArgs, stkArgSzB) = let
	    val vararg_h = vararg's "vararg_wrapper"
	    val callInterp = RawMemInlineT.rawccall :
		      Word32.word * (Word32.word * Word32.word * Word32.word) * 
		      (unit * Word32.word * Word32.word * Word32.word -> Word32.word) list
		      -> Word32.word
	    val cFunAddr = DL.addr (vararg's cFun)
	    val cArgs = encodeZippedArgList zippedArgs
	    in 
	        callInterp (DL.addr vararg_h, (cFunAddr, cArgs, Word32.fromInt stkArgSzB), [])
	    end

  end
