(* ppc-macosx.sml
 *
 * COPYRIGHT (c) 2003 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * C function calls for the PowerPC using the MacOS X ABI.
 *
 * Register conventions:
 *
 *    Register	Callee-save	Purpose
 *    --------	-----------	-------
 *	GPR0	   no		Zero
 *	 1	   no		Stack pointer
 *	 2	   no		scratch (TOC on AIX)
 *	 3	   no		arg0 and return result
 *	4-10	   no		arg1-arg7
 *	 11	   no		scratch
 *	 12	   no		holds taget of indirect call
 *	13-31	   yes		callee-save registers
 *
 *	FPR0	   no		scratch
 *	1-13	   no		floating-point arguments
 *	14-31	   yes		floating-point callee-save registers
 *
 *	V0-V1	   no		scratch vector registers
 *	 2-13	   no		vector argument registers
 *	14-19	   no		scratch vector registers
 *	20-31	   yes		callee-save vector registers
 *
 *	 LR	   no		link register holds return address
 *
 *	CR0-CR1	   no		scratch condition registers
 *	 2-4	   yes		callee-save condition registers
 *	 5-7	   no		scratch condition registers
 *
 * Calling convention:
 *
 *    Return result:
 *	+ Integer and pointer results are returned in GPR3
 *	+ 64-bit integers (long long) returned in GPR3/GPR4
 *	+ float/double results are returned in FPR1
 *	+ Struct results are returned in space provided by the caller.
 *	  The address of this space is passed to the callee as an
 *	  implicit first argument in GPR3 and the first real argument is
 *	  passed in GPR4.
 *
 *    Function arguments:
 *	* arguments (except for floating-point values) are passed in
 *	  registers GPR3-GPR10
 *
 * Note also that stack frames are supposed to be 16-byte aligned.
 *)

(* we extend the interface to support generating the stubs needed for
 * dynamic linking (see "Inside MacOS X: Mach-O Runtime Architecture"
 * for details.
 *)
signature PPC_MACOSX_C_CALLS =
  sig
    include C_CALLS

(*
    val genStub : {
	    name  : T.rexp,
            proto : CTypes.c_proto,
	    paramAlloc : {szb : int, align : int} -> bool,
            structRet : {szb : int, align : int} -> T.rexp,
	    saveRestoreDedicated :
	      T.mlrisc list -> {save: T.stm list, restore: T.stm list},
	    callComment : string option,
            args : c_arg list
	  } -> {
	    callseq : T.stm list,
	    result: T.mlrisc list
	  }
*)

  end;

functor PPCMacOSX_CCalls (

    structure T : MLTREE
    val ix : (T.stm, T.rexp, T.fexp, T.ccexp) PPCInstrExt.sext -> T.sext

  ): PPC_MACOSX_C_CALLS = struct
    structure T  = T
    structure Ty = CTypes
    structure C = PPCCells
    structure IX = PPCInstrExt

    fun error msg = MLRiscErrorMsg.error ("PPCCompCCalls", msg)

    datatype arg_loc
      = GPR of C.cell
      | FPR of C.cell
      | STK

    type arg_pos = {
	offset : int,		(* stack offset of memory for argument *)
	loc : arg_loc		(* location where argument is passed *)
      }

    datatype c_arg
      = ARG of T.rexp	    
      | FARG of T.fexp
      | ARGS of c_arg list

    val mem = T.Region.memory
    val stack = T.Region.memory

    val maxRegArgs = 6
    val paramAreaOffset = 68

    fun LI i = T.LI (T.I.fromInt (32, i))

    val GP = C.GPReg
    val FP = C.FPReg

    fun greg r = GP r
    fun oreg r = GP (r + 8)
    fun freg r = FP r

    fun reg32 r = T.REG (32, r)
    fun freg64 r = T.FREG (64, r)

    val sp = oreg 6
    val spreg = reg32 sp

    fun addli (x, 0) = x
      | addli (x, d) = let
	  val d' = T.I.fromInt (32, d)
	  in
	    case x
	     of T.ADD (_, r, T.LI d) =>
		  T.ADD (32, r, T.LI (T.I.ADD (32, d, d')))
	      | _ => T.ADD (32, x, T.LI d')
	  end

    fun argaddr n = addli (spreg, paramAreaOffset + 4*n)

  (* temp location for transfers through memory *)
    val tmpaddr = argaddr 1

    fun roundup (i, a) = a * ((i + a - 1) div a)

    fun intSizeAndAlign Ty.I_char = (1, 1)
      | intSizeAndAlign Ty.I_short = (2, 2)
      | intSizeAndAlign Ty.I_int = (4, 4)
      | intSizeAndAlign Ty.I_long = (4, 4)
      | intSizeAndAlign Ty.I_long_long = (8, 8)

  (* calculate size and alignment for a C type *)
    fun szal (T.C_unsigned ty) = intSizeAndAlign ty
      | szal (T.C_signed ty) = intSizeAndAlign ty
      | szal Ty.C_void = raise Fail "unexpected void type"
      | szal Ty.C_float = (4, 4)
      | szal Ty.C_PTR = (4, 4)
      | szal Ty.C_double = (8, 8)
      | szal (Ty.C_long_double) = (8, 8)
      | szal (Ty.C_ARRAY(t, n)) = let val (s, a) = szal t in (n * s, a) end
      | szal (Ty.C_STRUCT l) = let
(* FIXME: the rules for structs are more complicated (and they also depend
 * on the alignment mode).  In Power alignment, 8-byte quantites like
 * long long and double are 4-byte aligned in structs.
 *)
	(* i: next free memory address (relative to struct start);
	 * a: current total alignment,
	 * l: list of struct member types
	 *)
	  fun pack (i, a, []) =
	    (* when we are done with all elements, the total size
	     * of the struct must be padded out to its own alignment
	     *)
		(roundup (i, a), a)
	    | pack (i, a, t :: tl) = let
		val (ts, ta) = szal t (* size and alignment for member *)
		in
		(* member must be aligned according to its own
		 * alignment requirement; the next free position
		 * is then at "aligned member-address plus member-size";
		 * new total alignment is max of current alignment
		 * and member alignment (assuming all alignments are
		 * powers of 2) *)
		  pack (roundup (i, ta) + ts, Int.max (a, ta), tl)
		end
	  in
	    pack (0, 1, l)
	  end

    fun genCall { name, proto, paramAlloc, structRet, saveRestoreDedicated,
		  callComment, args } = let
	val { conv, retTy, paramTys } = proto
	val _ = case conv of
		    ("" | "ccall") => ()
		  | _ => error (concat ["unknown calling convention \"",
					String.toString conv, "\""])
	val res_szal =
	    case retTy of
		(Ty.C_long_double | Ty.C_STRUCT _) => SOME (szal retTy)
	      | _ => NONE

	val nargwords = let
	    fun loop ([], n) = n
	      | loop (t :: tl, n) =
		loop (tl, (case t of
			       (Ty.C_double | Ty.C_signed Ty.I_long_long |
				Ty.C_unsigned Ty.I_long_long) => 2
			     | _ => 1) + n)
	in
	    loop (paramTys, 0)
	end

	val regargwords = Int.min (nargwords, maxRegArgs)
	val stackargwords = Int.max (nargwords, maxRegArgs) - maxRegArgs

	val stackargsstart = paramAreaOffset + 4 * maxRegArgs

	val scratchstart = stackargsstart + 4 * stackargwords

	(* Copy struct or part thereof to designated area on the stack.
	 * An already properly aligned address (relative to %sp) is
	 * in to_off. *)
	fun struct_copy (sz, al, ARG a, t, to_off, cpc) =
	    (* Two main cases here:
	     *   1. t is C_STRUCT _: in this case "a" computes the address
	     *      of the struct to be copied.
	     *   2. t is some other non-floating type; "a" computes the
	     *      the corresponding value (i.e., not its address).
	     *)
	    let fun ldst ty =
		    T.STORE (ty, addli (spreg, to_off), a, stack) :: cpc
	    in
		case t of
		    (Ty.C_void | Ty.C_PTR |
		     Ty.C_signed (Ty.I_int | Ty.I_long) |
		     Ty.C_unsigned (Ty.I_int | Ty.I_long)) => ldst 32
		  | (Ty.C_signed Ty.I_char | Ty.C_unsigned Ty.I_char) => ldst 8
		  | (Ty.C_signed Ty.I_short | Ty.C_unsigned Ty.I_short) =>
		    ldst 16
		  | (Ty.C_signed Ty.I_long_long |
		     Ty.C_unsigned Ty.I_long_long) => ldst 64
		  | (Ty.C_ARRAY _) =>
		    error "ARRAY within gather/scatter struct"
		  | (Ty.C_STRUCT _) =>
		    (* Here we have to do the equivalent of a "memcpy". *)
		    let val from = a (* argument is address of struct *)
			fun cp (ty, incr) = let
			    fun load_from from_off =
				T.LOAD (32, addli (from, from_off), mem)
			    (* from_off is relative to from,
			     * to_off is relative to %sp *)
			    fun loop (i, from_off, to_off, cpc) =
				if i <= 0 then cpc
				else loop (i - incr,
					   from_off + incr, to_off + incr,
					   T.STORE (ty, addli (spreg, to_off),
						    load_from from_off,
						    stack)
					   :: cpc)
			in
			    loop (sz, 0, to_off, cpc)
			end
		    in
			case al of
			    1 => cp (8, 1)
			  | 2 => cp (16, 2)
			  | _ => (* 4 or more *) cp (32, 4)
		    end
		  | (Ty.C_float | Ty.C_double | Ty.C_long_double) =>
		    error "floating point type does not match ARG"
	    end
	  | struct_copy (_, _, ARGS args, Ty.C_STRUCT tl, to_off, cpc) =
	    (* gather/scatter case *)
	    let fun loop ([], [], _, cpc) = cpc
		  | loop (t :: tl, a :: al, to_off, cpc) = let
			val (tsz, tal) = szal t
			val to_off' = roundup (to_off, tal)
			val cpc' = struct_copy (tsz, tal, a, t, to_off', cpc)
		    in
			loop (tl, al, to_off' + tsz, cpc')
		    end
		  | loop _ =
		    error "number of types does not match number of arguments"
	    in
		loop (tl, args, to_off, cpc)
	    end
	  | struct_copy (_, _, ARGS _, _, _, _) =
	    error "gather/scatter for non-struct"
	  | struct_copy (sz, al, FARG a, t, to_off, cpc) =
	    let fun fldst ty =
		   T.FSTORE (ty, addli (spreg, to_off), a, stack) :: cpc
	    in
		case t of
		    Ty.C_float => fldst 32
		  | Ty.C_double => fldst 64
		  | Ty.C_long_double => fldst 128
		  | _ => error "non-floating-point type does not match FARG"
	    end

	val (stackdelta, argsetupcode, copycode) = let
	    fun loop ([], [], _, ss, asc, cpc) =
		(roundup (Int.max (0, ss - stackargsstart), 8), asc, cpc)
	      | loop (t :: tl, a :: al, n, ss, asc, cpc) = let
		    fun wordassign a =
			if n < 6 then T.MV (32, oreg n, a)
			else T.STORE (32, argaddr n, a, stack)
		    fun wordarg (a, cpc, ss) =
			loop (tl, al, n + 1, ss, wordassign a :: asc, cpc)

		    fun dwordmemarg (addr, region, tmpstore) = let
			fun toreg (n, addr) =
			    T.MV (32, oreg n, T.LOAD (32, addr, region))
			fun tomem (n, addr) =
			    T.STORE (32,
				     argaddr n,
				     T.LOAD (32, addr, region),
				     stack)
			fun toany (n, addr) =
			    if n < 6 then toreg (n, addr) else tomem (n, addr)
		    in
			(* if n < 6 andalso n div 2 = 0 then
			 *     use ldd here once MLRISC gets its usage right
			 * else
			 *   ... *)
			loop (tl, al, n+2, ss,
			      tmpstore @
			      toany (n, addr)
			      :: toany (n+1, addli (addr, 4))
			      :: asc,
			      cpc)
		    end
		    fun dwordarg mkstore =
			if n > 6 andalso n div 2 = 1 then
			    (* 8-byte aligned memory *)
			    loop (tl, al, n+2, ss,
				  mkstore (argaddr n) :: asc,
				  cpc)
			else dwordmemarg (tmpaddr, stack, [mkstore tmpaddr])
		in
		    case (t, a) of
			((Ty.C_void | Ty.C_PTR | Ty.C_ARRAY _ |
			  Ty.C_unsigned (Ty.I_int | Ty.I_long) |
			  Ty.C_signed (Ty.I_int | Ty.I_long)),
			 ARG a) => wordarg (a, cpc, ss)
		      | (Ty.C_signed Ty.I_char, ARG a) =>
			wordarg (T.SX (32, 8, a), cpc, ss)
		      | (Ty.C_unsigned Ty.I_char, ARG a) =>
			wordarg (T.ZX (32, 8, a), cpc, ss)
		      | (Ty.C_signed Ty.I_short, ARG a) =>
			wordarg (T.SX (32, 16, a), cpc, ss)
		      | (Ty.C_unsigned Ty.I_short, ARG a) =>
			wordarg (T.ZX (32, 16, a), cpc, ss)
		      | ((Ty.C_signed Ty.I_long_long |
			  Ty.C_unsigned Ty.I_long_long), ARG a) =>
			(case a of
			     T.LOAD (_, addr, region) =>
			     dwordmemarg (addr, region, [])
			   | _ => dwordarg (fn addr =>
					       T.STORE (64, addr, a, stack)))
		      | (Ty.C_float, FARG a) =>
			(* we use the stack region reserved for storing
			 * %o0-%o5 as temporary storage for transferring
			 * floating point values *)
			(case a of
			     T.FLOAD (_, addr, region) =>
			     wordarg (T.LOAD (32, addr, region), cpc, ss)
			   | _ =>
			     if n < 6 then let
				 val ld = T.MV (32, oreg n,
						T.LOAD (32, tmpaddr, stack))
				 val cp = T.FSTORE (32, tmpaddr, a, stack)
			     in
				 loop (tl, al, n + 1, ss, cp :: ld :: asc, cpc)
			     end
			     else loop (tl, al, n + 1, ss,
					T.FSTORE (32, argaddr n, a, stack)
					:: asc,
					cpc))
		      | (Ty.C_double, FARG a) =>
			(case a of
			     T.FLOAD (_, addr, region) =>
			     dwordmemarg (addr, region, [])
			   | _ => dwordarg (fn addr =>
					       T.FSTORE (64, addr, a, stack)))
		      | (Ty.C_long_double, FARG a) => let
			    (* Copy 128-bit floating point value (16 bytes)
			     * into scratch space (aligned at 8-byte boundary).
			     * The address of the scratch copy is then
			     * passed as a regular 32-bit argument. *)
			    val ss' = roundup (ss, 8)
			    val ssaddr = addli (spreg, ss')
			in
			    wordarg (ssaddr,
				     T.FSTORE (128, ssaddr, a, stack) :: cpc,
				     ss' + 16)
			end
		      | (t as Ty.C_STRUCT _, a) => let
			    (* copy entire struct into scratch space
			     * (aligned according to struct's alignment
			     * requirements).  The address of the scratch
			     * copy is then passed as a regular 32-bit
			     * argument. *)
			    val (sz, al) = szal t
			    val ss' = roundup (ss, al)
			    val ssaddr = addli (spreg, ss')
			    val cpc' = struct_copy (sz, al, a, t, ss', cpc)
			in
			    wordarg (ssaddr, cpc', ss' + sz)
			end
		      | _ => error "argument/type mismatch"
		end
	      | loop _ = error "wrong number of arguments"
	in
	    loop (paramTys, args, 0, scratchstart, [], [])
	end

	val (defs, uses) = let
	    val gp = T.GPR o reg32
	    val fp = T.FPR o freg64
	    val g_regs = map (gp o greg) [1, 2, 3, 4, 5, 6, 7]
	    val a_regs = map (gp o oreg) [0, 1, 2, 3, 4, 5]
	    val l_reg = gp (oreg 7)
	    val f_regs = map (fp o freg)
			     [0, 2, 4, 6, 8, 10, 12, 14,
			      16, 18, 20, 22, 24, 26, 28, 30]
	    (* a call instruction defines all caller-save registers:
	     *   - %g1 - %g7
	     *   - %o0 - %o5 (argument registers)
	     *   - %o7       (link register)
	     *   - all fp registers *)
	    
	    val defs = g_regs @ a_regs @ l_reg :: f_regs
	    (* A call instruction "uses" just the argument registers. *)
	    val uses = List.take (a_regs, regargwords)
	in
	    (defs, uses)
	end

	val result =
	    case retTy of
		Ty.C_float => [T.FPR (T.FREG (32, FP 0))]
	      | Ty.C_double => [T.FPR (T.FREG (64, FP 0))] (* %f0/%f1 *)
	      | Ty.C_long_double => []
	      | Ty.C_STRUCT _ => []
	      | Ty.C_ARRAY _ => error "array return type"
	      | (Ty.C_PTR | Ty.C_void |
		 Ty.C_signed (Ty.I_int | Ty.I_long) |
		 Ty.C_unsigned (Ty.I_int | Ty.I_long)) =>
		[T.GPR (T.REG (32, oreg 0))]
	      | (Ty.C_signed Ty.I_char | Ty.C_unsigned Ty.I_char) =>
		[T.GPR (T.REG (8, oreg 0))]
	      | (Ty.C_signed Ty.I_short | Ty.C_unsigned Ty.I_short) =>
		[T.GPR (T.REG (16, oreg 0))]
	      | (Ty.C_signed Ty.I_long_long | Ty.C_unsigned Ty.I_long_long) =>
		[T.GPR (T.REG (64, oreg 0))]

	val { save, restore } = saveRestoreDedicated defs

	val (sretsetup, srethandshake) =
	    case res_szal of
		NONE => ([], [])
	      | SOME (sz, al) => let
		    val addr = structRet { szb = sz, align = al }
		in
		    ([T.STORE (32, addli (spreg, 64), addr, stack)],
		     [T.EXT (ix (IX.UNIMP sz))])
		end

	val call = T.CALL { funct = name, targets = [],
			    defs = defs, uses = uses,
			    region = mem, pops = 0 }

	val call =
	    case callComment of
		NONE => call
	      | SOME c =>
		T.ANNOTATION (call, #create MLRiscAnnotations.COMMENT c)

	val (sp_sub, sp_add) =
	    if stackdelta = 0 then ([], []) else
	    if paramAlloc { szb = stackdelta, align = 4 } then ([], [])
	    else ([T.MV (32, sp, T.SUB (32, spreg, LI stackdelta))],
		  [T.MV (32, sp, addli (spreg, stackdelta))])

	val callseq =
	    List.concat [sp_sub,
			 copycode,
			 argsetupcode,
			 sretsetup,
			 save,
			 [call],
			 srethandshake,
			 restore,
			 sp_add]
			 
    in
	{ callseq = callseq, result = result }
    end
end
