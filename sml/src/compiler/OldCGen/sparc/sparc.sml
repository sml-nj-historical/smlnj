(* sparc.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 *)

functor SparcCM (
    structure C : CODER
      where type 'a instruction = 'a SparcInstr.instruction
        and type 'a sdi = 'a SparcInstr.sdi) : CMACHINE =
  struct

    structure P = CPS.P
    structure D = SparcSpec.ObjDesc

    fun bug s = ErrorMsg.impossible ("sparc/sparc.sml: " ^ s)

    val lwtoi = LargeWord.toInt		(* for converting descriptors *)

    val itow = Word.fromInt
    val wtoi = Word.toIntX

    fun low10 n = wtoi(Word.andb(itow n, 0w1023))
    fun high22 n = wtoi(Word.~>>(itow n, 0w10))
    fun andConst(x, w) = wtoi (Word.andb(itow x, w))

  (* Architecture depedent features *)
    val wordSzB = 4

    structure C' : sig
	eqtype label
	val mark : unit -> unit
	val comment : string -> unit
	exception BadReal of string
    end = C
    open C'

    structure S' : sig
	datatype register = REG of int
	datatype fregister = FREG of int
	datatype 'label labelexp
	  = LABELexp of {           (* An offset relative to a label.  The value of a *)
	      base : 'label,        (* label expression is ((dst - base) + offset). *)
	      dst : 'label,
	      offset : int
	    }
	datatype 'label operand
	  = REGrand of register		(* A register value *)
	  | IMrand of int		(* A small integer constant (13 bits) *)
	  | LABrand of 'label labelexp	(* A small valued label expression (13 bits) *)
	  | HIrand of 'label labelexp	(* The high 22 bits of a label expression *)
	  | LOrand of 'label labelexp	(* The low 10 bits of a label expression *)
	datatype cond_code
	  = CC_A | CC_E | CC_NE | CC_G | CC_GE | CC_L | CC_LE | CC_GEU | CC_LEU | CC_GU | CC_LU
	datatype fcond_code 
	  = FCC_A | FCC_U (* unordered *) | FCC_G | FCC_UG | FCC_L | FCC_UL
	  | FCC_LG | FCC_NE | FCC_E | FCC_UE | FCC_GE | FCC_UGE | FCC_LE
	  | FCC_ULE | FCC_O 

      end = SparcInstr
    open S'

  (* Offsets to important stack locations.
   * NOTE: the following must track SPARC.prim.asm in the runtime
   *)
    val mulAddrOffset = IMrand 72
    val divAddrOffset = IMrand 76
    val umulAddrOffset = IMrand 80
    val udivAddrOffset = IMrand 84
    val cvti2dAddrOffset = IMrand 88
    val startGCOffset = IMrand SparcSpec.startgcOffset

    val zeroR = REG 0                       (* %g0 *)
    val zeroRand = REGrand zeroR

    local

      fun emit_ld args = C.emit (SparcInstr.I_ld args)
      fun emit_ldb args = C.emit (SparcInstr.I_ldb args)
      fun emit_ldf args = C.emit (SparcInstr.I_ldf args)
      fun emit_st args = C.emit (SparcInstr.I_st args)
      fun emit_stb args = C.emit (SparcInstr.I_stb args)
      fun emit_stf args = C.emit (SparcInstr.I_stf args)
      fun emit_sethi args = C.emit (SparcInstr.I_sethi args)
      fun emit_bcc args = C.emit (SparcInstr.I_bcc args)
      fun emit_fbcc args = C.emit (SparcInstr.I_fbcc args)
      fun emit_jmpl args = C.emit (SparcInstr.I_jmpl args)
      fun emit_jmp (r, offset) = C.emit (SparcInstr.I_jmpl(r, offset, zeroR))
      fun emit_add args = C.emit (SparcInstr.I_add args)
      fun emit_addcc args = C.emit (SparcInstr.I_addcc args)
      fun emit_taddcctv args = C.emit (SparcInstr.I_taddcctv args)
      fun emit_sub args = C.emit (SparcInstr.I_sub args)
      fun emit_subcc args = C.emit (SparcInstr.I_subcc args)
      fun emit_sra args = C.emit (SparcInstr.I_sra args)
      fun emit_srl args = C.emit (SparcInstr.I_srl args)
      fun emit_sll args = C.emit (SparcInstr.I_sll args)
      fun emit_and args = C.emit (SparcInstr.I_and args)
      fun emit_andcc args = C.emit (SparcInstr.I_andcc args)
      fun emit_or args = C.emit (SparcInstr.I_or args)
      fun emit_xor args = C.emit (SparcInstr.I_xor args)
      fun emit_not args = C.emit (SparcInstr.I_not args)
      fun emit_tvs () = C.emit SparcInstr.I_tvs
      fun emit_fadd args = C.emit (SparcInstr.I_fadd args)
      fun emit_fsub args = C.emit (SparcInstr.I_fsub args)
      fun emit_fmul args = C.emit (SparcInstr.I_fmul args)
      fun emit_fdiv args = C.emit (SparcInstr.I_fdiv args)
      fun emit_fneg args = C.emit (SparcInstr.I_fneg args)
      fun emit_fabs args = C.emit (SparcInstr.I_fabs args)
      fun emit_fcmp args = C.emit (SparcInstr.I_fcmp args)
      fun emit_fmov args = C.emit (SparcInstr.I_fmov args)
      fun emit_fitod args = C.emit (SparcInstr.I_fitod args)

      local
	fun mkLabExp (lab, n) = LABELexp{base= C.baseLab, dst= lab, offset= (n-4096)}
      in

      fun setBaseAddr (lab, reg) = 
	       C.emitSDI (
	        SparcInstr.SetBaseAddr(mkLabExp(lab, 0), reg))

      fun loadAddr (lab, n, dst) = (
	    C.emitSDI (SparcInstr.LoadAddr(mkLabExp(lab, n), dst)))

      fun load (lab, n, dst, tmpR) = (
	    C.emitSDI (SparcInstr.Load(mkLabExp(lab, n), dst, tmpR)))

      fun loadF (lab, n, dst, tmpR) = (
	    C.emitSDI (SparcInstr.LoadF(mkLabExp(lab, n), dst, tmpR)))

      end (* local *)

    in

    datatype EA
      = Immed of int
      | Word32 of Word32.word
      | ImmedLab of label
      | Direct of register
      | FDirect of fregister

    datatype condition = EQL | NEQ | GTR | GEQ | LSS | LEQ 
		       | GEU | GTU | LTU | LEU

    val immed = Immed
    val immed32 = Word32


  (** Dedicated registers **)
    val exnptr = Direct(REG 7)              (* %g7 *)
    val arithtemps = []
    val varptr = Direct(REG 29)             (* %i5 *)
    val varptr_indexable = true
    val standardclosure = Direct(REG 26)    (* %i2 *)
    val standardarg = Direct(REG 24)        (* %i0 *)
    val standardcont = Direct(REG 25)       (* %i1 *)
    val standardlink = Direct(REG 1)        (* %g1 *)
    val miscregs = map (Direct o REG) [     (* %g2-%g3, %o0-%o1, %l0-%l7, %i4 *)
	  2, 3, 8, 9, 16, 17, 18, 19, 20, 21, 22, 23, 28
	  ]
  (* Note: cc treats none of the floating point registers as callee save. *)
    val savedfpregs = [] : EA list
    val floatregs = let fun from(n,m) = if n > m then [] else n :: from(n+2,m)
		    in map (FDirect o FREG) (from(0,31))
		    end
    val dataptrR = REG 6                    (* %g6 *)
    val storeptrR = REG 5		    (* %g5 *)
    val limitptrR = REG 4                   (* %g4 *)
    val limitptrRand = REGrand limitptrR
  (* the following two registers are used for calling ml_mul & ml_div *)
    val spR = REG 14                        (* %sp (%o6) *)
    val linkR = REG 15                      (* %o7, link register *)
    val maskR = REG 13       (* %o5, also used as temporary *)
    val checkR = REG 12      (* %o4, also used as temporary *)

  (** Temporary registers **
   * We use registers %o2-%o5 as temporaries.  They are used in a round-robin
   * order to facilitate instruction scheduling.
   *)
    local
      val rear = ref 0 and queue = ref 0w0
      fun ins i = let
	    val r = !rear
	    in
	      queue := Word.orb(Word.<<(itow i, itow r), !queue);
	      rear := r + 5
	    end
      fun remove () = let
	    val q = !queue
	    val x = wtoi (Word.andb (q, 0w31))
	    in
	      queue := Word.>> (q, 0w5);
	      rear := !rear - 5;
	      x
	    end
      val _ = app ins [10, 11, 12, 13]      (* %o2-%o5 *)
    in

  (* Registers %o2, %o3 & %o4 are also used to call ml_mul and ml_div. *)
    val arg1EA = Direct(REG 10) and arg2EA = Direct(REG 11)
    val opAddrR = REG 12

  (* Get a temporary register. *)
    fun getTmpReg () = REG(remove())

   (* should be cleaned up in the future *)
    val tmpfpreg = (FREG 30)

   (* If r is a temporary register, then free it. *)
    fun freeReg (REG r) = if ((9 < r) andalso (r < 14)) then (ins r) else ()

  (* Free a temporary register. *)
    fun freeTmpReg (REG r) = ins r

    end (* local *)


  (* align is a nop, since strings are automatically padded. *)
    fun align () = ()

    val emitlong = C.emitLong
    val realconst = C.emitReal
    val emitstring = C.emitString

    fun emitlab (n, ImmedLab lab) = C.emitLabel (lab, n)
      | emitlab _ = bug "[SparcCM.emitlab]"

    val newlabel = ImmedLab o C.newLabel
    fun define (ImmedLab lab) = C.define lab
      | define _ = bug "[SparcCM.define]"

    datatype immed_size = Immed13 | Immed32

    fun sizeImmed n = if (~4096 <= n) andalso (n < 4096) then Immed13 else Immed32


  (** Utility operations **)

    fun emitMove (src, dst) = emit_or (zeroR, REGrand src, dst)

    fun loadImmed32 (n, r) = let
	  val lo10 = low10 n
	  in
	    emit_sethi (IMrand(high22 n), r);
	    if (lo10 <> 0) then emit_or(r, IMrand lo10, r) else ()
	  end

    fun loadImmed (n, r) = (
	  case (sizeImmed n)
	   of Immed13 => emit_or(zeroR, IMrand n, r)
	    | Immed32 => loadImmed32 (n, r))

    local
      structure W = Word32
    in
    fun loadWord32(w, rd) = let
	val lo10 = W.toIntX (W.andb (w, 0w1023))
	val hi22 = W.toIntX (W.~>> (w, 0w10))
        in
	  emit_sethi(IMrand hi22, rd);
	  if lo10 = 0 then () else emit_or(rd, IMrand lo10, rd)
        end
    end

    fun op32 f (r1, n, r2) = let val tmpR = getTmpReg()
	  in
	    loadImmed32 (n, tmpR);
	    f (r1, REGrand tmpR, r2);
	    freeTmpReg tmpR
	  end

    fun loadReg(r, offset, dst) = (
	  case (sizeImmed offset)
	   of Immed13 => emit_ld (r, IMrand offset, dst)
	    | Immed32 => (op32 emit_ld) (r, offset, dst))

    fun store (src, r, offset) = (
	  case (sizeImmed offset)
	   of Immed13 => emit_st (r, IMrand offset, src)
	    | Immed32 => (op32 emit_st) (r, offset, src))


    fun loadFReg(r, offset, dst) = (
	  case (sizeImmed offset)
	   of Immed13 => emit_ldf (r, IMrand offset, dst)
	    | Immed32 => (op32 emit_ldf) (r, offset, dst))

    fun storeFReg (src, r, offset) = (
	  case (sizeImmed offset)
	   of Immed13 => emit_stf (r, IMrand offset, src)
	    | Immed32 => (op32 emit_stf) (r, offset, src))


    fun addImmed (r, n, dst) = (
	  case (sizeImmed n)
	   of Immed13 => emit_add (r, IMrand n, dst)
	    | Immed32 => (op32 emit_add) (r, n, dst))

    fun compareImmed (r, n) = (
	  case (sizeImmed n)
	   of Immed13 => emit_subcc (r, IMrand n, zeroR)
	    | Immed32 => (op32 emit_subcc) (r, n, zeroR))

    fun sparcCC EQL = CC_E   | sparcCC NEQ = CC_NE
      | sparcCC GTR = CC_G   | sparcCC GEQ = CC_GE
      | sparcCC LSS = CC_L   | sparcCC LEQ = CC_LE
      | sparcCC GEU = CC_GEU | sparcCC GTU = CC_GU
      | sparcCC LEU = CC_LEU | sparcCC LTU = CC_LU


  (** CMachine instructions **)

  (* move (src, dst) *)
    fun move (Immed n, Direct r) = loadImmed (n, r)
      | move (Word32 w, Direct dst) = loadWord32(w, dst)
      | move (ImmedLab lab, Direct r) = loadAddr (lab, 0, r)
      | move (FDirect (FREG fs), FDirect (FREG fd)) = let
	  fun even x = (andConst(x, 0wx1) = 0)
	  in
	    if (even fs andalso even fd)
	      then (
		emit_fmov(FREG fs, FREG fd);
		emit_fmov(FREG (fs+1), FREG (fd+1)))
	      else bug "[SparcCM.move: bad floating point registers]"
	end
      | move (Direct r1, Direct r2) = if (r1 = r2) then () else emitMove (r1, r2)
      | move _ = bug "[SparcCM.move]"


  (* spR is the stack pointer; pregs_offset is the initial stack offset
   * for pseudo registers, it should be consistent with the offset 
   * in the SPARC.prim.asm file.
   *)  
  val pregs_offset = 104 

  fun loadpseudo (Direct x,Immed i) = 
        emit_ld(spR, IMrand(2*(i-1)+pregs_offset), x)
        (* print ("loadpseudo ****  "^(Int.toString(i))^"  \n") *)
    | loadpseudo (Direct x,Direct y) =    (* this case is never used *)
        let val tmpR = getTmpReg()
         in emit_sll(y, IMrand 1, tmpR);
            emit_add(spR, REGrand tmpR, tmpR);
            emit_ld(tmpR, IMrand (pregs_offset-2), x);
            freeTmpReg tmpR
        end
    | loadpseudo _ = bug "[loadpseudo]"

  fun storepseudo(Direct x,Immed i) = 
        emit_st(spR, IMrand(2*(i-1)+pregs_offset), x)
        (* print ("storepseudo ****  "^(Int.toString(i))^"  \n"); *)
    | storepseudo(Direct x,Direct y) =    (* this case is never used *)
        let val tmpR = getTmpReg()
         in emit_sll(y, IMrand 1, tmpR);
            emit_add(spR, REGrand tmpR, tmpR);
            emit_st(tmpR, IMrand (pregs_offset-2), x);
            freeTmpReg tmpR
        end
    | storepseudo _ = bug "[storepseudo]"

(*  fun loadpseudo (x, i) = () 
    fun storepseudo (x, i) = () 
*)

   fun testLimit () = emit_subcc (dataptrR, limitptrRand, zeroR)

   fun decLimit n = (* for polling *)
       if (n < 2048) then
         emit_sub (limitptrR, IMrand n, limitptrR) 
       else (emit_sethi (IMrand(high22 n), checkR);
           emit_or (checkR, IMrand(low10 n), checkR);
           emit_sub (limitptrR, REGrand checkR, limitptrR))

  (* checkLimit (n):
   * Generate code to check the heap limit to see if there is enough free space
   * to allocate n bytes.
   * NOTE: THIS CODE USES TEMP REGS BY ALIASES.
   *   Thus it is important that none of the emitted pseudo-instructions
   *   below uses getTmpReg(), directly or indirectly.
   *)
    fun checkLimit (max_allocation, restart, mask, rlab, fregs) = 
      let val lab' = C.newLabel()
	  val n = max_allocation - 4096
          val _ = if (n > 0) then 
                    if (n < 2048)
		    then (emit_add (dataptrR,IMrand n,checkR);
                          emit_subcc (checkR, limitptrRand, zeroR))
		    else (emit_sethi (IMrand(high22 n), checkR);
                          emit_or (checkR, IMrand(low10 n), checkR);
                          emit_add (dataptrR, REGrand checkR, checkR);
                          emit_subcc (checkR, limitptrRand, zeroR))
	          else ()
          val _ = emit_bcc (CC_LU, lab');
       in (case fregs 
            of [] => (emit_ld (spR, startGCOffset, checkR);
                      move (mask, Direct maskR);
	              move (restart, Direct linkR);
	              emit_jmp (checkR, zeroRand))
             | _ => (let val k = length fregs
                         val desc = lwtoi(D.makeDesc(k * 8, D.tag_string))
                         val retlab = C.newLabel()

                         (* cps/limit.sml makes sure that there is enough
                            space left to save these floating 
                            point registers *)
                         fun deposit([], _) = ()
                           | deposit((FDirect(FREG fpr))::r, i) = 
                               (storeFReg(FREG fpr, dataptrR, i);
                                storeFReg(FREG(fpr + 1), dataptrR, i+4);
		                deposit(r,i+8))

                         fun restore(s, [], _) = ()
                           | restore(s, (FDirect(FREG fpr))::r, i) = 
                               (loadFReg(s, (i), FREG fpr);
	                        loadFReg(s, (i+4), FREG(fpr+1));
                                restore(s, r, i+8))

                      in deposit(fregs, 4);
                         move(immed desc, Direct checkR);
                         (* emit_or(dataptrR, IMrand 4, dataptrR); *)(*align*)
                         store(checkR, dataptrR, 0);
                         addImmed(dataptrR, 4, maskR);
           	         addImmed (dataptrR, k*8+4, dataptrR);
                         emit_st(spR, IMrand(4+pregs_offset), maskR);
                         (* I am using pseudo register #2 here !!! *)

                         emit_ld (spR, startGCOffset, checkR);
                         move (mask, Direct maskR);
	                 move (ImmedLab retlab, Direct linkR);
	                 emit_jmp (checkR, zeroRand);
                         
                         C.define retlab;
                         emit_ld(spR, IMrand(4+pregs_offset), maskR);
                         (* I am using pseudo register #2 here !!! *)
                         move (rlab, Direct linkR);
                         restore(maskR, fregs, 0);
			 testLimit();
                         emit_jmp (linkR, zeroRand)
                     end));
          C.define lab'
      end

  (* beginStdFn ():
   * Note the beginning of a standard function.  This requires generating 
   * code to load the base code block address into baseCodePtr.
   *)
    fun beginStdFn(ImmedLab lab, Direct reg) = setBaseAddr(lab,reg)

  (* jmp (dst):
   * Unconditional jump to destination.
   *)
    fun jmp (ImmedLab lab) = emit_bcc (CC_A, lab)
      | jmp (Direct r) = emit_jmp (r, zeroRand)
      | jmp _ = bug "[SparcCM.jmp]"

  (* record (vl, dst):
   * makes a new record, puts address of it into the destination specified
   * by the second arg. The contents are numbered from ~1 and up.
   *)
    fun record (vl : (EA * CPS.accesspath) list, Direct dst) = let
	  val minBlockSize = 6
	(* generate code to move one or more adjacent fields from one record into
	 * adjacent fields in the new record.  If the block is big enough, then
	 * use a block copy loop.
	 *)
	  fun blockMove (srcR, startindx, path, offset) = let
	      (* check a CPS path to see how large the block is *)
		fun chkpath (cnt, i,
		    path as (Direct r, CPS.SELp(j, CPS.OFFp 0)) :: rest) =
		      if (r = srcR) andalso (i+offset = j)
			then chkpath (cnt+1, i+1, rest)
			else (cnt, path)
		  | chkpath (cnt, _, rest) = (cnt, rest)
	      (* generate code to move fields individually *)
		fun moveFields (0, _, _) = ()
		  | moveFields (n, srcOffset, dstOffset) = let val tmpR = getTmpReg()
		      in
			loadReg(srcR, srcOffset, tmpR);
			store (tmpR, dataptrR, dstOffset);
			freeTmpReg tmpR;
			moveFields(n-1, srcOffset+wordSzB, dstOffset+wordSzB)
		      end
		val (blksz, rest) = chkpath(1, startindx+1, path)
		in
		  if (blksz < minBlockSize)
		    then moveFields(blksz, (startindx+offset)*wordSzB, startindx*wordSzB)
		    else if (offset = 0)
		      then let
			val lab = C.newLabel()
			val indxR = getTmpReg() and tmpR = getTmpReg()
			in
			  loadImmed (startindx*wordSzB, indxR);
			  C.define lab;
			  emit_ld (srcR, REGrand indxR, tmpR);
			  compareImmed (indxR, (startindx+blksz)*wordSzB);
			  emit_st (dataptrR, REGrand indxR, tmpR);
			  emit_add (indxR, IMrand wordSzB, indxR);
			  emit_bcc (CC_L, lab);
			  freeTmpReg indxR; freeTmpReg tmpR
			end
		      else let
			val lab = C.newLabel()
			val indxR1 = getTmpReg() and indxR2 = getTmpReg()
			val tmpR = getTmpReg()
			in
			  loadImmed ((startindx+offset)*wordSzB, indxR1);
			  loadImmed (startindx*wordSzB, indxR2);
			  C.define lab;
			  emit_ld (srcR, REGrand indxR1, tmpR);
			  emit_add (indxR1, IMrand wordSzB, indxR1);
			  emit_st (dataptrR, REGrand indxR2, tmpR);
			  emit_add (indxR2, IMrand wordSzB, indxR2);
			  compareImmed (indxR1, (startindx+offset+blksz)*wordSzB);
			  emit_bcc (CC_L, lab);
			  freeTmpReg indxR1; freeTmpReg indxR2; freeTmpReg tmpR
			end;
		  freeReg srcR;
		  (wordSzB*(startindx+blksz), rest)
		end (* blockMove *)
      (* For each field in the record generate the necessary moves to initialize
       * it in the new record.  Fields are initialized in ascending order.
       *)
	  fun fields (dstOffset, nil) = dstOffset
	    | fields (dstOffset, (Direct r, CPS.SELp(j, CPS.OFFp 0)) :: rest) = let
		val indx = dstOffset div wordSzB
		in
		  fields (blockMove (r, indx, rest, j-indx))
		end
	    | fields (dstOffset, (Direct r, CPS.SELp(j, p)) :: rest) = let
		val tmpR = getTmpReg()
		in
		  loadReg(r, j*wordSzB, tmpR);
		  freeReg r;
		  fields (dstOffset, (Direct tmpR, p) :: rest)
		end
	    | fields (dstOffset, (Direct r, CPS.OFFp 0) :: rest) = (
		store (r, dataptrR, dstOffset);
		freeReg r;
		fields (dstOffset+wordSzB, rest))

	    | fields (dstOffset, (Direct r, CPS.OFFp j) :: rest) = 
                bug "unexpected non-zero OFFp fields"
(*
	    | fields (dstOffset, (Direct r, CPS.OFFp j) :: rest) = let
		val tmpR = getTmpReg()
		val offset = j*wordSzB
		in
		  case (sizeImmed offset)
		   of Immed13 => emit_add (r, IMrand offset, tmpR)
		    | Immed32 => (
			loadImmed32 (offset, tmpR);
			emit_add (r, REGrand tmpR, tmpR))
		  (* end case *);
		  store (tmpR, dataptrR, dstOffset);
		  freeTmpReg tmpR; freeReg r;
		  fields (dstOffset+wordSzB, rest)
		end
*)
	    | fields (dstOffset, (x, p) :: rest) =  let
		val tmpR = getTmpReg()
		in
		  move (x, Direct tmpR);
		  fields (dstOffset, (Direct tmpR, p) :: rest)
		end
	  val szB = fields (0, vl)
	  in
	    addImmed (dataptrR, wordSzB, dst);
	    addImmed (dataptrR, szB, dataptrR)
	end
      | record _ = bug "[SparcCM.record]"

  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(y-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
    fun recordStore (x, y, alwaysBoxed) = let
	  fun storeListUpdate r = (
		  emit_st (dataptrR, zeroRand, r);
		  emit_st (dataptrR, IMrand 4, storeptrR);
		  emitMove (dataptrR, storeptrR);
		  addImmed (dataptrR, 8, dataptrR))
	  in
	    case (x, y)
	     of (Direct r, Immed 1) => storeListUpdate r
	      | (Direct r, Immed i) => let val tmpR = getTmpReg()
		  in
		    addImmed (r, 2*(i-1), tmpR);
		    storeListUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | (Direct r1, Direct r2) => let val tmpR = getTmpReg()
		  in
		    emit_sub (r2, IMrand 1, tmpR);
		    emit_add (tmpR, REGrand tmpR, tmpR);
		    emit_add (r1, REGrand tmpR, tmpR);
		    storeListUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | _ => bug "[SparcCM.recordStore]"
	    (* end case *)
	  end (* recordStore *)
(*** STOREPTR
    fun recordStore (x, y, alwaysBoxed) = let
	(* NOTE: eventually we can optimize the case where alwaysBoxed = false *)
	  fun storeVectorUpdate r = (
		  emit_st (limitptrR, IMrand 4092, r);
		  emit_sub (limitptrR, IMrand 4, limitptrR))
	  in
	    case (x, y)
	     of (Direct r, Immed 1) => storeVectorUpdate r
	      | (Direct r, Immed i) => let val tmpR = getTmpReg()
		  in
		    addImmed (r, 2*(i-1), tmpR);
		    storeVectorUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | (Direct r1, Direct r2) => let val tmpR = getTmpReg()
		  in
		    emit_sub (r2, IMrand 1, tmpR);
		    emit_add (tmpR, REGrand tmpR, tmpR);
		    emit_add (r1, REGrand tmpR, tmpR);
		    storeVectorUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | _ => bug "[SparcCM.recordStore]"
	    (* end case *)
	  end (* recordStore *)
***)
(*** STOREPTR
    fun recordStore (x, y, _) = record ([
	    (Immed(lwtoi(D.makeDesc(3, D.tag_record))), CPS.OFFp 0),
	    (x, CPS.OFFp 0), (y, CPS.OFFp 0), (storeptr, CPS.OFFp 0)
	  ], storeptr)
***)



  fun recordcont _ = bug "[SparcCM.recordcont not implemented yet]"

  (* select (i, x, y):  y <- mem[x + 4*i] *)
  fun select (i, Direct r, Direct dst) = loadReg(r, i*4, dst)
    | select (i, ImmedLab lab, Direct dst) = 
        let val tmpR = getTmpReg()
	 in load (lab, i*4, dst, tmpR);
	    freeTmpReg tmpR
        end
    | select _ = bug "[SparcCM.select]"

  (* offset (i, x, y):  y <- (x + 4*i) *)
  fun offset (i, Direct r, Direct dst) = addImmed (r, 4*i, dst)
    | offset (i, ImmedLab lab, Direct dst) = loadAddr (lab, i, dst)
    | offset _ = bug "[SparcCM.offset]"

    local
      fun moveByte movFn = let
	    fun mov (Direct r, Direct base, Direct indx) = movFn(base, REGrand indx, r)
	      | mov (Direct r, Direct base, Immed indx) = (
		  case (sizeImmed indx)
		   of Immed13 => movFn (base, IMrand indx, r)
		    | Immed32 => (op32 movFn) (base, indx, r))
	      | mov _ = bug "[SparcCM.moveByte]"
	    in
	      mov
	    end
      val loadByte = moveByte emit_ldb
      val storeByte = moveByte emit_stb
    in

  (* fetchindexb (x, y, z) fetches an unsigned byte:  y <- mem[x+z] *)
    fun fetchindexb (base, dst, indx) = loadByte(dst, base, indx)

  (* storeindexb (x, y, z) stores a byte:  mem[y+z] <- x *)
    fun storeindexb (Immed i, base, indx) = let
	  val tmpR = getTmpReg()
	  in
	    loadImmed (i, tmpR);
	    storeByte (Direct tmpR, base, indx);
	    freeTmpReg tmpR
	  end
      | storeindexb arg = storeByte arg
    end (* local *)

  (* jmpindexb (x):  pc <- (x+y) *)
    fun jmpindexb(ImmedLab lab, Direct y) = let
	  val tmpR1 = getTmpReg()
	  in
	    loadAddr (lab, 0, tmpR1);
	    emit_jmp (tmpR1, REGrand y);
	    freeTmpReg tmpR1
	  end
      | jmpindexb _ = bug "[SparcCM.jmpindexb]"

  (* fetchindexl (x, y, z) fetches a word:  y <- mem[x+2*(z-1)] *)
    fun fetchindexl (Direct r1, Direct dst, Direct r2) = let
	  val tmpR = getTmpReg()
	  in
	    emit_sub (r2, IMrand 1, tmpR);
	    emit_add (tmpR, REGrand tmpR, tmpR);
	    emit_ld (r1, REGrand tmpR, dst);
	    freeTmpReg tmpR
	  end
      | fetchindexl (Direct r1, Direct dst, Immed i) = loadReg(r1, 2*(i-1), dst)
      | fetchindexl (ImmedLab lab, Direct dst, Direct r) =  let
	  val tmpR1 = getTmpReg()
	  in
	    loadAddr (lab, ~2, tmpR1);
	    emit_add (r, REGrand tmpR1, tmpR1);
	    emit_ld (r, REGrand tmpR1, dst);
	    freeTmpReg tmpR1
	  end
      | fetchindexl _ = bug "[SparcCM.fetchindexl]"

  (*storeindexl (x, y, z) stores a word:  mem[y+2*(z-1)] <- x *)
    fun storeindexl (Direct src, Direct r1, Direct r2) = let val tmpR = getTmpReg()
	  in
	    emit_sub (r2, IMrand 1, tmpR);
	    emit_add (tmpR, REGrand tmpR, tmpR);
	    emit_st (r1, REGrand tmpR, src);
	    freeTmpReg tmpR
	  end
      | storeindexl (Direct src, Direct r, Immed i) = store (src, r, 2*(i-1))
      | storeindexl (Immed n, x, y) = let val tmpR = getTmpReg()
	  in
	    loadImmed (n, tmpR);
	    storeindexl (Direct tmpR, x, y);
	    freeTmpReg tmpR
	  end
      | storeindexl (ImmedLab lab, x, y) = let
	  val tmpR1 = getTmpReg()
	  in
	    loadAddr (lab, 0, tmpR1);
	    storeindexl (Direct tmpR1, x, y);
	    freeTmpReg tmpR1
	  end
      | storeindexl _ = bug "[SparcCM.storeindexl]"


   (* fetchindexd(x,y,z): y <- mem[x+4*(z-1)] *)
    fun fetchindexd(Direct x, FDirect(FREG fp), Direct z) = let
	  val tmpR = getTmpReg()
	  in
	    emit_sll (z, IMrand 2, tmpR);
	    emit_add (tmpR, REGrand x, tmpR);
	    emit_ldf (tmpR, IMrand ~4, FREG fp);
	    emit_ldf (tmpR, zeroRand, FREG(fp+1));
	    freeTmpReg tmpR
	  end
      | fetchindexd(Direct x, FDirect(FREG fp), Immed i) = let
	  val offset = 4*(i-1)
	  in
	    case sizeImmed (offset+4) 
	     of Immed13 => (
		  emit_ldf(x, IMrand offset, FREG fp);
		  emit_ldf(x, IMrand(offset+4), FREG(fp+1)))
	      | Immed32 => let val tmpR = getTmpReg()
		  in
		    loadImmed(offset,tmpR);
		    emit_add(x,REGrand tmpR,tmpR);
		    emit_ldf(tmpR,zeroRand,FREG fp);
		    emit_ldf(tmpR,IMrand 4,FREG(fp+1));
		    freeTmpReg tmpR
		  end
          end
      | fetchindexd _ = bug "[SparcCM.fetchindexd]"

  (* storeindexd: mem[y+4*(z-1)] <- x *)
    fun storeindexd (FDirect(FREG fp), Direct y, Direct z) = let
	  val tmpR = getTmpReg()
	  in
	    emit_sll (z, IMrand 2, tmpR);
	    emit_add (tmpR, REGrand y, tmpR);
	    emit_stf (tmpR, IMrand ~4, FREG fp);
	    emit_stf (tmpR ,zeroRand, FREG (fp+1));
	    freeTmpReg tmpR
	  end
      | storeindexd (FDirect(FREG fp), Direct y, Immed i) = let
	  val offset = 4*(i-1)
	  in
	    case (sizeImmed (offset+4))
	     of Immed13 => (
		  emit_stf (y, IMrand offset, FREG fp);
		  emit_stf (y, IMrand (offset+4), FREG(fp+1)))
	      | Immed32 => let
		  val tmpR = getTmpReg()
		  in
		    loadImmed(offset, tmpR);
		    emit_add(y, REGrand tmpR, tmpR);
		    emit_stf(tmpR, zeroRand, FREG fp);
		    emit_stf(tmpR, IMrand 4, FREG(fp+1));
		    freeTmpReg tmpR
		  end
	  end
      | storeindexd _ = bug "[SparcCM.storeindexd]"

  (* ashl (n, x, y) shift left: y <- (x << n), with  n >= 0 *)
    fun ashl (Direct cntR, Direct src, Direct dst) =
	  emit_sll(src, REGrand cntR, dst)
      | ashl (Immed cnt, Direct src, Direct dst) =
	  emit_sll (src, IMrand(andConst(cnt, 0w31)), dst)
      | ashl (Direct cntR, Immed src, Direct dst) = let val tmpR = getTmpReg()
	  in
	    loadImmed (src, tmpR);
	    emit_sll (tmpR, REGrand cntR, dst);
	    freeTmpReg tmpR
	  end
      | ashl (Immed cnt, Immed src, Direct dst) = (
	  loadImmed (wtoi (Word.<<(itow src, itow cnt)), dst))
      | ashl (shamt, Word32 w, dst) = let val tmpR = getTmpReg()
	in
	  loadWord32 (w, tmpR);
	  ashl (shamt, Direct tmpR, dst);
	  freeTmpReg tmpR
	end
      | ashl _ = bug "[SparcCM.ashl]"

  (* ashr (n, x, y) shift right: y <- (x >> n), with  n >= 0 *)
    fun ashr (Direct cntR, Direct src, Direct dst) =
	  emit_sra (src, REGrand cntR, dst)
      | ashr (Immed cnt, Direct src, Direct dst) =
	  emit_sra (src, IMrand(andConst(cnt, 0w31)), dst)
      | ashr (Direct cntR, Immed src, Direct dst) = let val tmpR = getTmpReg()
	  in
	    loadImmed (src, tmpR);
	    emit_sra (tmpR, REGrand cntR, dst);
	    freeTmpReg tmpR
	  end
      | ashr (Immed cnt, Immed src, Direct dst) = (
	  loadImmed (wtoi (Word.~>>(itow src, itow cnt)), dst))
      | ashr (shamt, Word32 w, dst) = let val tmpR = getTmpReg()
	in 
	  loadWord32 (w, tmpR);
	  ashr(shamt, Direct tmpR, dst);
	  freeTmpReg tmpR
	end
      | ashr _ = bug "[SparcCM.ashr]"

    local
	fun adjArgs f (a as Immed _, b, c) = f (b, a, c)
          | adjArgs f (a as Word32 _, b, c) = f(b, a, c)
	  | adjArgs f args = f args
	fun adjSubArgs f (a, Immed 0, c) = f(Direct(zeroR), a, c)
	  | adjSubArgs f (a, Immed b, c) = let val tmpR = getTmpReg()
	      in
		loadImmed (b, tmpR);
		f (Direct tmpR, a, c);
		freeTmpReg tmpR
	      end
	  | adjSubArgs f (a, Word32 w, c) = let val tmpR = getTmpReg()
	    in
	      loadWord32(w, tmpR);
	      adjSubArgs f (a, Direct tmpR, c);
	      freeTmpReg tmpR
	    end
	  | adjSubArgs f (a, b, c) = f (b, a, c)
	fun arithOp f (Direct r1, Direct r2, Direct dst) = f (r1, REGrand r2, dst)
	  | arithOp f (Word32 w1, w2 as Word32 _, t) = let
	      val tmpR = getTmpReg()
	    in
	      loadWord32(w1, tmpR);
	      arithOp f (Direct tmpR, w2, t);
	      freeTmpReg tmpR
	    end
	  | arithOp f (Immed i1, i2 as Immed _, t) = let
	      val tmpR = getTmpReg()
	    in
	      loadImmed(i1, tmpR);
	      arithOp f (Direct tmpR, i2, t);
	      freeTmpReg tmpR
	    end
	  | arithOp f (r1, Word32 w, r2) = let 
	      val tmpR = getTmpReg()
	    in
	      loadWord32(w, tmpR);
	      arithOp f (r1, Direct tmpR, r2);
	      freeTmpReg tmpR
	    end
	  | arithOp f (Immed i1, r2, dst) = let
	      val tmpR = getTmpReg()
            in
	      loadImmed(i1, tmpR);
	      arithOp f (Direct tmpR, r2, dst);
	      freeTmpReg tmpR
            end
	  | arithOp f (Word32 w, r2, dst) = let
	      val tmpR = getTmpReg()
            in
	      loadWord32(w, tmpR);
	      arithOp f (Direct tmpR, r2, dst);
	      freeTmpReg tmpR
            end
	  | arithOp f (Direct r, Immed n, Direct dst) = (
	      case (sizeImmed n)
	       of Immed13 => f (r, IMrand n, dst)
		| Immed32 => let val tmpR = getTmpReg()
		    in
		      loadImmed32 (n, tmpR);
		      f (r, REGrand tmpR, dst);
		      freeTmpReg tmpR
		    end)
	  | arithOp _ _ = bug "[SparcCM.arithOp]"
	val addt' = adjArgs (arithOp (fn args => (emit_addcc args; emit_tvs())))
    in

    val orb = adjArgs (arithOp emit_or)
    val andb = adjArgs (arithOp emit_and)
    val xorb = adjArgs (arithOp emit_xor)
    fun notb (Direct src, Direct dst) = emit_not (src, dst)
      | notb _ = bug "[SparcCM.notb]"

    val add = adjArgs (arithOp emit_add)
    fun addt (Immed a, b as Immed _, dst) = let val tmpR = getTmpReg ()
	(* This should only occur when we need to build a constant larger than
	 * 2^29.  Note, we assume that "b" is tagged (see "cps/generic.sml").
	 *)
	  in
	    loadImmed (a, tmpR);
	    addt' (Direct tmpR, b, dst);
	    freeTmpReg tmpR
	  end
      | addt (Word32 a, b as Word32 _, dst) = let val tmpR = getTmpReg ()
	  in
	    loadWord32 (a, tmpR);
	    addt' (Direct tmpR, b, dst);
	    freeTmpReg tmpR
	  end

      | addt args = addt' args

    val sub = adjSubArgs (arithOp emit_sub)
    val subt = adjSubArgs (arithOp (fn args => (emit_subcc args; emit_tvs())))

    fun lshr (Direct cntR, Direct src, Direct dst) =
	  emit_srl (src, REGrand cntR, dst)
      | lshr (Immed cnt, Direct src, Direct dst) =
	  emit_srl (src, IMrand(andConst(cnt, 0w31)), dst)
      | lshr (Direct cntR, Immed src, Direct dst) = let val tmpR = getTmpReg()
	  in
	    loadImmed (src, tmpR);
	    emit_srl (tmpR, REGrand cntR, dst);
	    freeTmpReg tmpR
	  end
      | lshr (Immed cnt, Immed src, dst) = let val tmpR = getTmpReg()
	  in
	    loadImmed (src, tmpR);
	    lshr (Immed cnt, Direct tmpR, dst);
	    freeTmpReg tmpR
	  end
      | lshr (shamt, Word32 w, dst) = let val tmpR = getTmpReg ()
	  in
	    loadWord32 (w, tmpR);
	    lshr (shamt, Direct tmpR, dst);
	    freeTmpReg tmpR
	  end
      | lshr _ = bug "[SparcCM.lshr]"

    end (* local *)

  (* mult/divt:
   * mult (a, b):  b <- (a * b) (with overflow checking done by ml_mul)
   * divt (a, b):  b <- (b div a)
   * mulu (a, b):  b <- (a * b) (unsigned; no overflow)
   * divu (a, b):  b <- (b div a) (unsigned)
   *)
    local
      (* call an off-line arithmetic routine. *)
	fun intOp opAddrOffset (a, b as Direct _) = (
	      emit_ld (spR, opAddrOffset, opAddrR);
	      move (a, arg2EA);
	      move (b, arg1EA);
	      emit_jmpl (opAddrR, zeroRand, linkR);
	      move (arg1EA, b))
	  | intOp _ _ = bug "[SparcCM.intOp]"
    in
    val mult = intOp mulAddrOffset
    val divt = intOp divAddrOffset
    val mulu = intOp umulAddrOffset
    val divtu = intOp udivAddrOffset
    end (* local *)

  (* bbs (i, dst, lab): test the i'th bit of dst and jump to lab if it is zero *)
    fun bbs (Immed i, Direct r, ImmedLab lab) = (
	  emit_andcc (r, IMrand(wtoi (Word.<<((0w1, itow i)))), zeroR);
	  emit_bcc (CC_NE, lab))
      | bbs _ = bug "[SparcCM.bbs]"

    local
      fun revCC CC_A = CC_A
	| revCC CC_E = CC_E     | revCC CC_NE = CC_NE
	| revCC CC_L = CC_G     | revCC CC_LE = CC_GE
	| revCC CC_G = CC_L     | revCC CC_GE = CC_LE
	| revCC CC_LEU = CC_GEU | revCC CC_GEU = CC_LEU
	| revCC CC_LU  = CC_GU  | revCC CC_GU  = CC_LU

      fun compare (cc, a as Immed _, b as Direct _) = compare (revCC cc, b, a)
	| compare (cc, Direct r1, Direct r2) = (
	    emit_subcc (r1, REGrand r2, zeroR); cc)
	| compare (cc, Direct r1, Immed n) = (compareImmed (r1, n); cc)
	| compare (cc, Immed a, Immed n) = let val tmpR = getTmpReg()
	    in
	      loadImmed(a, tmpR);
	      compareImmed(tmpR, n);
	      freeTmpReg tmpR;
	      cc
	    end
	| compare(cc, Word32 w, b) = let 
            val tmpR = getTmpReg()
	  in 
	    loadWord32(w, tmpR);
	    compare(cc, Direct tmpR, b) before freeTmpReg tmpR
	  end
	| compare(cc, a, Word32 w) = let 
            val tmpR = getTmpReg()
	  in
	    loadWord32(w, tmpR);
	    compare(cc, a, Direct tmpR) before freeTmpReg tmpR
	  end
	| compare _ = bug "[SparcCM.compare]"
    in
  (* ibranch (cond, a, b, lab): if (a <cond> b) then pc <- lab *)
    fun ibranch (cond, a, b, ImmedLab lab) = 
	  emit_bcc (compare (sparcCC cond, a, b), lab)
    end (* local *)


    (*
     * Floating point arithmetic instructions
     *)
    local
    (* Fetch a ML real value into a floating-point register pair *)
      fun fetchReal (Direct r, FREG i) = (
	    emit_ldf (r, zeroRand, FREG i);
	    emit_ldf (r, IMrand 4, FREG(i+1)))
	| fetchReal (ImmedLab lab, dst) = let val tmpR = getTmpReg()
	    in
	      loadF (lab, 0, dst, tmpR);
	      freeTmpReg tmpR
	    end
	| fetchReal _ = bug "[SparcCM.fetchReal]"
      fun floatOp fOp (FDirect fpr1, FDirect fpr2, FDirect fpr3) = fOp(fpr1,fpr2,fpr3)
	| floatOp _ _ = bug "[SparcCM.floatOp]"
    in

    fun loadfloat (src, FDirect fpr) = fetchReal(src, fpr)
      | loadfloat _ = bug "[SparcCM.loadfloat]"

    fun storefloat (FDirect(FREG fpr), Direct gpr) = let val tmpR = getTmpReg()
	  in
	    loadImmed (lwtoi D.desc_reald, tmpR);
(** ALIGN **)
	    emit_st (dataptrR, zeroRand, tmpR);
	    emit_stf (dataptrR, IMrand 4, FREG fpr);
	    emit_stf (dataptrR, IMrand 8, FREG (fpr+1));
	    addImmed (dataptrR, 4, gpr);
	    emit_add (dataptrR, IMrand 12, dataptrR);
	    freeTmpReg tmpR
	  end
      | storefloat _ = bug "[SparcCM.storefloat]"

    fun fprecord (tag, vl : (EA * CPS.accesspath) list, Direct dst) = 
      let open CPS
          val len = (List.length vl) * 8 + 4
	  fun fields (_,_,_,_,nil) = ()
	    | fields (t1,t2,f1 as (FREG fpr),i,
                      (Direct r,SELp(j,OFFp 0))::rest) =
                (loadFReg(r, (j*8), FREG fpr);
	         loadFReg(r, (j*8+4), FREG(fpr+1));
		 fields(t1,t2,f1,i,(FDirect f1,OFFp 0)::rest))
	    | fields (t1,t2,f1,i,(Direct r, SELp(j, p))::rest) = 
                (loadReg(r, j*4, t1);
                 fields (t2,t1,f1,i,(Direct t1,p)::rest))
	    | fields (t1,t2,f1,i,(FDirect(FREG fpr), OFFp 0) :: rest) = 
                (storeFReg(FREG fpr, dataptrR, i);
                 storeFReg(FREG(fpr + 1), dataptrR, i+4);
		 fields (t1,t2,f1,i-8,rest))
            | fields (t1,t2,f1,i,(Direct r, OFFp _)::rest) = 
                bug "wrong-type in fprecord in sparc.sml"
	    | fields (t1,t2,f1,i,(x,p)::rest) =  
                (move (x, Direct t1);
                 fields(t2,t1,f1,i,(Direct t1,p)::rest))

	  val tmpR1 = getTmpReg()
	  val tmpR2 = getTmpReg()
          val tmpF1 = tmpfpreg
       in 
          emit_or(dataptrR, IMrand 4, dataptrR); (*align*)
          move(tag,Direct tmpR1);
          store(tmpR1, dataptrR, 0);
	  fields (tmpR1, tmpR2, tmpF1, len-8, rev vl);
	  addImmed (dataptrR, wordSzB, dst);
          freeTmpReg tmpR1;
          freeTmpReg tmpR2;
	  addImmed (dataptrR, len, dataptrR)
      end
      | fprecord _ = bug "[SparcCM.fprecord]"

    val faddd = floatOp emit_fadd
    val fsubd = floatOp emit_fsub
    val fmuld = floatOp emit_fmul
    val fdivd = floatOp emit_fdiv

    fun fnegd (FDirect (fpr1 as FREG f1), FDirect (fpr2 as FREG f2)) = (
	  emit_fneg (fpr1, fpr2);
	  if (fpr1 <> fpr2) then emit_fmov (FREG(f1+1), FREG(f2+1)) else ())
      | fnegd _ = bug "[SparcCM.fnegd]"

    fun fabsd (FDirect (fpr1 as FREG f1), FDirect (fpr2 as FREG f2)) = (
	  emit_fabs (fpr1, fpr2);
	  if (fpr1 <> fpr2) then emit_fmov (FREG(f1+1), FREG(f2+1)) else ())
      | fabsd _ = bug "[SparcCM.fabsd]"

  (* convert an int to a double.  Because there is no data-path from general
   * purpose registers to the FP registers, we use the heap as a staging point.
   *)
    local
      fun convert (gpr, fpr) = (
	    emit_st (spR, cvti2dAddrOffset, gpr);
            emit_ldf (spR, cvti2dAddrOffset, fpr);
	    emit_fitod (fpr, fpr))
    in
    fun cvti2d (Direct r, FDirect fpr) = convert (r, fpr)
      | cvti2d (Immed i, FDirect fpr) = let val tmpR = getTmpReg()
	  in
	    loadImmed (i, tmpR);
	    convert (tmpR, fpr);
	    freeTmpReg tmpR
	  end
      | cvti2d _ = bug "[SparcCM.cvti2d]"
    end (* local fun convert ... *)

    fun fbranchd (cond, FDirect fp1, FDirect fp2, ImmedLab lab) = let
          fun sparcFCC P.fEQ  = FCC_E
            | sparcFCC P.fULG = FCC_NE
	    | sparcFCC P.fUN  = FCC_U
	    | sparcFCC P.fLEG = FCC_O
	    | sparcFCC P.fGT  = FCC_G
	    | sparcFCC P.fGE  = FCC_GE
	    | sparcFCC P.fUGT = FCC_UG
	    | sparcFCC P.fUGE = FCC_UGE
	    | sparcFCC P.fLT  = FCC_L
	    | sparcFCC P.fLE  = FCC_LE
	    | sparcFCC P.fULT = FCC_UL
	    | sparcFCC P.fULE = FCC_ULE
	    | sparcFCC P.fLG  = FCC_LG
	    | sparcFCC P.fUE  = FCC_UE
        in
	  emit_fcmp(fp1,fp2);
	  emit_fbcc(sparcFCC cond, lab)
	end
      | fbranchd _ = bug "[SparcCM.fbranchd]"
    end (* local *)

    end (* local *)

  end (* functor SparcCM *)


(*
 * $Log$
 *
 *)
