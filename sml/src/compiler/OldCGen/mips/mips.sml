(* mips.sml
 *
 * Copyright (c) 1992 by AT&T Bell Laboratories
 *
 *)

functor MipsCM(structure C: CODER
	         where type 'a instruction = 'a MipsInstrSet.instruction
		   and type 'a sdi = 'a MipsInstrSet.sdi
	       structure E: ENDIAN
	       structure MachSpec: MACH_SPEC) : CMACHINE =
struct

  structure D = MachSpec.ObjDesc
  val dtoi = LargeWord.toInt	(* convert descriptors to int *)

  structure M = MipsInstrSet
  open M
  type EA = C.label M.EA

  fun bug s = ErrorMsg.impossible ("mips/mips.sml: " ^ s)

  val wtoi = Word.toIntX

  exception BadReal 	= C.BadReal
  val align 		= fn () => ()
  val mark 		= C.mark
  val emitlong 		= C.emitLong
  val realconst 	= C.emitReal
  val emitstring 	= C.emitString
  val newlabel 		= M.ImmedLab o C.newLabel
  val immed       	= M.Immed 
  val emitSDI		= C.emitSDI
  val emit		= C.emit

  fun emitlab(k,ImmedLab lab) = C.emitLabel(lab,k)
    | emitlab _ = bug "MipsCM.emitlab"

  fun define(ImmedLab lab) = C.define lab
    | define _ = bug "MipsCM.define"


  (* Register Map
     Reg   gc   desc
     -------------------------------------
     0	   n	zero
     1     n    temporary reg
     2	   y	standard arg
     3	   y	standard continuation
     4	   y    standard closure
     5	   y	standard link
     6-18  y    misc regs
     19    n    limit reg
     20    n    var pointer
     21    n    temporary reg & heapExhausted reg.
     22    n    store pointer
     23    n    allocation pointer
     24	   n    base reg
     25    n    temporary reg & maskReg
     26-27 n	reserved for OS kernel
     28	   n	C global pointer
     29	   n	C stack pointer
     30    y    exnptr
     31	   n	temporary reg & gclink register
  *)
  val varptr_indexable	 	   = true
  val standardlink : EA            = Direct(Reg 5)
  val standardarg : EA		   = Direct(Reg 2)
  val standardcont : EA		   = Direct(Reg 3)
  val standardclosure : EA	   = Direct(Reg 4)
  val miscregs : EA list 			   = map (Direct o Reg) 
      				         [6,7,8,9,10,11,12,13,14,15,16,17,18]

  val limit as Direct limit' : EA  = Direct(M.limitReg)
  val varptr : EA 	           = Direct(Reg 20)
  val stackptr'                    = (Reg 29)

  val storeptr as Direct storeptr' : EA = Direct(Reg 22)
  val dataptr as Direct dataptr' : EA = Direct(M.allocReg)

  val exnptr	  		   = Direct(M.exnptrReg)

  val floatregs: EA list	   = map (Direct o Freg)
      					 [0,2,4,6,8,10,12,14,16,18]
  val savedfpregs: EA list	   = map (Direct o Freg) [20,22,24,26,28]

  val arithtemps: EA list	   = []

  local
      exception NoTmpRegs
      val tmpRegs	= [M.heapExhaustedReg,M.maskReg,M.linkReg,Reg 1]
      val queue         = ref tmpRegs
  in  
      fun getTmpReg()    = case !queue
	                   of hd :: rest => (queue := rest; hd)
			    | _ => raise NoTmpRegs
      fun freeTmpReg reg = queue := !queue @ [reg]

      (*** should be cleaned up in the future ***)
      val tmpfpreg = (Freg 30)

  end


  fun emitBRANCH(cond,rs,rt,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in 
	  emitSDI(M.BRANCH(cond,rs,rt,lab,tmpR,flabel));
	  C.define flabel;
	  freeTmpReg tmpR
      end

  fun emitBRANCH_COP1(cond,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in
	  emitSDI(M.BRANCH_COP1(cond,lab,tmpR,flabel));
	  C.define flabel;
	  freeTmpReg tmpR
      end


  datatype immedSize = IMMED16 | IMMED32

  fun immed_size n = if (~32764<=n) andalso (n<=32764) 
		     then IMMED16 
		     else IMMED32

  val immed32 = M.Immed32

  fun load_immed(n,r) =  
      case (immed_size n)
	of IMMED16 => emit(M.ADD(r,Reg 0,Immed16Op n))
	 | IMMED32 => let val (hi,lo) = M.split n
		      in  emit (M.LUI(r,Immed16Off(wtoi hi)));
			  emit (M.ADD(r,r,Immed16Op(wtoi lo)))
		      end

  local
    structure W = Word32
  in
    fun load_immed32(w, rd) = let
        val lo = W.andb(w, 0w65535)
	val hi = W.~>>(w, 0w16)
    in emit(M.LUI(rd, Immed16Off(W.toIntX hi)));
       emit(M.OR(rd, rd, Immed16Op(W.toInt lo)))
    end
  end

  fun do_immed_arith(instr,rt,rs,n) =
      case (immed_size n)
	of IMMED16 => emit(instr(rt,rs,Immed16Op n))
	 | IMMED32 => let 
			  val (hi,lo) = M.split n
			  val tmpR = getTmpReg()
		      in 
			  emit (M.LUI(tmpR,Immed16Off(wtoi hi)));
			  emit (M.ADD(tmpR,tmpR,Immed16Op(wtoi lo)));
			  emit (instr(rt,rs,RegOp tmpR));
			  freeTmpReg tmpR
		      end

  fun do_immed_mem(instr,rt,base,n) =
      case (immed_size n)
	of IMMED16 => emit(instr(rt,base,Immed16Off n))
	 | IMMED32 => let 
			  val (hi,lo) = M.split n
			  val tmpR = getTmpReg()
		      in 
			  emit (M.LUI(tmpR,Immed16Off(wtoi hi)));
			  emit (M.ADD(tmpR,tmpR,RegOp base));
			  emit (instr(rt,tmpR,Immed16Off(wtoi lo)));
			  freeTmpReg tmpR
		      end

  fun do_immed_logical(instr,rt,rs,n) = 
      if n >=0 andalso n < 65536 then 
	  emit(instr(rt,rs,Immed16Op n))
      else let val tmpR = getTmpReg()
        in
	  load_immed(n,tmpR);
	  emit(instr(rt,rs,RegOp tmpR));
	  freeTmpReg tmpR
        end

 (*
  * move(a,b) means b <- a
  *)
  val Reg0 = Reg 0
  val RegOp0 : C.label M.arithOpnd = RegOp(Reg 0)

  fun move(Direct a, Direct b) =
        (case (reg_rep a, reg_rep b)
          of (Freg' _, Freg' _) => emit(M.MOV_DOUBLE(b,a))
 	   | (Freg' _, _) => bug "MipsCM.move: destination not a float reg"
	   | (_, Freg' _) => bug "MipsCM.move: source not a float reg"
	   | (Reg' a', Reg' b') => if a'=b' then ()
		                   else emit(M.ADD(b,a,RegOp0)))
    | move(ImmedLab lab, Direct dst) = emitSDI(LOADADDR(dst,lab,0))
    | move(Immed n, Direct dst) = load_immed(n,dst)
    | move(Immed32 w, rd as Direct dst) = load_immed32(w,dst)
    | move _ = bug "MipsCM.move"

  fun jmp (Direct r)     = emit(M.JUMP r)
    | jmp (ImmedLab lab) = emitBRANCH(true,Reg0,Reg0,lab)
    | jmp _              = bug "MipsCM.jmp: bad target"

  (* stackptr' is the stack pointer; pregs_offset is the stack offset
   * of pseudo registers, it should be consistent with the offset in 
   * the MIPS.prim.asm file.
   *)
  val pregs_offset = 16

  fun loadpseudo (Direct x,Immed i) = 
        do_immed_mem(M.LW,x,stackptr',2*(i-1)+pregs_offset)
    | loadpseudo (Direct x,Direct y) =   (* this case is never used *)
        let val tmpR = getTmpReg()
         in emit(M.SLL(tmpR,y,Int5 1));
            emit(M.ADD(tmpR,stackptr',RegOp tmpR));
            emit(M.LW(x,tmpR,Immed16Off (pregs_offset-2)));
            freeTmpReg tmpR
        end
    | loadpseudo _ = bug "[loadpseudo]"

  fun storepseudo(Direct x,Immed i) = 
        do_immed_mem(M.SW,x,stackptr',2*(i-1)+pregs_offset)
    | storepseudo(Direct x,Direct y) =   (* this case is never used *)
        let val tmpR = getTmpReg()
         in emit (M.SLL(tmpR,y,Int5 1));
            emit (M.ADD(tmpR,tmpR,RegOp stackptr'));
            emit (M.SW(x,tmpR,Immed16Off (pregs_offset-2)));
            freeTmpReg tmpR
        end
    | storepseudo _ = bug "[storepseudo]"


 (*
  * jmpindexb(x,y) means pc <- (x+y) 
  *)
  fun jmpindexb(ImmedLab lab,Direct y) = 
      let val tmpR = getTmpReg()
      in 
	emitSDI(LOADADDR(tmpR,lab,0));
	emit(M.ADD(tmpR,y,RegOp tmpR));
	emit(M.JUMP tmpR);
	freeTmpReg tmpR
      end
    | jmpindexb _ = bug "MipsCM.jmpindexb"


 (* should be rewritten to use all the temp registers *)
  fun record(vl, Direct z) = let
        open CPS
	val len = List.length vl
	fun f(_,i,nil) = ()
	  | f((t1,t2),i,(Direct r, SELp(j,p))::rest) = 
	       (* follow ptrs to get the item  *)
		(emit(M.LW(t1,r,Immed16Off(j*4))); 
                 f((t2,t1),i,(Direct t1,p)::rest))
	  | f(t,i,(Direct r,OFFp 0)::rest) = 
	       (*  simple store, last first  *) 
		(emit(M.SW(r,dataptr',Immed16Off(i*4)));  f(t,i-1,rest))
	  | f((t1,t2),i,(Direct r, OFFp j)::rest) =          
               bug "unexpected non-zero OFFp record fields"
(*
	  | f((t1,t2),i,(Direct r, OFFp j)::rest) = 
		(emit(M.ADD(t1,r,Immed16Op(4*j))); 
		 f((t2,t1),i,(Direct t1,OFFp 0)::rest))
*)
	  | f((t1,t2),i,(ea,p)::rest) =
	       (* convert to register-based  *)
		(move(ea,Direct t1);  f((t2,t1),i,(Direct t1,p)::rest))
	val tmpR1 = getTmpReg()
	val tmpR2 = getTmpReg()
      in 
       (* store first word in 0(dataptr') *)
	f((tmpR1,tmpR2),len-1,rev vl); 
	freeTmpReg tmpR1;
	freeTmpReg tmpR2;
	emit (M.ADD(z,dataptr',Immed16Op 4));
	do_immed_arith(M.ADD,dataptr',dataptr',4*len)
      end
    | record _ = bug "MipsCM.record: result not a register"


 (* should be rewritten to use all the temp registers *)
  fun recordcont(vl, Direct z, n) = let
        open CPS
	val len = List.length vl
        val _ = if (len > n) 
                then bug "continuation records is larger than framesize"
                else ()
	fun f(_,i,nil) = ()
	  | f((t1,t2),i,(Direct r, SELp(j,p))::rest) = 
	       (* follow ptrs to get the item  *)
		(emit(M.LW(t1,r,Immed16Off(j*4))); 
                 f((t2,t1),i,(Direct t1,p)::rest))
	  | f(t,i,(Direct r,OFFp 0)::rest) = 
	       (*  simple store, last first  *) 
		(emit(M.SW(r,dataptr',Immed16Off(i*4)));  f(t,i-1,rest))
	  | f((t1,t2),i,(Direct r, OFFp j)::rest) = 
		(emit(M.ADD(t1,r,Immed16Op(4*j))); 
		 f((t2,t1),i,(Direct t1,OFFp 0)::rest))
	  | f((t1,t2),i,(ea,p)::rest) =
	       (* convert to register-based  *)
		(move(ea,Direct t1);  f((t2,t1),i,(Direct t1,p)::rest))
	val tmpR1 = getTmpReg()
	val tmpR2 = getTmpReg()
      in 
       (* store first word in 0(dataptr') *)
	f((tmpR1,tmpR2),len-1,rev vl); 
	freeTmpReg tmpR1;
	freeTmpReg tmpR2;
	emit (M.ADD(z,dataptr',Immed16Op 4));
	do_immed_arith(M.ADD,dataptr',dataptr',4*n)
      end
    | recordcont _ = bug "MipsCM.record: result not a register"


  (* recordStore(x, y, alwaysBoxed) records a store operation into mem[x+2*(y-1)].
   * The flag alwaysBoxed is true if the value stored is guaranteed to be boxed.
   *)
    fun recordStore (x, y, _) = let
	  fun storeListUpdate r = (
		  emit (M.SW(r, dataptr', Immed16Off 0));
		  emit (M.SW(storeptr', dataptr', Immed16Off 4));
		  emit (M.ADD(storeptr', dataptr', RegOp0));
		  emit (M.ADD(dataptr', dataptr', Immed16Op 8)))
	  in
	    case (x, y)
	     of (Direct r, Immed 1) => storeListUpdate r
	      | (Direct r, Immed i) => let val tmpR = getTmpReg()
		  in
		    do_immed_arith (M.ADD, tmpR, r, 2*(i-1));
		    storeListUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | (Direct r1, Direct r2) => let val tmpR = getTmpReg()
		  in
		    emit (M.ADD(tmpR, r2, Immed16Op ~1));
		    emit (M.ADD(tmpR, tmpR, RegOp tmpR));
		    emit (M.ADD(tmpR, tmpR, RegOp r1));
		    storeListUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | _ => ErrorMsg.impossible "[MipsCM.recordStore]"
	    (* end case *)
	  end (* recordStore *)
(*** STOREPTR
    fun recordStore (x, y, alwaysBoxed) = let
	(* NOTE: eventually we can optimize the case where alwaysBoxed = false *)
	  fun storeVectorUpdate r = (
		  emit (M.SW(r, M.limitReg, Immed16Off 4092));
		  emit (M.ADD(M.limitReg, M.limitReg, Immed16Op ~4)))
	  in
	    case (x, y)
	     of (Direct r, Immed 1) => storeVectorUpdate r
	      | (Direct r, Immed i) => let val tmpR = getTmpReg()
		  in
		    do_immed_arith (M.ADD, tmpR, r, 2*(i-1));
		    storeVectorUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | (Direct r1, Direct r2) => let val tmpR = getTmpReg()
		  in
		    emit (M.ADD(tmpR, r2, Immed16Op ~1));
		    emit (M.ADD(tmpR, tmpR, RegOp tmpR));
		    emit (M.ADD(tmpR, tmpR, RegOp r1));
		    storeVectorUpdate tmpR;
		    freeTmpReg tmpR
		  end
	      | _ => ErrorMsg.impossible "[MipsCM.recordStore]"
	    (* end case *)
	  end (* recordStore *)
***)
(*** STOREPTR
    fun recordStore (x, y, _) = record ([
	    (Immed(dtoi(D.makeDesc(3, dtoi D.tag_record))), CPS.OFFp 0),
	    (x, CPS.OFFp 0), (y, CPS.OFFp 0), (storeptr, CPS.OFFp 0)
	  ], storeptr)
***)


    fun select(i,Direct v',Direct w) = do_immed_mem(M.LW,w,v',i*4)
      | select(i,ImmedLab lab,Direct w) = emitSDI(LOAD(w,lab,i*4))
      | select _ = bug "MipsCM.select: bad dst"


  fun offset(i,v,Direct w) =
       (case v
	 of Direct v'    => do_immed_arith(M.ADD,w,v',i*4)
	  | ImmedLab lab => let val tmpR = getTmpReg()
			    in
				emitSDI(LOADADDR(tmpR,lab,0));
				do_immed_arith(M.ADD,w,tmpR,i*4);
				freeTmpReg tmpR
			    end
	  | _ 	       => bug "MipsCM.offset: bad src")
    | offset _ = bug "MipsCM.offset: bad dst"


 (* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z], 
  *	where y is not x or z 
  *)
  fun fetchindexb(Direct x,Direct y,Immed indx)  = do_immed_mem(M.LBU,y,x,indx)
    | fetchindexb(Direct x,Direct y,Direct indx) = let
          val tmpR = getTmpReg()
      in 
	  emit (M.ADD(tmpR,indx,RegOp x));
	  emit (M.LBU(y,tmpR,Immed16Off 0));
	  freeTmpReg tmpR
      end
    | fetchindexb _ = bug "MipsCM.fetchindexb"


 (* 
  * storeindexb(x,y,z) stores a byte: mem[y+z] <- x. 
  *)
  fun storeindexb(Immed xi,y,z) = 
      let val tmpR = getTmpReg()
      in
	  do_immed_arith(M.ADD,tmpR,Reg0,xi);  
	  storeindexb(Direct tmpR,y,z);
	  freeTmpReg tmpR
      end
    | storeindexb(Direct x,Direct y,Direct indx) =
      let val tmpR = getTmpReg()
      in 
	  emit (M.ADD(tmpR,y,RegOp indx));
	  emit (M.SB(x,tmpR,Immed16Off 0));
	  freeTmpReg tmpR
      end
    | storeindexb(Direct x,Direct y,Immed indx) = do_immed_mem(M.SB,x,y,indx)
    | storeindexb _ = bug "MipsCM.storeindexb" 


 (* 
  * fetchindexl(x,y,z) fetches a word:   y <- mem[x+2*(z-1)] 
  *)
  fun fetchindexl(x,Direct y,Direct z') = 
      let val tmpR = getTmpReg()
      in
	  emit(M.SLL(tmpR,z',Int5 1));
	  (case x 
	       of Direct x' => (emit (M.ADD(tmpR,x',RegOp tmpR));
				emit (M.LW(y,tmpR,Immed16Off ~2)))
	     | Immed n      => do_immed_mem(M.LW,y,tmpR,n-2)
	     | ImmedLab lab => 
		   let val tmpR2 = getTmpReg()
		   in
		       emitSDI(M.LOADADDR(tmpR2,lab,0));
		       emit(M.ADD(tmpR,tmpR,RegOp tmpR2));
		       freeTmpReg tmpR2;
		       emit(M.LW(y,tmpR,Immed16Off ~2))
		   end);
	       freeTmpReg tmpR
      end
    | fetchindexl(x,Direct y,Immed z') =  
      (case x
	 of Direct x'    => do_immed_mem(M.LW,y,x',2*(z'-1))
	  | Immed n      => do_immed_mem(M.LW,y,Reg0,n+2*(z'-1))
	  | ImmedLab lab => emitSDI(LOAD(y,lab,2*(z'-1))))
    | fetchindexl _ = bug "MipsCM.fetchindexl"


 (* 
  * storeindexl(x,y,z) stores a word:    mem[y+2*(z-1)] <- x 
  *)
  fun storeindexl(Direct x,Direct y,Direct z) = 
      let val tmpR = getTmpReg()
      in 
	  emit (M.SLL(tmpR,z,Int5 1));
	  emit (M.ADD(tmpR,tmpR,RegOp y));
	  emit (M.SW(x,tmpR,Immed16Off ~2));
	  freeTmpReg tmpR
      end
    | storeindexl(Direct x,Direct y,Immed zi) = do_immed_mem(M.SW,x,y,2*(zi-1))
    | storeindexl(Immed xi,y,z) =  let val tmpR = getTmpReg()
				   in
				       load_immed(xi,tmpR); 
				       storeindexl(Direct tmpR,y,z);
				       freeTmpReg tmpR
				   end
    | storeindexl(ImmedLab lab,y,z) = let val tmpR = getTmpReg()
				      in
					  emitSDI(M.LOADADDR(tmpR,lab,0));
					  storeindexl(Direct tmpR,y,z);
					  freeTmpReg tmpR
				      end
    | storeindexl _ = bug "MipsCM.storeindexl: bad args"


 (*
  * three - can *only* be used for commutative operators
  *)
  fun three f (do_immed, x as Direct x', y as Direct y', ea) = 
      (case ea
	 of Immed zi     => do_immed(f,x',y',zi)
          | Immed32 w    => let val tmpR = getTmpReg()
			    in load_immed32(w,tmpR);
			       three f (do_immed, x, y, Direct tmpR);
			       freeTmpReg tmpR
			    end
	  | Direct z'    => emit(f(x',y',RegOp z'))
	  | ImmedLab lab => let val tmpR = getTmpReg()
			    in
				emitSDI(M.LOADADDR(tmpR,lab,0));  
				emit(f(x',y',RegOp tmpR));
				freeTmpReg tmpR
			    end)

    | three f (do_immed,Direct x', ea, Direct z') = 
        three f (do_immed,Direct x',Direct z',ea)
    | three f (do_immed,x, Immed yi,z) = let val tmpR = getTmpReg()
      in
	load_immed(yi,tmpR);
	three f (do_immed,x,Direct tmpR,z);
	freeTmpReg tmpR
      end
    | three f (do_immed,x, Immed32 w,z) = let val tmpR = getTmpReg()
      in
	load_immed32(w,tmpR);
	three f (do_immed,x,Direct tmpR,z);
	freeTmpReg tmpR
      end
    | three _ _ = bug "MipsCM.three: bad args"

  fun add(x,y,z)	= three M.ADDU (do_immed_arith,z,x,y)
  fun orb(x,y,z) 	= three M.OR  (do_immed_logical,z,x,y) 
  fun andb(x,y,z)	= three M.AND (do_immed_logical,z,x,y)
  fun xorb(x,y,z)	= three M.XOR (do_immed_logical,z,x,y)

 (* Subtraction may appear a bit odd. 
  * The MIPS machine instruction and  MIPSCODER.sub both subtract 
  * their second operand from their first operand.
  * The CMACHINE.sub subtracts the first operand from the second.
  * This will certainly lead to endless confusion.
  *
  * sub(a,b,c) mean c <- b-a
  *)
  fun sub (Direct x,Direct y,Direct z) = emit(M.SUBU(z,y,x))
    | sub (Immed xi,y,z) = let val tmpR = getTmpReg()
			   in load_immed(xi,tmpR);
			      sub (Direct tmpR,y,z);
			      freeTmpReg tmpR
			   end
    | sub (Immed32 w,y,z) = let val tmpR = getTmpReg()
			    in load_immed32(w,tmpR);
			       sub(Direct tmpR, y, z);
			       freeTmpReg tmpR
			    end
    | sub (x,Immed 0,dest) = sub (x, Direct(Reg0), dest)
    | sub (x,Immed y,z) = let val tmpR = getTmpReg()
			  in load_immed(y,tmpR);
			      sub (x,Direct tmpR,z);
			      freeTmpReg tmpR
			  end
    | sub (x, Immed32 w, z) = let val tmpR = getTmpReg()
			      in load_immed32(w,tmpR);
				 sub(x, Direct tmpR, z);
				 freeTmpReg tmpR
			      end
    | sub  _ = bug "MipsCM.sub: mismatched args"

  fun notb(a,b) 	= sub (a, Immed ~1, b)


 (* 
  * integer arithmetic with overflow trapping - addt subt mult divt
  *)
  fun addt (Immed ai, Immed bi, Direct rd) =
        (load_immed(ai,rd);  do_immed_arith(M.ADD,rd,rd,bi))
    | addt (Immed32 aw, b, c as Direct rd) = 
	(load_immed32(aw,rd);  addt(b, c, c))
    | addt (x, y, z) = three M.ADD (do_immed_arith,z,x,y)

  fun subt (Immed xi,y,z) 	      = addt(y,Immed (~xi),z)
    | subt (Direct x,Direct y,Direct z)= emit(M.SUB(z,y,x))
    | subt (x,Immed 0,dest)            = subt (x, Direct(Reg0), dest)
    | subt (x,Immed k,dest)            = let val tmpR = getTmpReg()
					  in
					      do_immed_arith(M.ADD,tmpR,Reg0,k);
					      subt (x,Direct tmpR,dest);
					      freeTmpReg tmpR
					  end
    | subt (Immed32 x, y, z)           = let val tmpR = getTmpReg()
					  in 
					    load_immed32(x, tmpR);
					    subt(Direct tmpR, y, z);
					    freeTmpReg tmpR
					 end
    | subt (x, Immed32 y, z)           = let val tmpR = getTmpReg()
					  in 
					    load_immed32(y, tmpR);
					    subt(x, Direct tmpR, z);
					    freeTmpReg tmpR
					 end

    | subt  _ = bug "MipsCM.subt: mismatched args"

 (* The Mips multiplies two 32-bit quantities to get a 64-bit result.
  * That result fits in 32 bits if and only if the high-order word is zero
  * or negative one, and it has the same sign as the low order word.
  * Thus, we can add the sign bit of the low order word to the high order 
  * word, and we have overflow if and only if the result is nonzero.
  *)
  fun mult(ea,y as Direct y') =
      let val tmpR = getTmpReg()
      in 
	  (case ea
	   of Immed xi  => 
	       (do_immed_arith(M.ADD,tmpR,Reg0, xi);  mult(Direct tmpR,y))
	    | Immed32 wi => 
	       (load_immed32(wi, tmpR); mult(Direct tmpR, y))
	    | Direct x' => 
	       let val ok = C.newLabel()
	       in  emit (M.MULT(x',y'));
		   emit (M.MFLO y');
		   emit (M.SLT(y',y',RegOp (Reg0)));
		   emit (M.MFHI tmpR);
		   emit (M.ADD(tmpR,y',RegOp tmpR));
		   emit (M.MFLO y');
		   emitBRANCH(true,tmpR,Reg0,ok);
		   emit (M.LUI(tmpR,Immed16Off 32767));
		   emit (M.ADD(tmpR,tmpR,RegOp tmpR));
		   C.define ok
	       end
	    | _ => bug "MipsCM.mult");
	  freeTmpReg tmpR
      end
    | mult _ = bug "MipsCM.mult: result not a register"

  fun mulu(Direct x,Direct y) = 
      (emit(M.MULTU(x,y)); emit(M.MFLO y))
    | mulu(Immed32 xi,y) = let val tmpR = getTmpReg()
      in
	load_immed32(xi,tmpR);
	mulu(Direct tmpR,y); 
	freeTmpReg tmpR
      end
    | mulu _ = bug "mulu"


 (*
  * divt(a,b) means b <- b div a 
  *)
  fun divt(Direct x',Direct y') = 
      let val oklabel = C.newLabel()
      in 
	  (* emit (M.DIV(y',x')); *)
	  emitBRANCH(false,Reg0,x',oklabel);
	  emit(M.BREAK 7);
	  C.define oklabel;
	  emit (M.DIV(y',x'));
	  emit (M.MFLO y')
      end
    | divt(Immed xi, y) = 
      let val tmpR = getTmpReg()
      in
	  do_immed_arith(M.ADD,tmpR,Reg0,xi);
	  divt(Direct tmpR,y);
	  freeTmpReg tmpR
      end
    | divt(Immed32 xi, y) = 
      let val tmpR = getTmpReg()
      in
	load_immed32(xi, tmpR);
	divt(Direct tmpR, y);
	freeTmpReg tmpR
      end
    | divt _ = bug "MipsCM.divt: mismatched args"

  fun divtu(Direct x',Direct y') = let
        val oklabel = C.newLabel()
      in
	emitBRANCH(false,Reg0,x',oklabel);
	emit(M.BREAK 7);
	C.define oklabel;
	emit (M.DIVU(y',x'));
	emit (M.MFLO y')
      end
    | divtu(Immed32 w,y) = let
        val tmpR = getTmpReg()
      in
	load_immed32(w,tmpR);
	divtu(Direct tmpR,y);
	freeTmpReg tmpR
      end
      

  fun ashr(shamt,Direct rt,Direct rd) =
      (case shamt
	   of Direct rs => emit(M.SRAV(rd,rt,rs))
	    | Immed n      => 
	       if n >= 32 orelse n < 0 then
		   bug "MipsCM.ashr: Too large a shift distance"
	       else
		   emit(M.SRA(rd,rt,Int5 n))
	    | _ => bug "MipsCM.ashr")
    | ashr(shamt,Immed n,dst) = let val tmpR = getTmpReg()
				in  
				    load_immed(n,tmpR);
				    ashr(shamt,Direct tmpR,dst);
				    freeTmpReg tmpR
				end
    | ashr(shamt, Immed32 w, dst) = let val tmpR = getTmpReg()
				    in load_immed32(w,tmpR);
				       ashr(shamt, Direct tmpR, dst);
				       freeTmpReg tmpR
				    end
    | ashr _ = bug "MipsCM.ashr: bad args"

  fun lshr(shamt,Direct rt,Direct rd) = 
      (case shamt
       of Direct rs => emit(M.SRLV(rd,rt,rs))
        | Immed n => 
	    if n >= 32 orelse n < 0 then
		bug "MipsCM.lshr: bad shift distance"
	    else
		emit(M.SRL(rd,rt,Int5 n))
	| _ => bug "MipsCM.ashr")
    | lshr(shamt,Immed n,dst) = let val tmpR = getTmpReg()
      in
	  load_immed(n,tmpR);
	  lshr(shamt,Direct tmpR,dst);
	  freeTmpReg tmpR
      end
    | lshr(shamt, Immed32 w, dst) = let val tmpR = getTmpReg()
      in
	  load_immed32(w,tmpR);
	  lshr(shamt,Direct tmpR,dst);
	  freeTmpReg tmpR
      end

    | lshr _ = bug "MipsCM.ashr: bad args"

  fun ashl(shamt,Direct rt,Direct rd) =
      (case shamt
	   of Direct rs => emit(M.SLLV(rd,rt,rs))
	 | Immed n      => 
	       if n >= 32 orelse n < 0 then
		   bug "MipsCM.ashl: Too large a shift distance"
	       else
		   emit(M.SLL(rd,rt,Int5 n))
	 | _ => bug "MipsCM.ashl")
    | ashl(shamt,Immed n,dst) = let val tmpR = getTmpReg()
				in  
				    load_immed(n,tmpR);
				    ashl(shamt,Direct tmpR,dst);
				    freeTmpReg tmpR
				end
    | ashl(shamt,Immed32 w,dst) = let val tmpR = getTmpReg()
				  in  
				    load_immed32(w,tmpR);
				    ashl(shamt,Direct tmpR,dst);
				    freeTmpReg tmpR
				  end
    | ashl _ = bug "MipsCM.ashl: bad args"

  datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR
	             | GEU | GTU | LTU | LEU

(** NOTE: these optimizations ought to be done in the CPS phase!!!
 ** ALSO: maybe we want to add a "wbranch" function and get rid of the
 ** unsigned condition codes.
 **)
  fun ibranch(cond,Immed a,Immed b,ImmedLab lab) = let
        val condVal = (case cond
	       of EQL => a=b  | NEQ => a<>b | LSS => a<b
		| LEQ => a<=b | GTR => a>b  | GEQ => a>=b
		| GTU => (Word.fromInt a > Word.fromInt b)
		| GEU => (Word.fromInt a >= Word.fromInt b)
		| LTU => (Word.fromInt a < Word.fromInt b)
		| LEU => (Word.fromInt a <= Word.fromInt b)
	      (* end case *))
	in
	  if condVal then emitBRANCH(true,Reg0,Reg0,lab) else ()
	end
    | ibranch(cond,Immed32 a,Immed32 b,ImmedLab lab) = let
        val wtoi = Int32.fromLarge o Word32.toLargeIntX
        fun cmpi32(cmp, a, b) = cmp(wtoi a, wtoi b)
      in
        if (case cond 
	   of EQL => a = b
	    | NEQ => a <> b
            | GEU => Word32.>=(a,b)
	    | GTU => Word32.>(a,b)
	    | LTU => Word32.<(a,b)
	    | LEU => Word32.<=(a,b)
	    | LEQ => cmpi32(Int32.<=, a, b)
	    | LSS => cmpi32(Int32.<, a, b)
            | GEQ => cmpi32(Int32.>=, a, b)
	    | GTR => cmpi32(Int32.>, a, b))
        then emitBRANCH(true, Reg0, Reg0, lab)
        else ()
      end
    | ibranch(cond,Immed32 a,b,lab) = let val tmpR = getTmpReg()
      in load_immed32(a,tmpR);
	 ibranch(cond,Direct tmpR,b,lab);
	 freeTmpReg tmpR
      end
    | ibranch(cond,a,Immed32 b,lab) = let val tmpR = getTmpReg()
      in load_immed32(b,tmpR);
	 ibranch(cond,a,Direct tmpR,lab);
	 freeTmpReg tmpR
      end
    | ibranch(cond,Immed n,Direct t,label) =
      let val tmpR = getTmpReg()
      in  
	  load_immed(n,tmpR);
	  ibranch(cond,Direct tmpR,Direct t,label);
	  freeTmpReg tmpR
      end
    | ibranch(cond,Direct rs,Immed n,label) =
     (*
      * could do a better job of this case (ref.G.Kane, table C.2)
      *)
      let val tmpR = getTmpReg()
      in 
	  load_immed(n,tmpR); 
	  ibranch(cond,Direct rs,Direct tmpR,label);
	  freeTmpReg tmpR
      end
    | ibranch(cond,Direct rs,Direct rt,ImmedLab lab) = let val tmpR = getTmpReg()
      in
        case cond
	 of NEQ => emitBRANCH(false,rs,rt,lab)
          | EQL => emitBRANCH(true,rs,rt,lab)
	  | LEQ => (emit(M.SLT(tmpR,rt,RegOp rs)); 
		    emitBRANCH(true,tmpR,Reg0,lab))
	  | GEQ => (emit(M.SLT(tmpR,rs,RegOp rt));
		    emitBRANCH(true,tmpR,Reg0,lab))
	  | LSS => (emit(M.SLT(tmpR,rs,RegOp rt));
		    emitBRANCH(false,tmpR,Reg0,lab))
	  | GTR => (emit(M.SLT(tmpR,rt,RegOp rs));
		    emitBRANCH(false,tmpR,Reg0,lab))
	  | GEU => (emit(M.SLTU(tmpR,rs,RegOp rt));
		    emitBRANCH(true,tmpR,Reg0,lab))
	  | GTU => (emit(M.SLTU(tmpR,rt,RegOp rs));
		    emitBRANCH(false,tmpR,Reg0,lab))
	  | LTU => (emit(M.SLTU(tmpR,rs,RegOp rt));
		    emitBRANCH(false,tmpR,Reg0,lab))
	  | LEU => (emit(M.SLTU(tmpR,rt,RegOp rs));
		    emitBRANCH(true,tmpR,Reg0,lab))
	(*esac*);
	freeTmpReg tmpR
      end
    | ibranch _ = bug "MipsCM.ibranch: bad args"

 (*
  * bbs - branch on bit set.
  *)
  fun bbs(Immed k, Direct y, ImmedLab label) =
      let val tmpR = getTmpReg()
      in
	  do_immed_logical(M.AND,tmpR,y, 
			     Word.toIntX(Word.<<(0w1, Word.fromInt k)));
	  emitBRANCH(false,tmpR,Reg0,label);
	  freeTmpReg tmpR
      end
    | bbs _ = bug "MipsCM.bbs: bad args"


  fun floatreg (Direct fpr) = 
         (case reg_rep fpr 
	   of Freg' _ => fpr 
	    | _ => bug "MipsCM.floatreg: expected floatreg")
    | floatreg _ = bug "MipsCM.floatreg: expected floatreg"

  local 
    val real_tag = dtoi D.desc_reald
    val lowOff = E.low_order_offset

    fun store_float(n',dst,offset) = 
          case (reg_rep n', dst)
	   of (Freg' n, Direct dst') =>
	       if n mod 2 <> 0 then
		    bug "MipsCM.store_float: bad float reg"
	       else (do_immed_mem (M.SWC1,Freg(n+1-lowOff),dst',offset+4);
		     do_immed_mem (M.SWC1,Freg(n+lowOff),dst',offset))
	    | _ => bug "MipsCM.store_float: bad args"

    fun load_float(dest',src,offset) =
          case reg_rep dest'
	   of Freg' dest =>
	       if dest mod 2 <> 0 then bug "MipsCM.load_float.1"
	       else 
                (case src
     	          of Direct src' => 
		     (do_immed_mem(M.LWC1,Freg(dest+lowOff),src',offset);
	              do_immed_mem(M.LWC1,Freg(dest+1-lowOff),src',offset+4))
		   | ImmedLab lab => 
	              let val tmpR = getTmpReg()
		       in emitSDI(LOADF(Freg dest,lab,offset,tmpR));
		          freeTmpReg tmpR
		      end
		   | _ => bug "MipsCM.load_float.3")
	    | _ => bug "MipsCM.load_float.2"

  in
    fun storefloat(src,Direct dst) =
	  (case reg_rep dst 
	    of Reg' result =>
		(store_float(floatreg src,dataptr,4);
		 let val tmpR = getTmpReg()
		 in
		     emit (M.ADD(tmpR,Reg0,Immed16Op real_tag));
		     emit (M.SW(tmpR,dataptr',Immed16Off 0));
		     emit (M.ADD(Reg result,dataptr',Immed16Op 4));
		     emit (M.ADD(dataptr',dataptr',Immed16Op 12));
		    freeTmpReg tmpR
		end)
	     | _ => bug "MipsCM.storefloat: bad args")
      | storefloat _ = bug "MipsCM.storefloat: bad args.2"

    fun loadfloat(src, dst) = load_float(floatreg dst,src,0)
					  (* y <- mem[x+4*(z-1)] *)
    fun fetchindexd(Direct x, y, z) =
	  (case z 
	    of Immed i   => load_float(floatreg y, Direct x, 4*(i-1))
	     | Direct z' => let val tmpR = getTmpReg()
			    in
				emit (M.SLL(tmpR,z',Int5 2));
				emit (M.ADD(tmpR,x,RegOp tmpR));
				load_float(floatreg y, Direct tmpR, ~4);
				freeTmpReg tmpR
			    end
	     | _ => bug "MipsCM.fetchindexd")
      | fetchindexd _ = bug "MipsCM.fetchindexd"

					  (* mem[y+4*(z-1)] <- x *)
    fun storeindexd(x, Direct y, z) =
	  (case z 
	    of Immed i => store_float(floatreg x,Direct y, 4*(i-1))
	     | Direct z' => let val tmpR = getTmpReg()
			    in
				emit (M.SLL(tmpR,z',Int5 2));
				emit (M.ADD(tmpR,y,RegOp tmpR));
				store_float(floatreg x,Direct tmpR,~4);
				freeTmpReg tmpR
			    end
	     | _ => bug "MipsCM.storeindexd")
      | storeindexd _ = bug "MipsCM.storeindexd"

  fun fprecord(tag, vl, Direct z) = 
        let open CPS
	    val len = (List.length vl) * 8 + 4
	    fun f(_,_,_,i,nil) = ()
	      | f(t1,t2,f1,i,(Direct r, SELp(j,OFFp 0))::rest) = 
                  (case (reg_rep r, reg_rep f1) 
                    of (Reg' src, Freg' dest) =>
                       (do_immed_mem(M.LWC1,Freg(dest+lowOff),r,j*8);
                        do_immed_mem(M.LWC1,Freg(dest+1-lowOff),r,j*8+4);
                        f(t1,t2,f1,i,(Direct f1, OFFp 0)::rest))
                     | _ => bug "wrong register assignment1 in mips.sml")
              | f(t1,t2,f1,i,(Direct r, SELp(j,p))::rest) = 
                   (case reg_rep r 
                     of (Reg' src) =>
                        (do_immed_mem(M.LW,t1,r,j*4);
                         f(t2,t1,f1,i,(Direct t1,p)::rest))
                      | _ => bug "wrong register assignment3 in mips.sml")
	      | f(t1,t2,f1,i,(Direct r, OFFp 0)::rest) = 
                  (case reg_rep r 
                    of (Freg' n) =>
                       (do_immed_mem(M.SWC1,Freg(n+1-lowOff),dataptr',i+4);
                        do_immed_mem(M.SWC1,Freg(n+lowOff),dataptr',i);
                        f(t1,t2,f1,i-8,rest))
                     | _ => bug "wrong register assignment2 in mips.sml")
	      | f(t1,t2,f1,i,(Direct r, OFFp j)::rest) = 
                  bug "non-zero offset elements in fprecord in mips.sml"
	      | f(t1,t2,f1,i,(ea, p)::rest) =
		  (move(ea,Direct t1); f(t2,t1,f1,i,(Direct t1,p)::rest))

	    val tmpR1 = getTmpReg()
	    val tmpR2 = getTmpReg()
            val tmpF1 = tmpfpreg
         in 
            orb(dataptr,Immed 4,dataptr);  (* align *)
            move(tag,Direct tmpR1);
            emit(M.SW(tmpR1,dataptr',Immed16Off 0));
	    f(tmpR1,tmpR2,tmpF1,len-8,rev vl); 
            freeTmpReg tmpR1;
            freeTmpReg tmpR2;
            emit (M.ADD(z,dataptr',Immed16Op 4));
            do_immed_arith(M.ADD,dataptr',dataptr',len)
        end
    | fprecord _ = bug "MipsCM.fprecord: result not a register"

  end

 (* Note: mipsdepend will ensure that nothing generated here gets reordered.
  * Also note that an unsigned comparison is necessary, since this is a pointer
  * comparison.
  *)
  fun testLimit() = emit(M.SLTU(M.heapExhaustedReg,dataptr',RegOp(limit')))
  fun decLimit n = do_immed_arith(M.ADD,limit',limit',~n)  (* for polling *)

  val startgc_offset = MachSpec.startgcOffset

  fun checkLimit(max_allocation, lab, mask, rlab, fregs) =
      (* NOTE: THIS CODE USES TEMP REGS BY ALIASES.
       Thus it is important that none of the emitted pseudo-instructions
       below uses getTmpReg(), directly or indirectly. *)
    let val lab' = C.newLabel()
        val _ = if max_allocation > 4096 then 
                  (do_immed_arith(M.ADD,M.heapExhaustedReg,dataptr',
		                  max_allocation - 4096);
                   emit(M.SLTU(M.heapExhaustedReg,M.heapExhaustedReg,
                               RegOp(limit'))))
                else ()
        val _ = emitBRANCH(false,M.heapExhaustedReg, Reg0, lab')
     in (case fregs 
          of [] => (do_immed_mem(M.LW,M.heapExhaustedReg,stackptr',
                                 startgc_offset);
	            move(mask, Direct M.maskReg);
	            move(lab, Direct M.linkReg);
                    emit(M.JUMP M.heapExhaustedReg))
           | _ => (let val k = length fregs
                       val lowOff = E.low_order_offset
                       val desc = dtoi(D.makeDesc(k * 8, D.tag_string))
                       val retlab = C.newLabel()

                       fun deposit([], _) = ()
                         | deposit((Direct x)::r, i) = 
                             (case (reg_rep x)
                               of (Freg' n) => 
                                    (do_immed_mem(M.SWC1,Freg(n+1-lowOff),
                                                  dataptr',i+4);
                                     do_immed_mem(M.SWC1,Freg(n+lowOff),
                                                  dataptr',i);
		                     deposit(r, i+8))
                                | _ => bug "wrong register checkLimit")

                       fun restore(s, [], _) = ()
                         | restore(s, (Direct x)::r, i) = 
                             (case (reg_rep x)
                               of (Freg' n) =>
                                    (do_immed_mem(M.LWC1,Freg(n+1-lowOff),
                                                  s,i+4);
                                     do_immed_mem(M.LWC1,Freg(n+lowOff),
                                                  s,i);
                                     restore(s, r, i+8))
                                | _ => bug "wrong register checkLimit")

                    in deposit(fregs,4);
                       move(immed desc, Direct M.heapExhaustedReg);
                       (* orb(dataptr,Immed 4,dataptr);*) (* align *)
                       emit(M.SW(M.heapExhaustedReg, dataptr', Immed16Off 0));
                       emit(M.ADD(M.maskReg, dataptr', Immed16Op 4));
                       do_immed_arith(M.ADD,dataptr',dataptr',k*8+4);
                       do_immed_mem(M.SW,M.maskReg,stackptr',4+pregs_offset);
                       (* I am using pseudo register #2 here !!! *)

                       do_immed_mem(M.LW,M.heapExhaustedReg,stackptr',
                                    startgc_offset);
	               move(mask, Direct M.maskReg);
	               move(ImmedLab retlab, Direct M.linkReg);
                       emit(M.JUMP M.heapExhaustedReg);

                       C.define retlab;
                       do_immed_mem(M.LW,M.maskReg,stackptr',4+pregs_offset);
                       (* I am using pseudo register #2 here !!! *)
                       move(rlab, Direct M.linkReg);
                       restore(M.maskReg, fregs, 0);
                       emit(M.JUMP M.linkReg)
                   end));
        C.define lab'
    end

  fun beginStdFn(ImmedLab lab, Direct reg) = emitSDI(M.SETBASEADDR(lab,reg))

  local 
      structure P = CPS.P
      fun floating_arith f (x,y,z) = emit(f(floatreg x,floatreg y,floatreg z))
  in
      fun fmuld(x,y,z) 	  = floating_arith M.MUL_DOUBLE (z,x,y)
      fun fdivd(x,y,z) 	  = floating_arith M.DIV_DOUBLE (z,x,y)
      fun faddd(x,y,z) 	  = floating_arith M.ADD_DOUBLE (z,x,y)
      fun fsubd(x,y,z) 	  = floating_arith M.SUB_DOUBLE (z,x,y)
      fun fnegd(op1,result) = emit(M.NEG_DOUBLE(floatreg result,floatreg op1))
      fun fabsd(op1,result) = emit(M.ABS_DOUBLE(floatreg result,floatreg op1))

      fun fbranchd (cond, op1, op2, ImmedLab label) = let
	    fun compare P.fEQ   = (M.EQ, true)
	      | compare P.fULG  = (M.EQ, false)
	      | compare P.fGT   = (M.NGT, false)
	      | compare P.fGE   = (M.NGE, false)
	      | compare P.fLT   = (M.LT, true)
	      | compare P.fLE   = (M.LE, true)
	      | compare P.fLG   = (M.UEQ, false)
	      | compare P.fLEG  = (M.NGLE, false)
	      | compare P.fUGT  = (M.LE, false)
	      | compare P.fUGE  = (M.LT, false)
	      | compare P.fULT  = (M.ULT, true)
	      | compare P.fULE  = (M.ULE, true)
	      | compare P.fUE   = (M.UEQ, true)
	      | compare P.fUN   = (M.EQ, true)

	    val (cmp, test) = compare cond
	  in
	    emit(M.FCMP(cmp, floatreg op1, floatreg op2));
	    emitBRANCH_COP1(test, label)
	  end
	| fbranchd _ = bug "MipsCM.fbranchd: insane target"
  end


  fun cvti2d(Direct src,dst as Direct dst') = 
      (case (reg_rep src, reg_rep dst')
        of (Reg' _, Freg' _) => (emit (M.MTC1(src, dst'));
				   emit (M.CVTI2D(dst', dst'))))
    | cvti2d(Immed n, dst) = 
                   let val tmpR = getTmpReg()
		    in  do_immed_arith(M.ADD,tmpR,Reg0,n);
			cvti2d(Direct tmpR,dst);
			freeTmpReg tmpR
		   end

  val comment = C.comment
end







(*
 * $Log: mips.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:47  george
 * Version 110.5
 *
 *)
