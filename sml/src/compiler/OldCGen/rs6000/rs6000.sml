(* Copyright (c) 1992 by AT&T Bell Laboratories 
 *
 *)

(* IBM RS6000 Cmachine implementation *)

functor RS6000CM (structure C : CODER
 	  	   where type 'a instruction = 'a RS6000InstrSet.instruction
		     and type 'a sdi = 'a RS6000InstrSet.sdi) : CMACHINE =
struct

  structure M = RS6000InstrSet
  structure P = CPS.P
  structure D = RS6000Spec.ObjDesc
  val dtoi = LargeWord.toInt	(* convert descriptor to int *)

  open M
 
  val bug 		= fn msg => ErrorMsg.impossible ("RS6kCM." ^ msg)

  val wtoi              = Word.toIntX

  type EA		= C.label M.EA
  exception BadReal 	= C.BadReal
  val align		= fn () => ()
  val mark 		= C.mark
  val emitlong 		= C.emitLong
  val realconst 	= C.emitReal
  val emitstring 	= C.emitString
  val newlabel 		= M.ImmedLab o C.newLabel
  val immed       	= M.Immed
  val immed32		= M.Immed32
  val emitSDI		= C.emitSDI
  val emit		= C.emit

  fun emitlab(k,ImmedLab lab) = C.emitLabel(lab,k)
    | emitlab _ = bug "emitlab"

  fun define(ImmedLab lab) = C.define lab
    | define _ = bug "RS6kCM.define"

  (** 
     Register Map
     Reg   gc   desc
     -------------------------------------
     0	   n   odd ball register
     1	   n   stack pointer	(not used in ML)
     2     n   TOC 		(not used in ML)
     3-13  y   miscregs
     14	   y   data pointer
     15	   n   heap limit 
     16	   y   store pointer
     17    y   standardlink
     18    y   standardclosure
     19    y   standardarg
     20    y   standardcont
     21    y   exception pointer
     22    y   varptr
     23    y   base pointer
     24-27 y   misc regs
     28    n   temporary (also gclink)
     29-31 n   temporaries
  **)

  val varptr_indexable	 	   = true
  val stackptr as Direct stackptr' : EA = Direct(M.stackReg)

  val dataptr  as Direct dataptr'  : EA = Direct(M.allocReg)
  val limitptr as Direct limitptr' : EA = Direct(M.limitReg)
  val storeptr as Direct storeptr' : EA = Direct(Reg 16)
  val standardlink		   = Direct(Reg 17)
  val standardclosure		   = Direct(Reg 18)
  val standardarg		   = Direct(Reg 19)
  val standardcont		   = Direct(Reg 20)
  val exnptr	  		   = Direct(M.exnptrReg)
  val varptr 			   = Direct(Reg 22)
  val miscregs : EA list	   = map (Direct o Reg) 
      					 [24,25,26,27,3,4,5,6,7,8,9,10,11,12,13]
  val gcLinkReg			   = Reg 28

  val floatregs: EA list	   = map (Direct o Freg)
      					 [1,2,3,4,5,6,7,8,9,10,11,12,13]
  val savedfpregs: EA list	   = map (Direct o Freg) 
					 [14,15,16,17,18,19,20,21,22,23,
					  24,25,26,27,28,29,30,31]
  val arithtemps: EA list	   = []
  val tmpFreg			   = Freg 0

  local
      exception NoTmpRegs
      val front		= ref 0
      val back		= ref 0
      val tmpRegs	= [M.maskReg,Reg 30,Reg 31]
      val qsize		= length tmpRegs + 1
      val queue 	= Array.array(qsize,~1)
      fun insert(Reg r) = Array.update(queue,!back,r) 
				  before back := (!back+1) mod qsize
	| insert _      = bug "insert"
      fun remove() 	= if !front = !back then raise NoTmpRegs
			  else Array.sub(queue,!front) 
				   before front := (!front+1) mod qsize
      val _ = app insert tmpRegs
  in  
      fun getTmpReg()    = Reg(remove())
      fun freeTmpReg reg = insert reg			        

     (* should be cleaned up in the future *)
      val tmpfpreg = (Freg 30)

  end

  fun emitBRANCH(cond,bool,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in 
	  emitSDI(M.BRANCH(cond,bool,lab,tmpR,flabel));
	  C.define flabel;
	  freeTmpReg tmpR
      end

  fun emitFBRANCH(cond,cr,bool,lab) = 
      let val flabel = C.newLabel()
	  val tmpR = getTmpReg()
      in
	  emitSDI(M.FBRANCH(cond,cr,bool,lab,tmpR,flabel)); 
	  C.define flabel;
	  freeTmpReg tmpR
      end

  datatype immedSize = IMMED16 | IMMED32

  fun immed_size n = if (~32768 <= n) andalso (n < 32768) then IMMED16
		     else IMMED32

  fun do_immed_signed(instr,rt,ra,si) = 
      case (immed_size si) 
	of IMMED16 => emit (instr(rt,ra,Immed16Op si))
         | IMMED32 => let
	       val (hi,lo) = M.split si
	       val tmpR = getTmpReg()
	   in
	       emit (M.LIU(tmpR, Immed16Op(wtoi hi)));
	       emit (M.A(tmpR,tmpR,Immed16Op(wtoi lo)));
	       emit (instr(rt,ra,RegOp tmpR));
	       freeTmpReg tmpR
           end

  fun load_immed(rt,n) = 
      case (immed_size n) 
        of IMMED16 => emit (M.CAL(rt,Reg 0,Immed16Op n))
         | IMMED32 => let
	       val (hi,lo) = M.split n
           in
	       emit (M.LIU(rt,Immed16Op(wtoi hi)));
	       emit (M.A(rt,rt,Immed16Op(wtoi lo)))
           end

  local
    structure W = Word32
  in
    fun load_immed32(rt,w) = let
	val lo' = W.andb(w, 0w65535)
	val hi' = W.~>>(w, 0w16)
	val (hi,lo) = if W.<(lo', 0w32768)
		then (hi',lo')
		else (W.+(hi', 0w1), W.-(lo', 0w65536))
      in
	if hi = 0w0 then emit(M.CAL(rt, Reg 0, Immed16Op(W.toIntX lo)))
	else (emit(M.LIU(rt, Immed16Op(W.toIntX hi)));
	      emit(M.A(rt, rt, Immed16Op(W.toIntX lo))))
      end
  end
	       
 (* move(a,b) means a -> b *)
  fun move (Direct(fp1 as Freg _),Direct(fp2 as Freg _)) = emit (M.FMR(fp2,fp1))
    | move (_, Direct(Freg _))        = bug "move: bad src"
    | move (Immed n, Direct dst)      = load_immed(dst,n)
    | move (Immed32 w, Direct dst)    = load_immed32(dst,w)
    | move (ImmedLab lab, Direct dst) = emitSDI(LOADADDR(dst,lab,0))
    | move (Direct src, Direct dst)   = if src = dst 
				        then ()
				        else emit (M.AND(dst,src,RegOp src))
    | move _ 			      = bug "move"

  fun compare_immed(cmp,ra,n) = 
      if   n >= ~32768 andalso n <= 32767 
      then emit (cmp(ra,Immed16Op n))
      else let val tmpR = getTmpReg()
	   in 
	       move(Immed n,Direct tmpR);
	       emit (cmp(ra,RegOp tmpR));
	       freeTmpReg tmpR
	   end

  fun jmp (Direct r)     = (emit (M.MTSPR(M.LR,r)); emit (M.BR()))
    | jmp (ImmedLab lab) = emit (B(Label24Off(M.POSLAB lab,0)))
    | jmp _		 = bug "jmp"

  (* stackptr' is the stack pointer; pregs_offset is the initial stack 
   * offset for pseudo registers, it should be consistent with the
   * offset in the RS6000.prim.asm file.
   *)
  val pregs_offset = 40
  
  fun loadpseudo (Direct x,Immed i) = 
        do_immed_signed(M.L, x, stackptr', 2*(i-1)+pregs_offset)
    | loadpseudo (Direct x,Direct y) =    (* this case is never used *)
        let val tmpR = getTmpReg()
         in emit(M.SL(tmpR, y, M.Int5Shift 1));
            emit(M.A(tmpR, stackptr', RegOp tmpR));
            do_immed_signed(M.L, x, tmpR, pregs_offset-2);
            freeTmpReg tmpR
        end
    | loadpseudo _ = bug "[loadpseudo]"

  fun storepseudo(Direct x,Immed i) = 
        do_immed_signed(M.ST, x, stackptr', 2*(i-1)+pregs_offset)
    | storepseudo(Direct x,Direct y) =    (* this case is never used *)
        let val tmpR = getTmpReg()
         in emit(M.SL(tmpR, y, M.Int5Shift 1));
            emit(M.A(tmpR, stackptr', RegOp tmpR));
            do_immed_signed(M.ST, x, tmpR, pregs_offset-2);
            freeTmpReg tmpR
        end
    | storepseudo _ = bug "[storepseudo]"

 (* jmpindexb(x,y) means pc <- x + y *)
  fun jmpindexb (ImmedLab lab,Direct y) = let
        val tmpR = getTmpReg()
      in
	  emitSDI(M.LOADADDR(tmpR,lab,0));
	  emit (M.A(tmpR,y,RegOp tmpR));
	  emit (M.MTSPR(M.LR,tmpR));
	  freeTmpReg tmpR;
	  emit (M.BR())
      end
    | jmpindexb _ = bug "jmpindexb"

  fun record(vl, Direct z) = let
        open CPS
	val len = List.length vl
	fun f(_,i,nil) = ()
	  | f((t1,t2),i,(Direct r, SELp(j,p))::rest) = 
	       (** follow ptrs to get the item  **)
	        (do_immed_signed(M.L,t1,r,j*4);
		 f((t2,t1),i,(Direct t1,p)::rest))
	  | f(t,i,(Direct r,OFFp 0)::rest) = 
	       (**  simple store, last first  **) 
	        (do_immed_signed(M.ST,r,dataptr',i*4);
		 f(t,i-1,rest))
	  | f((t1,t2),i,(Direct r, OFFp j)::rest) = 
               bug "unexpected non-zero OFFp record fields"
(*
	  | f((t1,t2),i,(Direct r, OFFp j)::rest) = 
		(emit (M.A(t1,r,Immed16Op(4*j))); 
		 f((t2,t1),i,(Direct t1,OFFp 0)::rest))
*)
	  | f((t1,t2),i,(ea,p)::rest) =
	       (* convert to register-based  *)
		(move(ea,Direct t1);  
		 f((t2,t1),i,(Direct t1,p)::rest))
	val tmpR1 = getTmpReg()
	val tmpR2 = getTmpReg()
      in 
       (* store first word in 0(dataptr') *)
	f((tmpR1,tmpR2),len-1,rev vl); 
	freeTmpReg tmpR1;
	freeTmpReg tmpR2;
	emit (M.A(z,dataptr',Immed16Op 4));
	do_immed_signed(M.A,dataptr',dataptr',4*len)
      end
    | record _ = bug "record"

  fun recordStore(x,y,_) = 
    let fun storeListUpdate r = (
		emit (M.ST(r,dataptr',Immed16Op 0));
		emit (M.ST(storeptr',dataptr',Immed16Op 4));
		emit (M.OR(storeptr',dataptr',Immed16Op 0));
		emit (M.A(dataptr',dataptr',Immed16Op 8)))
     in
            case (x, y)
             of (Direct r, Immed 1) => storeListUpdate r
	      | (Direct r, Immed i) => let val tmpR = getTmpReg()
	          in
	            do_immed_signed (M.A, tmpR, r, 2*(i-1));
	            storeListUpdate tmpR;
	            freeTmpReg tmpR
	          end
	      | (Direct r1, Direct r2) => let val tmpR = getTmpReg()
	          in
	            emit (M.A(tmpR, r2, Immed16Op ~1));
	            emit (M.A(tmpR, tmpR, RegOp tmpR));
	            emit (M.A(tmpR, tmpR, RegOp r1));
	            storeListUpdate tmpR;
	            freeTmpReg tmpR
	          end
	      | _ => ErrorMsg.impossible "[RS6000CM.recordStore]"
            (* end case *)
    end (* recordStore *)

  fun select (i,Direct v',Direct w)    = do_immed_signed(M.L,w,v',i*4)
    | select (i,ImmedLab lab,Direct w) = emitSDI(LOAD(w,lab,i*4))
    | select _ 			       = bug "select"

  fun offset (i,Direct v',Direct w)    = do_immed_signed(M.A,w,v',i*4)
    | offset (i,ImmedLab lab,Direct w) = let val tmpR = getTmpReg()
					 in
					     emitSDI(LOADADDR(tmpR,lab,0));
					     do_immed_signed(M.A,w,tmpR,i*4);
					     freeTmpReg tmpR
					 end
    | offset _ 			       = bug "offset"

  fun fetchindexb(Direct x,Direct y,Immed indx) = 
                                          do_immed_signed(M.LBZ,y,x,indx)
    | fetchindexb(Direct x,Direct y,Direct indx)= emit (M.LBZ(y,x,RegOp indx))
    | fetchindexb _ 				= bug "fetchindexb"

  fun storeindexb(Immed xi,y,z) = 
        let val tmpR = getTmpReg()
        in  load_immed(tmpR,xi);
  	    storeindexb(Direct tmpR,y,z);
  	    freeTmpReg tmpR
        end
    | storeindexb(Direct x,Direct y,Direct indx)= emit (M.STB(x,y,RegOp indx))
    | storeindexb(Direct x,Direct y,Immed indx) = 
                                         do_immed_signed(M.STB,x,y,indx)
    | storeindexb _ = bug "storeindexb"

  fun fetchindexl(x,Direct y,Direct z') = let
        val tmpR = getTmpReg()
      in
	  emit (M.SL(tmpR,z',M.Int5Shift 1));
	  (case x 
	     of Direct x'    => ( emit (M.A(tmpR,x',RegOp tmpR));
			          emit (M.L(y,tmpR,Immed16Op ~2)))
	      | Immed n      => do_immed_signed(M.L,y,tmpR,n-2)
	      | ImmedLab lab => 
		   let val tmpR2 = getTmpReg()
		   in
		       emitSDI(M.LOADADDR(tmpR2,lab,0));
		       emit (M.A(tmpR,tmpR,RegOp tmpR2));
		       freeTmpReg tmpR2;
		       emit (M.L(y,tmpR,Immed16Op ~2))
		   end);
	  freeTmpReg tmpR
      end
    | fetchindexl(x,Direct y,Immed z') =  
      (case x
	 of Direct x'    => do_immed_signed(M.L,y,x',2*(z'-1))
	  | Immed n      => do_immed_signed(M.L,y,Reg 0,n+2*(z'-1))
	  | ImmedLab lab => emitSDI(LOAD(y,lab,2*(z'-1))))
    | fetchindexl _ = bug "fetchindexl"

  fun storeindexl(Direct x,Direct y,Direct z) = let
        val tmpR = getTmpReg()
      in 
        emit (M.SL(tmpR,z,Int5Shift 1));
	emit (M.A(tmpR,tmpR,RegOp y));
	emit (M.ST(x,tmpR,Immed16Op ~2));
	freeTmpReg tmpR
      end
    | storeindexl(Direct x,Direct y,Immed zi) = 
        do_immed_signed(M.ST,x,y,2*(zi-1))
    | storeindexl(Immed xi,y,z) =  let val tmpR = getTmpReg()
				   in
				       move(Immed xi,Direct tmpR);
				       storeindexl(Direct tmpR,y,z);
				       freeTmpReg tmpR
				   end
    | storeindexl(ImmedLab lab,y,z) = let val tmpR = getTmpReg()
				      in
					  emitSDI(M.LOADADDR(tmpR,lab,0));
					  storeindexl(Direct tmpR,y,z);
					  freeTmpReg tmpR
				      end
    | storeindexl(Direct x,ImmedLab lab,Immed zi) = let
        val tmpR = getTmpReg()
      in
	  emitSDI(M.LOADADDR(tmpR,lab,0));  
	  do_immed_signed(M.ST,x,tmpR,2*(zi-1));
	  freeTmpReg tmpR
      end
    | storeindexl _ = bug "storeindexl: bad args"

  local
    fun three f (Direct x',Direct y',Immed zi)     = do_immed_signed(f,x',y',zi)
      | three f (x as Direct _, y as Direct _,Immed32 w) = let 
           val tmpR = getTmpReg()
	in load_immed32(tmpR,w);
	   three f (x,y,Direct tmpR);
	   freeTmpReg tmpR
        end
      | three f (x, y as Immed yi, z as Immed _)   = let
	  val tmpR = getTmpReg()
	in
	  load_immed(tmpR, yi);
	  three f (x, Direct tmpR, z);
	  freeTmpReg tmpR
	end
      | three f (x, y as Immed32 yi, z as Immed32 _)   = let
	  val tmpR = getTmpReg()
	in
	  load_immed32(tmpR, yi);
	  three f (x, Direct tmpR, z);
	  freeTmpReg tmpR
	end
      | three f (x, y as Immed32 yi, z as Immed zi)   = let
	  val tmpR = getTmpReg()
	in
	  load_immed32(tmpR, yi);
	  three f (x, Direct tmpR, z);
	  freeTmpReg tmpR
	end
      | three f (x, y as Immed yi, z as Immed32 zi)   = let
	  val tmpR = getTmpReg()
	in
	  load_immed32(tmpR, zi);
	  three f (x, z, Direct tmpR);
	  freeTmpReg tmpR
	end
      | three f (x, y as Immed _, z) = three f (x, z, y)
      | three f (x, y as Immed32 _, z) = three f (x, z, y)
      | three f (Direct x',Direct y',Direct z')    = emit (f(x',y',RegOp z'))
      | three f (Direct x',Direct y',ImmedLab lab) = let
	  val tmpR = getTmpReg()
	in
	    emitSDI(M.LOADADDR(tmpR,lab,0));  
	    emit (f(x',y',RegOp tmpR));
	    freeTmpReg tmpR
	end
      | three f (Direct x,ea,Direct z) = three f (Direct x,Direct z,ea)
      | three _ _ 		       = bug "three: bad args"
  in
    fun add(x,y,z) 		= three M.A   (z,x,y)
    fun orb(x,y,z) 		= three M.OR  (z,x,y) 
    fun andb(x,y,z)		= three M.AND (z,x,y)
    fun xorb(x,y,z)		= three M.XOR (z,x,y)
  end

  fun fprecord(tag, vl, Direct dst) = 
        let open CPS
	    val len = (List.length vl) * 8 + 4
	    fun f(_,_,_,i,nil) = ()
	      | f(t1,t2,f1 as (Freg fpr),i,(Direct r, SELp(j,OFFp 0))::rest) = 
                   (do_immed_signed(M.L,t1,r,j*8);
	            emit(M.ST(t1,stackptr',Immed16Op M.fLoadStoreOff));
	            do_immed_signed(M.L,t1,r,j*8+4);
	            emit(M.ST(t1,stackptr',Immed16Op(M.fLoadStoreOff+4)));
	            emit(M.LFD(Freg fpr,stackptr',Immed16Op M.fLoadStoreOff));
                    f(t1,t2,f1,i,(Direct f1,OFFp 0)::rest))

              | f(t1,t2,f1,i,(Direct r, SELp(j,p))::rest) = 
                   (do_immed_signed(M.L,t1,r,j*4);
                    f(t2,t1,f1,i,(Direct t1,p)::rest))

	      | f(t1,t2,f1,i,(Direct (Freg fpr), OFFp 0)::rest) = 
       	           (emit(M.STFD(Freg fpr,stackptr',Immed16Op M.fLoadStoreOff));
	            emit(M.L(t1,stackptr',Immed16Op M.fLoadStoreOff));
	            do_immed_signed(M.ST,t1,dataptr',i);
	            emit(M.L(t1,stackptr',Immed16Op(M.fLoadStoreOff+4)));
	            do_immed_signed(M.ST,t1,dataptr',i+4);
		    f(t1,t2,f1,i-8,rest))

	      | f(t1,t2,f1,i,(Direct _, OFFp _)::rest) =
                   bug "wrong-type in fprecord in rs6000.sml"
	      
	      | f(t1,t2,f1,i,(ea, p)::rest) =
                   (move (ea, Direct t1);
                    f(t2,t1,f1,i,(Direct t1,p)::rest))

	    val tmpR1 = getTmpReg()
    	    val tmpR2 = getTmpReg()
            val tmpF1 = tmpfpreg
         in 
            orb(dataptr,Immed 4,dataptr); (* align *)
	    move(tag,Direct tmpR1);
            do_immed_signed(M.ST,tmpR1,dataptr',0);
	    f(tmpR1, tmpR2, tmpF1, len-8, rev vl);
            do_immed_signed(M.A,dst,dataptr',4);
            freeTmpReg tmpR1;
            freeTmpReg tmpR2;
	    do_immed_signed(M.A,dataptr',dataptr',len)
        end
    | fprecord _ = bug "[SparcCM.fprecord]"

  fun recordcont _ = bug "record_cont not implemented yet"

  val startgc_offset = RS6000Spec.startgcOffset

  fun testLimit() = emit (M.CMPL(limitptr',dataptr'))

  fun decLimit n = (* for polling *)
      (* note: M.S doesn't work here because of overflow I think -lfh *)
      do_immed_signed(M.A,limitptr',limitptr',~n) 

  fun beginStdFn (ImmedLab lab,Direct reg) = emitSDI(M.SETBASEADDR(lab,reg))
    | beginStdFn _  			   = bug "beginStdFn"

  fun checkLimit(max_allocation, restart, mask, rlab, fregs) = 
    let val lab = C.newLabel()
	val tmpR = getTmpReg()
        val _ = if max_allocation > 4096 
                then (do_immed_signed(M.A,tmpR,dataptr',max_allocation-4096);
	              emit (M.CMPL(limitptr',tmpR)))
	        else ()
	val _ = emitBRANCH(M.GT,true,lab)
     in (case fregs
          of [] => (emit (M.L(tmpR,stackptr',Immed16Op startgc_offset));
	            emit (M.MTSPR(M.LR,tmpR));
	            freeTmpReg tmpR;
	            move(mask, Direct M.maskReg);
	            move(restart, Direct gcLinkReg);
	            emit (M.BR()))
           | _ => (let val k = length fregs
                       val desc = dtoi(D.makeDesc(k * 8, D.tag_string))
                       val retlab = C.newLabel()

                       (* cps/limit.sml makes sure that there is enough
                          space left to save these floating 
                          point registers *)
                       fun deposit([], _) = ()
                         | deposit((Direct(Freg fpr))::r, i) = 
                             (emit(M.STFD(Freg fpr,stackptr',
                                          Immed16Op M.fLoadStoreOff));
	                      emit(M.L(tmpR,stackptr',
                                       Immed16Op M.fLoadStoreOff));
	                      do_immed_signed(M.ST,tmpR,dataptr',i);
	                      emit(M.L(tmpR,stackptr',
                                       Immed16Op(M.fLoadStoreOff+4)));
	                      do_immed_signed(M.ST,tmpR,dataptr',i+4);
		              deposit(r,i+8))

                       fun restore(s, [], _) = ()
                         | restore(s, (Direct(Freg fpr))::r, i) = 
                             (do_immed_signed(M.L,tmpR,s,i);
	                      emit(M.ST(tmpR,stackptr',
                                        Immed16Op M.fLoadStoreOff));
	                      do_immed_signed(M.L,tmpR,s,i+4);
	                      emit(M.ST(tmpR,stackptr',
                                        Immed16Op(M.fLoadStoreOff+4)));
	                      emit(M.LFD(Freg fpr,stackptr',
                                         Immed16Op M.fLoadStoreOff));
                              restore(s, r, i+8))

                    in deposit(fregs,4);
                       move(immed desc, Direct tmpR);
                       (* orb(dataptr,Immed 4,dataptr); *) (* align *)
                       do_immed_signed(M.ST,tmpR,dataptr',0);
                       do_immed_signed(M.A,M.maskReg,dataptr',4);
      	               do_immed_signed(M.A,dataptr',dataptr',k*8+4);
                       do_immed_signed(M.ST,M.maskReg,stackptr',
                                       4+pregs_offset);

                       emit (M.L(tmpR,stackptr',Immed16Op startgc_offset));
	               emit (M.MTSPR(M.LR,tmpR));
	               move(mask, Direct M.maskReg);
	               move(ImmedLab retlab, Direct gcLinkReg);
	               emit (M.BR());

                       C.define retlab;
		       let val tmpR2 = getTmpReg()
		       in 
			  do_immed_signed(M.L,tmpR2,stackptr',4+pregs_offset);
			  restore(tmpR2, fregs, 0);
			  freeTmpReg tmpR2
		       end;
                       move (rlab, Direct gcLinkReg);
                       emit (M.MTSPR(M.LR,gcLinkReg));
	               freeTmpReg tmpR;
		       testLimit();
                       emit (M.BR())
                   end));
	 C.define lab
     end	 

  fun trapOnOverflow () = let val lab = C.newLabel()
			  in 
			      emitBRANCH(M.SO,false,lab);
			      emit(M.TRAP());
			      C.define lab
			  end
  fun trapOnDivZero () = let val lab = C.newLabel()
			 in 
			     emitBRANCH(M.SO,false,lab);
			     emit(MTFSB1 5);
			     emit(M.TRAP());
			     C.define lab
			 end
  local 
    fun move2reg (Direct(Reg r)) = (Reg r,NONE)
      | move2reg (Immed n)       = let val tmpR = getTmpReg()
				   in 
				       move(Immed n, Direct tmpR);
				       (tmpR,SOME tmpR)
				   end
      | move2reg (ImmedLab lab)  = let val tmpR = getTmpReg()
				   in 
				       move(ImmedLab lab, Direct tmpR);
				       (tmpR, SOME tmpR)
				   end
      | move2reg (Immed32 w)     = let val tmpR = getTmpReg()
				   in
				     move(Immed32 w, Direct tmpR);
				     (tmpR, SOME tmpR)
				   end
      | move2reg _ 		 = bug "move2reg"

    fun free NONE = () 
      | free (SOME r) = freeTmpReg r
  in
    fun addt(x,y,Direct z) = let val (x',tmpx) = move2reg x
				 val (y',tmpy) = move2reg y
			     in 
				 emit (M.AO(z,x',y'));
				 trapOnOverflow();
				 free tmpx; 
				 free tmpy
			     end
      | addt _ 		   = bug "addt"
				      
    fun mult(x,Direct y) = let val (x',tmpx) = move2reg x
			   in
			       emit (MULSO(y,x',y));
			       trapOnOverflow();
			       free tmpx
			   end
      | mult _ 		 = bug "mult"
  end

  fun sub (Direct x,Direct y,Direct z) = emit (M.SF(z,x,RegOp y))
    | sub (Direct x,Immed yi,Direct z) = do_immed_signed(M.SF,z,x,yi)
    | sub (x as Direct _, Immed32 w, z as Direct _) = let val tmpR = getTmpReg()
      in load_immed32(tmpR,w);
	 sub(x,Direct tmpR, z);
	 freeTmpReg tmpR
      end
    | sub (Immed32 w, y, z) = let val tmpR = getTmpReg()
      in load_immed32(tmpR, w);
	 sub(Direct tmpR, y, z);
	 freeTmpReg tmpR
      end
    | sub (Immed xi,y,z) = let  val tmpR = getTmpReg()
      in
	move(Immed xi,Direct tmpR);
	sub(Direct tmpR,y,z);
	freeTmpReg tmpR
      end
    | sub _			       = bug "sub"

  fun notb(a,b)	= sub(a, Immed ~1, b)

  local 
      fun subtract(Direct x,Direct y,Direct z) = emit (SFO(z,x,y))
	| subtract(Immed xi,y,z)  = let val tmpR = getTmpReg()
				    in
					move(Immed xi,Direct tmpR);
					subtract(Direct tmpR,y,z);
					freeTmpReg tmpR
				    end
	| subtract(x,Immed yi,z)  = let val tmpR = getTmpReg()
				    in 
					move(Immed yi,Direct tmpR);
					subtract(x,Direct tmpR,z);
					freeTmpReg tmpR
				    end
	| subtract(Immed32 xi,y,z) = let val tmpR = getTmpReg()
				    in
					move(Immed32 xi,Direct tmpR);
					subtract(Direct tmpR,y,z);
					freeTmpReg tmpR
				    end
	| subtract(x,Immed32 yi,z) = let val tmpR = getTmpReg()
				    in 
					move(Immed32 yi,Direct tmpR);
					subtract(x,Direct tmpR,z);
					freeTmpReg tmpR
				    end

	| subtract _ 	          = bug "subtract"
  in
      fun subt arg = (subtract arg; trapOnOverflow())
  end

			(* divt(a,b) means b <- b / a *)
  local
    fun divide (Direct x,Direct y) = emit (M.DIVS(y,y,x))
      | divide (xi as Immed _, y) = let
          val tmpR = getTmpReg()
        in
	  move(xi, Direct tmpR);
	  divide(Direct tmpR, y);
	  freeTmpReg tmpR
	end
      | divide (x, yi as Immed _) = let
          val tmpR = getTmpReg()
        in
	  move(yi, Direct tmpR);
	  divide(x, Direct tmpR);
	  freeTmpReg tmpR
	end
      | divide (xi as Immed32 _, y) = let
          val tmpR = getTmpReg()
        in
	  move(xi, Direct tmpR);
	  divide(Direct tmpR, y);
	  freeTmpReg tmpR
	end
      | divide (x, yi as Immed32 _) = let
          val tmpR = getTmpReg()
        in
	  move(yi, Direct tmpR);
	  divide(x, Direct tmpR);
	  freeTmpReg tmpR
	end
      | divide _ = bug "divide"
  in
      fun divt arg = (divide arg; trapOnDivZero())
  end
				      
  fun ashl (Direct rs,Direct rt,Direct rd) = emit (M.SL(rd,rt,RegShift rs))
    | ashl (Immed n,Direct rt,Direct rd) = 
      if n >= 32 orelse n < 0 then
	  bug "ashl: shift distance"
      else
	  emit (M.SL(rd,rt,Int5Shift n))
    | ashl(shamt,Immed n,dst) = let 
        val tmpR = getTmpReg()
      in  
	  move(Immed n, Direct tmpR);
	  ashl(shamt,Direct tmpR,dst);
	  freeTmpReg tmpR
      end
    | ashl(shamt,Immed32 w,dst) = let 
        val tmpR = getTmpReg()
      in
	load_immed32(tmpR, w);
	ashl(shamt,Direct tmpR, dst);
	freeTmpReg tmpR
      end
    | ashl _ = bug "ashl"

  fun ashr (Direct rs,Direct rt,Direct rd) = 
      emit (M.SRA(rd,rt,RegShift rs))
    | ashr (Immed n,Direct rt,Direct rd) = 
      if n >= 32 orelse n < 0 then
	  bug "ashr: shift distance"
      else
	  emit (M.SRA(rd,rt,Int5Shift n))
    | ashr(shamt,Immed n,dst) = let
        val tmpR = getTmpReg()
      in  
	  move(Immed n,Direct tmpR);
	  ashr(shamt,Direct tmpR,dst);
	  freeTmpReg tmpR
      end
    | ashr(shamt,Immed32 w, dst) = let 
        val tmpR = getTmpReg()
      in 
	load_immed32(tmpR, w);
	ashr(shamt,Direct tmpR,dst);
	freeTmpReg tmpR
      end
    | ashr _ = bug "ashr: bad args"

  fun lshr(Direct rs,Direct rt,Direct rd) = emit(M.SRL(rd,rt,RegShift rs))
    | lshr(Immed n,Direct rt,Direct rd) = 
      if n >= 32 orelse n < 0 then
	  bug "lshr: shift distance"
      else
	  emit (M.SRL(rd,rt,Int5Shift n))
    | lshr(shamt,Immed n,dst) = let val tmpR = getTmpReg()
      in
	load_immed(tmpR,n);
	lshr(shamt,Direct tmpR,dst);
	freeTmpReg tmpR
      end
    | lshr(shamt,Immed32 w,dst) = let
        val tmpR = getTmpReg()
      in 
	load_immed32(tmpR, w);
	lshr(shamt,Direct tmpR,dst);
	freeTmpReg tmpR
      end
    | lshr _ = bug "lshr"

  fun mulu(Direct x,Direct y) = emit(M.MULS(y,x,y))
    | mulu(Immed32 xi,y) = let val tmpR = getTmpReg()
      in
	load_immed32(tmpR,xi);
        mulu(Direct tmpR,y);
	freeTmpReg tmpR
      end
    | mulu _ = bug "mulu"

  (* divtu(a,b) = b <- b / a *)
  fun divtu(Direct ra,Direct rb) = let
	fun trap() = (emit(M.MTFSB1 5);	emit(M.TRAP()))
	val doneL  = C.newLabel()	val doitL    = C.newLabel()
	val tmpR   = getTmpReg()
      in
        emit(M.CAL(tmpR,Reg 0,M.Immed16Op 0));
        emit(M.CMPL(rb,ra));
        emitBRANCH(M.LT,true,doneL);
	emit(M.CAL(tmpR,Reg 0,M.Immed16Op 1));
	emit(M.CMP(ra,M.Immed16Op 0));
	emitBRANCH(M.LT,true,doneL);
	emitBRANCH(M.EQ,false,doitL);
	trap();
	C.define doitL;
	emit(M.CAL(Reg 0,Reg 0,M.Immed16Op 0));
	emit(M.MTSPR(M.MQ,rb));
	emit(M.DIV(tmpR,Reg 0,ra));

        C.define doneL;
        emit(M.CAL(rb,tmpR,M.Immed16Op 0));
        freeTmpReg tmpR
      end
    | divtu(x as Immed32 _,y) = let val tmpR = getTmpReg()
      in
        move(x,Direct tmpR);
	divtu(Direct tmpR,y);
	freeTmpReg tmpR
      end
    | divtu _ = bug "divtu"

  local 
    fun floatreg (Direct(fpr as Freg _)) = fpr
      | floatreg _ 			 = bug "floatreg"

    fun floating_arith f (x,y,z) = let 
          val lab = C.newLabel()
	in
	    emit (f(floatreg x,floatreg y,floatreg z))
(*
	    emitFBRANCH(M.FEX,1,false,lab);
	    emit(M.TRAP());
	    C.define lab
*)
        end

    val real_tag = dtoi D.desc_reald

    fun store_float(Freg fp,Direct dst,offset) = let
	  val tmpR = getTmpReg()
        in
	    emit(M.STFD(Freg fp,stackptr',Immed16Op M.fLoadStoreOff));
	    emit(M.L(tmpR,stackptr',Immed16Op M.fLoadStoreOff));
	    do_immed_signed(M.ST,tmpR,dst,offset);
	    emit(M.L(tmpR,stackptr',Immed16Op(M.fLoadStoreOff+4)));
	    do_immed_signed(M.ST,tmpR,dst,offset+4);
	    freeTmpReg tmpR
	end
      | store_float _ = bug "store_float"	    

    fun load_float (Freg dst,Direct src,offset) = let
	  val tmpR = getTmpReg()
        in
	    do_immed_signed(M.L,tmpR,src,offset);
	    emit(M.ST(tmpR,stackptr',Immed16Op M.fLoadStoreOff));
	    do_immed_signed(M.L,tmpR,src,offset+4);
	    emit(M.ST(tmpR,stackptr',Immed16Op(M.fLoadStoreOff+4)));
	    emit(M.LFD(Freg dst,stackptr',Immed16Op M.fLoadStoreOff));
	    freeTmpReg tmpR
	    
        end
      | load_float (Freg dst,ImmedLab lab,offset) = let
 	  val tmpR = getTmpReg()
	in
	    emitSDI(LOADF(Freg dst,lab,offset,tmpR));
	    freeTmpReg tmpR
	end
      | load_float _ = bug "load_float"
  in
      fun fmuld(x,y,z) 	    = floating_arith M.FMO (z,x,y)
      fun fdivd(x,y,z) 	    = floating_arith M.FDO (z,x,y)
      fun faddd(x,y,z) 	    = floating_arith M.FAO (z,x,y)
      fun fsubd(x,y,z) 	    = floating_arith M.FSO (z,x,y)
      fun fnegd(op1,result) = emit (M.FNEG(floatreg result,floatreg op1))
      fun fabsd(op1,result) = emit (M.FABS(floatreg result,floatreg op1))

      fun storefloat(src,Direct(Reg result)) =
	    (store_float(floatreg src,dataptr,4);
	     let val tmpR = getTmpReg()
	     in
		 load_immed(tmpR,real_tag);
		 emit (M.ST(tmpR,dataptr',Immed16Op 0));
		 emit (M.A(Reg result,dataptr',Immed16Op 4));
		 emit (M.A(dataptr',dataptr',Immed16Op 12));
		 freeTmpReg tmpR
	     end)
	| storefloat  _ = bug "storefloat"
  
      fun loadfloat(src, dst) = load_float(floatreg dst,src,0)

     (* fetchindexd (x,y,z) y <- mem[x+4*(z-1)] *)
      fun fetchindexd (Direct x,y,Immed i) = 
	    load_float(floatreg y, Direct x, 4*(i-1))
	| fetchindexd (Direct x,y,Direct z) = let
	    val tmpR = getTmpReg()
  	  in
	    emit (M.SL(tmpR,z,Int5Shift 2));
	    emit (M.A(tmpR,x,RegOp tmpR));
	    load_float(floatreg y,Direct tmpR,~4);
	    freeTmpReg tmpR
	  end
	| fetchindexd _ = bug "fetchindexd"

    fun storeindexd (x,Direct y,Immed i) = 
	  store_float(floatreg x,Direct y, 4*(i-1))
      | storeindexd (x,Direct y,Direct z) = let
          val tmpR = getTmpReg()
	in
	    emit (M.SL(tmpR,z,Int5Shift 2));
	    emit (M.A(tmpR,y,RegOp tmpR));
	    store_float(floatreg x,Direct tmpR,~4);
	    freeTmpReg tmpR
    	end
      | storeindexd _ = bug "storeindexd"		
  end

  datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR 
    		     | GEU | GTU | LTU | LEU

  fun ibranch(cond,Immed a,Immed b,ImmedLab lab) = let
	fun wCmp cmp (a, b) = cmp(Word31.fromInt a, Word31.fromInt b)
	in
	  if (case cond
	       of EQL => a=b  | NEQ => a<>b | LSS => a<b
		| LEQ => a<=b | GTR => a>b  | GEQ => a>=b
		| GEU => wCmp Word31.>= (a, b) | GTU => wCmp Word31.> (a, b)
		| LEU => wCmp Word31.<= (a, b) | LTU => wCmp Word31.< (a, b)
	      (* end case *))
	    then emit (M.B(Label24Off(POSLAB lab,0)))
	    else ()
	end
    | ibranch(cond,Immed32 a,Immed32 b,ImmedLab lab) = let
        fun wtoi w = Int32.fromLarge(Word32.toLargeIntX w)
        fun cmpi32(cmp, a, b) = cmp(wtoi a, wtoi b)
      in
        if (case cond 
	   of EQL => a = b
	    | NEQ => a <> b
	    | GEU => Word32.>=(a,b)
	    | GTU => Word32.>(a,b)
	    | LEU => Word32.<=(a,b)
	    | LTU => Word32.<(a,b)
	    | LEQ => cmpi32(Int32.<=, a, b)
	    | LSS => cmpi32(Int32.<, a, b)
	    | GEQ => cmpi32(Int32.>=, a, b)
	    | GTR => cmpi32(Int32.>, a, b))
        then emit(M.B(Label24Off(POSLAB lab,0)))
        else ()
      end
    | ibranch(cond,rs as Direct _,Immed n,lab) = let
	val tmpR as Direct tmpR' = Direct(getTmpReg())
      in
	move(Immed n,tmpR);
	ibranch(cond,rs,tmpR,lab);
	freeTmpReg tmpR'
      end
    | ibranch(cond,rs,Immed32 w,lab) = let
        val tmpR = getTmpReg()
      in move(Immed32 w, Direct tmpR);
	 ibranch(cond, rs, Direct tmpR, lab);
	 freeTmpReg tmpR
      end
    | ibranch(cond,Immed n,rb,lab) = let
	val tmpR as Direct tmpR' = Direct(getTmpReg())
      in
	 move(Immed n,tmpR);
	 ibranch(cond,tmpR,rb,lab);
	 freeTmpReg tmpR'
      end
    | ibranch(cond,Immed32 w,rb,lab) = let
	val tmpR as Direct tmpR' = Direct(getTmpReg())
      in
	 move(Immed32 w,tmpR);
	 ibranch(cond,tmpR,rb,lab);
	 freeTmpReg tmpR'
      end
    | ibranch(NEQ,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMP(ra,RegOp rb)); emitBRANCH(M.EQ,false,lab))
    | ibranch(EQL,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMP(ra,RegOp rb)); emitBRANCH(M.EQ,true,lab))
    | ibranch(GTR,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMP(ra,RegOp rb)); emitBRANCH(M.GT,true,lab))
    | ibranch(LEQ,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMP(ra,RegOp rb)); emitBRANCH(M.GT,false,lab))
    | ibranch(LSS,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMP(ra,RegOp rb)); emitBRANCH(M.LT,true,lab))
    | ibranch(GEQ,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMP(ra,RegOp rb)); emitBRANCH(M.LT,false,lab))
    | ibranch(GEU,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMPL(ra,rb)); emitBRANCH(M.LT,false,lab))
    | ibranch(GTU,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMPL(ra,rb)); emitBRANCH(M.GT,true,lab))
    | ibranch(LEU,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMPL(ra,rb)); emitBRANCH(M.GT,false,lab))
    | ibranch(LTU,Direct ra,Direct rb,ImmedLab lab) = 
	(emit(M.CMPL(ra,rb)); emitBRANCH(M.LT,true,lab))
    | ibranch _ = bug "ibranch"

  fun fbranchd(cond,Direct fra,Direct frb,ImmedLab lab) = let
        fun test2bits(bit1, bit2) =
	  (emit(M.CROR(M.FL, bit1, bit2)); 
	   emitFBRANCH(M.FL, 2, true, lab))
      in
        (emit (M.FCMP(fra,frb));
	 case cond 
	 of P.fEQ  => emitFBRANCH(M.FE, 2, true, lab)
	  | P.fULG => emitFBRANCH(M.FE, 2, false, lab)
	  | P.fUN  => emitFBRANCH(M.UN, 2, true, lab)
	  | P.fLEG => emitFBRANCH(M.UN, 2, false, lab)
	  | P.fGT  => emitFBRANCH(M.FG, 2, true, lab)
	  | P.fGE  => test2bits(M.FG, M.FE)
	  | P.fUGT => test2bits(M.FG, M.UN)
	  | P.fUGE => emitFBRANCH(M.FL, 2, false, lab)
	  | P.fLT  => emitFBRANCH(M.FL, 2, true, lab)
	  | P.fLE  => test2bits(M.FL, M.FE)
	  | P.fULT => test2bits(M.UN, M.FL)
	  | P.fULE => emitFBRANCH(M.FG, 2, false, lab)
	  | P.fLG  => test2bits(M.FL, M.FG)
	  | P.fUE  => test2bits(M.UN, M.FE)
         (*esac*))
      end
    | fbranchd _ = bug "fbranch"

 (* Should implement ANDcc and do this better *)
  fun bbs(Immed k,Direct y,ImmedLab label) =
      let val tmpR = getTmpReg()
	  val k2 = Word.toInt (Word.<<(0w1, Word.fromInt k))
      in
	  do_immed_signed(M.AND,tmpR, y, k2);
	  compare_immed(M.CMP, tmpR, k2);
	  freeTmpReg tmpR;
	  emitBRANCH(M.EQ,true,label)
      end
    | bbs _ = bug "bbs: bad args"


  val cvti2dTmpOffset   = 16
  val cvti2dConstOffset = 8
  fun cvti2d(Direct(src as Reg _),Direct(dst as Freg _)) = let
      val tmpR = getTmpReg()
    in
	emit(M.XORU(tmpR,src,Immed16Op 32768));
	emit(M.ST(tmpR,stackptr',Immed16Op(cvti2dTmpOffset+4)));
	emit(M.LIU(tmpR,Immed16Op 17200));
	emit(M.ST(tmpR,stackptr',Immed16Op cvti2dTmpOffset));
	emit(M.LFD(dst,stackptr',Immed16Op cvti2dTmpOffset));
        emit(M.LFD(tmpFreg,stackptr',Immed16Op cvti2dConstOffset));
	emit(M.FSO(dst,dst,tmpFreg));
	freeTmpReg tmpR
    end

  val comment = C.comment
end

(*
 * $Log: rs6000.sml,v $
 * Revision 1.2  1998/05/20 18:35:53  george
 *     Turn off the capability of processing non-zero OFFSET objects  -- zsh
 *
 * Revision 1.1.1.1  1998/04/08 18:39:49  george
 * Version 110.5
 *
 *)
