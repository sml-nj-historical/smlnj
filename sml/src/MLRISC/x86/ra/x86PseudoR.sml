signature X86REWRITE_PSEUDO = sig
  structure F : FLOWGRAPH
  val rewrite : int -> F.cluster -> int * int
    (* Takes a cluster and returns a list of registers to prohibit 
     * from spilling 
     *)
end


functor X86RewritePseudo
  (structure Instr : X86INSTR
   structure Flowgraph : FLOWGRAPH where I = Instr
   val ea : int -> Instr.operand) : X86REWRITE_PSEUDO =
struct
  structure C = X86Cells
  structure I = Instr
  structure F = Flowgraph

  fun error msg = MLRiscErrorMsg.error("X86RewritePseudo",msg)

  fun rewrite firstPseudo (F.CLUSTER{blocks, regmap, ...}) = let
    val first = C.newReg()
    datatype temp_use = OK of I.instruction | NO of I.instruction
    fun shuffle(dests, srcs, tmp)  = let
      fun move(rd,rs) = I.MOVE{mvOp=I.MOVL, src=rs, dst=rd}
      fun lookup r = 
	(Intmap.map regmap r handle _ => 
           error ("rewrite.subst.lookup "^Int.toString r))
      fun loop((p as (rd, dst, rs, src))::rest, changed, used, done, instrs) = 
	  if List.exists (fn r => dst=r) used then
	    loop(rest, changed, used, p::done, instrs)
	  else 
	    loop(rest, true, used, done, move(I.Direct rd, I.Direct rs)::instrs)
	| loop([], false, _, done, instrs) = (done, instrs)
	| loop([], true, _, done, instrs) = 
	    loop(done, false, map #4 done, [], instrs)

      fun cycle([], instrs) = instrs
	| cycle(moves, instrs) = 
	  (case loop(moves, false, map #4 moves, [], instrs) 
	   of ([], instrs) => instrs
	    | ((rd, rd', rs, rs')::nonCyclic, instrs) => let
		val instrs' = move(Option.valOf tmp, I.Direct rs)::instrs
		val (cyclic, instrs'') = 
		  loop(nonCyclic, false, map #4 nonCyclic, [], instrs')
	      in cycle(cyclic, move(I.Direct rd, Option.valOf tmp)::instrs'')
	      end
	  (*esac*))
      fun rmCoalesced([], [], remain, coalesced) = (remain, coalesced)
	| rmCoalesced(rd::rds, rs::rss, remain, coalesced) = let
	    val dst = lookup rd
	    val src = lookup rs
	  in
	    if dst = src then 
	      rmCoalesced(rds, rss, remain, move(I.Direct rd, I.Direct rs)::coalesced)
	    else rmCoalesced(rds, rss, (rd, dst, rs, src)::remain, coalesced)
	  end
    in rev (cycle (rmCoalesced(dests, srcs, [], [])))
    end

    fun doBlock(F.BBLOCK{blknum, insns, liveOut, succ, ...}) = let
	  fun pseudoR r =  (r >= 8 andalso r < firstPseudo)
	  fun resetLiveOut() = let
	    fun reset(gp, fp, cc) = 
	      liveOut := (List.filter (not o pseudoR) gp, fp, cc)
	  in
	    case !succ
	     of [] => reset(!liveOut)
	      | [(F.EXIT _,_)] => reset(!liveOut)
	      | _ => ()
	  end

	  (* subst: hd(acc) is the last instruction in the stream. *)
	  fun subst(instr, acc) = let
	    fun movl{src, dst, acc} = 
	      I.MOVE{mvOp=I.MOVL, src=src, dst=dst}::acc

	    fun displace(base, disp, acc, mem) = 
	      if pseudoR base then let
		  val tmpR = C.newReg()
		  val newDisp = I.Displace{base=tmpR, disp=disp, mem=mem}
		in (newDisp, movl{src=ea base, dst=I.Direct tmpR, acc=acc})
		end
	      else (I.Displace{base=base, disp=disp, mem=mem}, acc)

	    fun indexedEa(base, index, scale, disp, mem) = 
	      I.Indexed{base=base, index=index, scale=scale, disp=disp, mem=mem}

	    fun indexed(NONE, index, scale, disp, acc, mem) = 
		if pseudoR index then let
		    val tmpR = C.newReg()
		    val newIndx = indexedEa(NONE, tmpR, scale, disp, mem)
		  in (newIndx, movl{src=ea index, dst=I.Direct tmpR, acc=acc})
		  end
		else (indexedEa(NONE, index, scale, disp, mem), acc)
	      | indexed(ba as SOME base, index, scale, disp, acc, mem) = let
		  val b = pseudoR base
		  val i = pseudoR index
		in 
		  if b andalso i then let
		      val tmpB = C.newReg()
		      val tmpI = C.newReg()
		      val opnd = indexedEa(SOME tmpB, tmpI, scale, disp, mem)
		    in (opnd, movl{src=ea base, dst=I.Direct tmpB, 
				   acc=movl{src=ea index, dst=I.Direct tmpI, acc=acc}})
		    end
		  else if b then let
		      val tmpB = C.newReg()
		    in (indexedEa(SOME tmpB, index, scale, disp, mem), 
			movl{src=ea base, dst=I.Direct tmpB, acc=acc})
		    end
		  else if i then let
		      val tmpI = C.newReg()
		    in (indexedEa(ba, tmpI, scale, disp, mem), 
			movl{src=ea index, dst=I.Direct tmpI, acc=acc})
		    end
		  else (indexedEa(ba, index, scale, disp, mem), acc)

		end
	    fun direct(r, acc) = 
	      if pseudoR r then (ea r, acc) else (I.Direct r, acc)

	    fun operand(I.Direct r, acc) = direct(r, acc)
	      | operand(I.Indexed{base, index, scale, disp, mem}, acc) = 
		 indexed(base, index, scale, disp, acc, mem)
	      | operand(I.Displace{base, disp, mem}, acc) = 
                 displace(base, disp, acc, mem)
	      | operand arg = arg

	    fun done(opnd, f) = let
	      val (opnd', acc') = operand(opnd, acc)
	    in f opnd' :: acc'
	    end

	    fun memArg(I.Displace _) = true
	      | memArg(I.Indexed _) = true
	      | memArg _ = false

	    fun withTmp f = let
	      val t = C.newReg()
	    in f t
	    end

            fun rewrite instr =
	    case instr
	     of I.JMP(opnd, labs) => done(opnd, fn opnd => I.JMP(opnd, labs))
	      | I.JCC{opnd, cond} => done(opnd, fn opnd => I.JCC{opnd=opnd, cond=cond})
	      | I.MOVE{src, dst, mvOp} => let
		  val (srcOpnd, acc1) = operand(src, acc)
		  val (dstOpnd, acc2) = operand(dst, acc1)
		in
		  if memArg srcOpnd andalso memArg dstOpnd then 
		    withTmp(fn t =>
		       I.MOVE{src=I.Direct t, dst=dstOpnd, mvOp=mvOp}::
			    movl{src=srcOpnd, dst=I.Direct t, acc=acc2})
		  else 
		    I.MOVE{src=srcOpnd, dst=dstOpnd, mvOp=mvOp}::acc2
		end
	      | I.LEA{r32, addr} => let
		  val (srcOpnd, acc1) = operand(addr, acc)
		in
		  if pseudoR r32 then 
		    withTmp(fn t => 
		      movl{dst=ea r32, src=I.Direct t, 
			   acc=I.LEA{r32=t, addr=srcOpnd}::acc1})
		  else I.LEA{r32=r32, addr=srcOpnd}::acc1
		end
	      | I.CMP{lsrc, rsrc} => let
		  val (lsrcOpnd, acc1) = operand(lsrc, acc)
		  val (rsrcOpnd, acc2) = operand(rsrc, acc1)
		in
		  if memArg lsrcOpnd andalso memArg rsrcOpnd then 
		    withTmp(fn t =>
		       I.CMP{lsrc=I.Direct t, rsrc=rsrcOpnd}::
			 movl{src=lsrcOpnd, dst=I.Direct t, acc=acc2})
		  else
		    I.CMP{lsrc=lsrcOpnd, rsrc=rsrcOpnd}::acc2
		end
	      | I.BINARY{binOp, src, dst} => let
		  val (srcOpnd, acc1) = operand(src, acc)
		  val (dstOpnd, acc2) = operand(dst, acc1)
		in 
		  if memArg srcOpnd andalso memArg dstOpnd then
		    withTmp(fn t =>
		      I.BINARY{binOp=binOp, src=I.Direct t, dst=dstOpnd}::
			 movl{src=srcOpnd, dst=I.Direct t, acc=acc2})
		  else I.BINARY{binOp=binOp, src=srcOpnd, dst=dstOpnd}::acc2
		end
	      | I.CALL(opnd,def,use,mem) => let
		  val (opnd1, acc1) = operand(opnd, acc)
		  fun cellset(gp, fp, cc) =
		    (List.filter (not o pseudoR) gp, fp, cc)
		in I.CALL(opnd1, cellset def, cellset use, mem)::acc
		end
	      | I.MULTDIV{multDivOp, src} => 
		  done(src, fn opnd => I.MULTDIV{multDivOp=multDivOp, src=opnd})
	      | I.MUL3{dst, src1, src2} =>  let
		  val (src1Opnd, acc1) = operand(src1, acc)
		in
		  if pseudoR dst then
		    withTmp(fn t =>
		      movl{dst=ea dst, src=I.Direct t, acc=
			I.MUL3{dst=t, src1=src1Opnd, src2=src2}::acc1})
		  else I.MUL3{dst=dst, src1=src1Opnd, src2=src2}::acc1
		end
	      | I.UNARY{unOp, opnd} => 
		  done(opnd, fn opnd => I.UNARY{unOp=unOp, opnd=opnd})
	      | I.PUSH opnd => done(opnd, fn opnd => I.PUSH opnd)	
	      | I.POP opnd => done(opnd, fn opnd => I.POP opnd)
	      | I.COPY{dst, src, tmp} => let
		  (* Note:
		   *  Parallel copies are not allowed after this point.
		   *  Consider:
		   *    (r8, r9, edx) <- (566, 567, 560)
		   *
		   *  RA32 may well decide to allocate 560 to r8.
		   *  After the rewrite we will get:
		   *
		   *      mem[r8] <- 566
		   *	  mem[r9] <- 567
		   *          edx <- 560
		   * 
		   * If 560 should spill, we all of a sudden have the 
		   * incorrect value being read from the spill location.
		   *)
		  fun f((instr as I.MOVE{mvOp, src, dst})::rest, acc) =
		      (case (src, dst)
			of (I.Direct s, I.Direct d) =>
			    if s=d then f(rest, acc)
			    else if pseudoR d andalso pseudoR  s then
			      f(rest, withTmp(fn t =>
				 (movl{src=I.Direct t, dst=ea d, acc=
				    movl{src=ea s, dst=I.Direct t, acc=acc}})))
			    else if pseudoR d then
			      f(rest, withTmp(fn t =>
				 (movl{src=I.Direct s, dst=ea d, acc=acc})))
			    else if pseudoR s then
			      f(rest, withTmp(fn t =>
			         (movl{src=ea s, dst=I.Direct d, acc=acc})))
			    else f(rest, I.COPY{src=[s], dst=[d], tmp=NONE}::acc)
		         | _  => f(rest, instr::acc)
		      (*esac*))

		    | f([], acc) = acc
	        in f(shuffle (dst, src, tmp), acc)
		end
	      | I.FSTP opnd => done(opnd, fn opnd => I.FSTP opnd)
	      | I.FLD opnd => done(opnd, fn opnd => I.FLD opnd)
	      | I.FILD opnd => done(opnd, fn opnd => I.FILD opnd)
	      | I.FBINARY{src,dst,binOp} => 
		  done(src, fn opnd => I.FBINARY{binOp=binOp, src=opnd, dst=dst})
              | I.ANNOTATION{i,a} => rewrite i
	      | _ => instr::acc
          in  rewrite instr
	  end (* subst *)
	in insns := List.foldl subst [] (rev(!insns));
	   resetLiveOut()
	end (*doBlock*)
      | doBlock _ = ()
  in app doBlock blocks;  (first, C.newReg())
  end (* rewrite *)
end
