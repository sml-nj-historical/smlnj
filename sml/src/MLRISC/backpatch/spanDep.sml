(* 
 * This version of the span dependency resolution also fill delay slots
 * using a few simple strategies.
 * 
 * Allen
 *)

functor SpanDependencyResolution
    (structure Emitter   : INSTRUCTION_EMITTER
     structure CFG       : CONTROL_FLOW_GRAPH
     			where I = Emitter.I
			  and P = Emitter.S.P
     structure Jumps     : SDI_JUMPS
     			where I = CFG.I
     structure DelaySlot : DELAY_SLOT_PROPERTIES
     			where I = CFG.I
     structure Props     : INSN_PROPERTIES
     			where I = CFG.I
     structure Placement : BLOCK_PLACEMENT
			where CFG = CFG
     ) : BBSCHED = 
     (*
       sharing Placement.CFG = CFG
       sharing CFG.P = Emitter.P
       sharing CFG.I = Jumps.I = DelaySlot.I = Props.I = Emitter.I)
         : BBSCHED = *)
struct

  structure CFG = CFG
  structure E = Emitter
  structure I = CFG.I
  structure C = I.C
  structure J = Jumps
  structure P = CFG.P
  structure D = DelaySlot
  structure G = Graph
  structure A = Array

  fun error msg = MLRiscErrorMsg.error("SpanDependencyResolution",msg)

  datatype code =
      SDI of {size : int ref,		(* variable sized *)
	      insn : I.instruction}
    | FIXED of {size: int,		(* size of fixed instructions *)
		insns: I.instruction list}
    | BRANCH of {insn : code list,      (* instruction with delay slot*)
                 branchSize : int,
                 fillSlot : bool ref} 
    | DELAYSLOT of {insn : code list,    (* instruction in delay slot *)
                    fillSlot : bool ref}
    | CANDIDATE of (* two alternatives *)
      { oldInsns  : code list, (* without delay slot filling *)
        newInsns  : code list, (* when delay slot is filled *)
        fillSlot  : bool ref   (* should we fill the delay slot? *)
      }
   
  datatype compressed = 
      PSEUDO of P.pseudo_op
    | LABEL  of Label.label
    | CODE of Label.label * code list
    | CLUSTER of {comp : compressed list}

  val clusterList : compressed list ref = ref []
  val dataList : P.pseudo_op list ref = ref []
  fun cleanUp() = (clusterList := []; dataList := [])

  fun bbsched(cfg as G.GRAPH graph) = let
    fun maxBlockId (CFG.BLOCK{id, ...}::rest, curr) = 
       if id > curr then maxBlockId(rest, id) else maxBlockId(rest, curr)
     | maxBlockId([], curr) = curr
    val blocks = map #2 (Placement.blockPlacement(cfg))
    val N = maxBlockId(blocks, #capacity graph ())

    (* Order of blocks in code layout *)
    val blkOrder = Array.array(N, 0)

    (* Maps blknum -> label at the position of the second instruction *)
    (* This is incase the first instruction gets used to fill a delay slot *)
    val dummy = Label.anon ()
    val labelMap = A.array(N, dummy)

    (* enter labels into the labelMap *)
    fun enterLabels(blocks) = 
      List.app 
        (fn CFG.BLOCK{id, ...} => Array.update(labelMap, id, Label.anon ()))
	blocks

    (* create block order *)
    fun blockOrder(blocks) = let
      fun order(CFG.BLOCK{id, ...}, n) = (Array.update(blkOrder, id, n); n+1)
    in List.foldl order 0 blocks
    end

    fun isFallthrough(blk1, blk2) = 
      Array.sub(blkOrder, blk1) + 1 = Array.sub(blkOrder, blk2)

    fun isBackwards(blk1, blk2) = 
      Array.sub(blkOrder, blk2) <= Array.sub(blkOrder, blk1)

    (* zero length copy instruction *)
    fun isEmptyCopy instr =
      Props.instrKind(instr) = Props.IK_COPY 
         andalso J.sdiSize(instr, Label.addrOf, 0) = 0 

    (* Find the target of a block, and return the first instruction and 
     * its associated label.
     *)
    fun findTarget(blknum, [CFG.BLOCK{id=id1, insns=insns1, ...},
			    CFG.BLOCK{id=id2, insns=insns2, ...}]) = let
          fun extract(blknum, insns) = let
	    (* skip over empty copies *)
	    fun find [] = NONE
	      | find(instrs as instr::rest) = 
	        if isEmptyCopy instr then find rest else find' rest

	    (* Okay, we are now guaranteed that the remaining 
	     * instructions will not be used in the delay slot of
	     * the current block.   Find the first instruction.
	     *)
	    and find' [first] = SOME(first, A.sub(labelMap,blknum))
	      | find' [] = NONE
	      | find' (_::rest) = find' rest
          in
	    case insns 
	     of jmp::rest => 
	         if Props.instrKind jmp = Props.IK_JUMP then find rest 
		 else find insns
	      | [] => NONE (* no first instruction *)
          end
        in
	  if isFallthrough(blknum, id1) then extract(id2, !insns2)
	  else if isFallthrough(blknum, id2) then extract(id1, !insns1)
	       else NONE
	end
      | findTarget _ = NONE



    fun compress [] = []
      | compress (CFG.BLOCK{id, align, labels, insns, ...}::rest) = let

          val succ = map (#node_info graph) (#succ graph id)

	  val backward = 
	    List.exists 
	      (fn CFG.BLOCK{id=id1, ...} => isBackwards(id, id1))
	      succ

	  (* build the code list *)
	  fun scan([],nonSdiInstrs,nonSdiSize,code) = 
		 group(nonSdiSize,nonSdiInstrs,code)
	    | scan(instr::instrs,nonSdiInstrs,nonSdiSize,code) =
	      let val {n,nOn,nOff,nop} = D.delaySlot{instr=instr,backward=backward}
	      in  case (nOff,instrs) of
		      (D.D_ALWAYS,delaySlot::rest) => 
		      if D.delaySlotCandidate{jmp=instr,
					      delaySlot=delaySlot} andalso
			 not(D.conflict{src=delaySlot,dst=instr}) 
		      then scan(rest,[],0,
				mkCandidate1(instr,delaySlot)::
				group(nonSdiSize,nonSdiInstrs,code))
		      else scanSdi(instr,instrs,nonSdiInstrs,nonSdiSize,code)
		  |  _ =>  scanSdi(instr,instrs,nonSdiInstrs,nonSdiSize,code)
	      end
	  and scanSdi(instr,instrs,nonSdiInstrs,nonSdiSize,code) =
	      let val s = J.minSize instr
	      in  if J.isSdi instr then
		       scan(instrs,[],0,SDI{size=ref s,insn=instr}::
			    group(nonSdiSize,nonSdiInstrs,code))
		  else scan(instrs,instr::nonSdiInstrs,nonSdiSize+s,code)
	      end
	  and group(0,[],code) = code
	    | group(size,insns,code) = FIXED{size=size,insns=insns}::code

	  and buildList instrs = scan'(instrs,[],0,[])

	  and scan'([],nonSdiInstrs,nonSdiSize,code) = 
		 group(nonSdiSize,nonSdiInstrs,code)
	    | scan'(instr::instrs,nonSdiInstrs,nonSdiSize,code) =
	      let val s = J.minSize instr
	      in  if J.isSdi instr then
		       scan'(instrs,[],0,SDI{size=ref s,insn=instr}::
			     group(nonSdiSize,nonSdiInstrs,code))
		  else scan'(instrs,instr::nonSdiInstrs,nonSdiSize+s,code)
	      end

	  (* 
	   * Create a branch delay slot candidate sequence.
	   * jmp is the normal jump instruction; jmp' is the
	   * jump instruction when the delay slot is active.
	   *)
	  and mkCandidate1(jmp,delaySlot) = 
	      let val fillSlot = ref true
		  val jmp' = D.enableDelaySlot{n=false,nop=false,instr=jmp}
	      in  CANDIDATE{newInsns= 
			      [BRANCH{branchSize=J.minSize jmp',
				      insn=buildList [jmp'],
				      fillSlot=fillSlot},
			       DELAYSLOT{insn=buildList [delaySlot],
					 fillSlot=fillSlot}],
			    oldInsns=buildList [jmp,delaySlot],
			    fillSlot=fillSlot}
	      end 

	  (* 
	   * Create a branch delay slot candidate sequence.
	   * jmp is the normal jump instruction; jmp' is the
	   * jump instruction when the delay slot is active.
	   *)
	  and mkCandidate2(jmp,delaySlot,label) = 
	      let val fillSlot = ref true
		  val jmp' = D.setTarget(
			      D.enableDelaySlot{n=true,nop=false,instr=jmp},
			      label)
	      in  CANDIDATE{newInsns= 
			      [BRANCH{branchSize=J.minSize jmp',
				      insn=buildList [jmp'],
				      fillSlot=fillSlot},
			       DELAYSLOT{insn=buildList [delaySlot],
					 fillSlot=fillSlot}],
			    oldInsns=buildList [jmp],
			    fillSlot=fillSlot}
	      end 

	  (*
	   * Try different strategies for delay slot filling
	   *)
	  and fitDelaySlot(jmp,body) =
	     (case body of  (* remove empty copies *)
		[] => fitDelaySlot'(jmp,body)
	      | prev::rest =>
		  if isEmptyCopy prev
		  then fitDelaySlot(jmp,rest)
		  else fitDelaySlot'(jmp,body)
	     )

	  and fitDelaySlot'(jmp,body) =
	  let val {n,nOn,nOff,nop} = D.delaySlot{instr=jmp,backward=backward}
	      (* 
	       * Use the previous instruction to fill the delay slot 
	       *)
	      fun strategy1() =
		  case (nOff,body) of
		     (D.D_ALWAYS,delaySlot::body) => 
		      if not(D.delaySlotCandidate{jmp=jmp,
						 delaySlot=delaySlot}) orelse
			 D.conflict{src=delaySlot,dst=jmp} 
		      then strategy2()
		      else scan(body,[],0,
				[mkCandidate1(eliminateNop jmp,delaySlot)])
		  | _ => strategy2()
	      (* 
	       * Use the first instruction in the target block to fill
	       * the delay slot.
	       * BUG FIX: note this is unsafe if this first instruction
	       * is also used to fill the delay slot in the target block!  
	       *)
	      and strategy2() =
		  case (nOn,findTarget(id,succ)) of
		    (D.D_TAKEN,SOME(delaySlot,label)) => 
		      if not(D.delaySlotCandidate{jmp=jmp,
					    delaySlot=delaySlot}) orelse
			D.conflict{src=delaySlot,dst=jmp} 
		      then strategy3()
		      else scan(body,[],0,
			   [mkCandidate2(eliminateNop jmp,delaySlot,label)])
		  | _ => strategy3()

	      (* 
	       * If nop is on and if the delay slot is only active on
	       * the fallsthru branch, then turn nullify on and eliminate
	       * the delay slot
	       *)
	      and strategy3() = scan(eliminateNop(jmp)::body,[],0,[]) 

	      and eliminateNop(jmp) = 
		  case (nop,nOn) of
		     (true,(D.D_FALLTHRU | D.D_NONE)) =>
			  D.enableDelaySlot{n=true,nop=false,instr=jmp}
		  |  _ => jmp

	  in  strategy1()
	  end


	  and process([],others) = others
	    | process(instrs as jmp::body,others) = let
               fun alignIt(chunks) = 
	         (case !align of NONE => chunks | SOME p => PSEUDO(p)::chunks)
              in
		alignIt
		   (map LABEL (!labels) @
		      CODE
		        (A.sub(labelMap, id),
			 case Props.instrKind jmp 
		          of Props.IK_JUMP => fitDelaySlot(jmp,body)
			   | _	           => scan(instrs,[],0,[])
	       	         )::others)
              end
	in 
	  process(!insns,compress rest)
	end (* compress *) 

    val CFG.INFO{data, ...} = #graph_info graph
  in
    blockOrder(blocks);
    enterLabels(blocks);
    clusterList := CLUSTER{comp=compress blocks} :: !clusterList;
    dataList := !data @ !dataList
  end (* bbsched *)




  fun finish() = let
    fun labels(PSEUDO pOp::rest, pos) = 
          (P.adjustLabels(pOp, pos); labels(rest, pos+P.sizeOf(pOp,pos)))
      | labels(LABEL lab::rest, pos) = 
	 (Label.setAddr(lab,pos); labels(rest, pos))
      | labels(CODE(lab,code)::rest, pos) = let
	  fun size(FIXED{size, ...}) = size
	    | size(SDI{size, ...}) = !size
            | size(BRANCH{insn,...}) = sizeList(insn,0)
            | size(DELAYSLOT{insn,...}) = sizeList(insn,0)
            | size(CANDIDATE{oldInsns,newInsns,fillSlot,...}) =
                sizeList(if !fillSlot then newInsns else oldInsns,0)
          and sizeList([],n) = n
            | sizeList(code::rest,n) = sizeList(rest,size code + n)
	in  Label.setAddr(lab,pos+4);
            labels(rest, sizeList(code,pos))
	end
      | labels(CLUSTER{comp, ...}::rest, pos) = labels(rest, labels(comp,pos))
      | labels([], pos) = pos

    val delaySlotSize = D.delaySlotSize

    fun adjust(CLUSTER{comp, ...}::cluster, pos, changed) = 
    let fun scan(PSEUDO pOp::rest, pos, changed) = 
              scan(rest, pos+P.sizeOf(pOp,pos), changed)
	  | scan(LABEL _::rest, pos, changed) = scan(rest, pos, changed)
	  | scan(CODE(_,code)::rest, pos, changed) = 
              let val (pos,changed) = doCode(code,pos,changed)
              in  scan(rest,pos,changed) end
	  | scan([], pos, changed) = adjust(cluster, pos, changed)
          | scan _ = error "scan"
        and doCode([],pos,changed) = (pos,changed)
          | doCode(code::rest,pos,changed) =
            case code of
              FIXED{size,...} => doCode(rest,pos+size,changed)
            | SDI{size, insn} =>
              let val newSize = J.sdiSize(insn, Label.addrOf, pos)
 	      in  if newSize <= !size then 
                     doCode(rest,!size + pos,changed)
		  else (size:=newSize; doCode(rest, newSize+pos, true))
              end
            | DELAYSLOT{insn,fillSlot,...} => 
                let val (newPos,changed) = doCode(insn,pos,changed)
                in  doCode(rest, newPos,
                           if newPos - pos <> delaySlotSize then 
                           (fillSlot := false; true) else changed)
                end
            | BRANCH{insn,branchSize,fillSlot,...} => 
                let val (newPos,changed) = doCode(insn,pos,changed)
                in  doCode(rest, newPos,
                           if newPos - pos <> branchSize then
                           (fillSlot := false; true) else changed)
                end
            | CANDIDATE{oldInsns,newInsns,fillSlot,...} =>
                doCode((if !fillSlot then newInsns else oldInsns) @ rest,
                       pos,changed)
    in  scan(comp, pos, changed)
    end
      | adjust(_::_, _, _) = error "adjust"
      | adjust([], _, changed) = changed

    fun fixpoint zl = let 
      val size = labels(zl, 0)
    in if adjust(zl, 0, false) then fixpoint zl else size
    end

    val E.S.STREAM{defineLabel,pseudoOp,emit,beginCluster,...} = E.makeStream []
    fun emitCluster(CLUSTER{comp},loc) = let
	  val emitInstrs = app emit 
	  fun process(PSEUDO pOp,loc) = (pseudoOp pOp; loc+P.sizeOf(pOp,loc))
	    | process(LABEL lab,loc) = (defineLabel lab; loc)
	    | process(CODE(_,code),loc) = let
		fun e(FIXED{insns, size, ...},loc) = (emitInstrs insns; loc+size)
		  | e(SDI{size, insn},loc) = 
		      (emitInstrs(J.expand(insn, !size, loc)); !size + loc)
		  | e(BRANCH{insn,...},loc) = foldl e loc insn
		  | e(DELAYSLOT{insn,...},loc) = foldl e loc insn
		  | e(CANDIDATE{newInsns,oldInsns,fillSlot,...},loc) =
		      foldl e loc (if !fillSlot then newInsns else oldInsns)
	      in foldl e loc code
	      end
	    | process _ = error "process"
	in foldl process loc comp
	end
      | emitCluster _ = error "emitCluster"

    (* The dataList is in reverse order, and the entries in each
     * are also in reverse 
     *)
    val compressed = rev (map PSEUDO (!dataList) @ !clusterList) before cleanUp()
  in 
     beginCluster(fixpoint(compressed));
     foldl emitCluster 0 compressed;
     ()
  end (*finish*)
end (* spanDep.sml *)

