(* 
 * This version of the span dependency resolution also fill delay slots
 * using a few simple strategies.
 * 
 * Allen
 *)

functor SpanDependencyResolution
    (structure Flowgraph : FLOWGRAPH
     structure Emitter : INSTRUCTION_EMITTER
     structure Jumps : SDI_JUMPS
     structure DelaySlot : DELAY_SLOT_PROPERTIES
     structure Props : INSN_PROPERTIES
       sharing Flowgraph.P = Emitter.P
       sharing Flowgraph.I = Jumps.I = DelaySlot.I = Props.I = Emitter.I)
         : BBSCHED =
struct

  structure F = Flowgraph
  structure E = Emitter
  structure I = F.I
  structure C = I.C
  structure J = Jumps
  structure P = Flowgraph.P
  structure D = DelaySlot
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
    | CLUSTER of {comp : compressed list,
                  regmap : C.cell -> C.cell
                 }

  val clusterList : compressed list ref = ref []
  fun cleanUp() = clusterList := []

  fun bbsched(cluster as 
              F.CLUSTER{blocks, regmap, blkCounter, ...}) = 
  let val regmap = C.lookup regmap

      fun blknumOf(F.BBLOCK{blknum,...}) = blknum
        | blknumOf(F.EXIT{blknum,...}) = blknum
        | blknumOf _ = error "blknumOf"

      fun noPseudo [] = true
        | noPseudo (F.BBLOCK _::_) = true
        | noPseudo (F.LABEL _::rest) = noPseudo rest
        | noPseudo (F.PSEUDO _::_) = false

      (* Maps blknum -> label at the position of the second instruction *)
      val dummy = Label.newLabel ""
      val labelMap = A.array(!blkCounter,dummy)

      (* enter labels into the labelMap *)
      fun enterLabels([]) = ()
        | enterLabels(F.PSEUDO _::rest) = enterLabels rest
        | enterLabels(F.LABEL _::rest) = enterLabels rest
        | enterLabels(F.BBLOCK{blknum,...}::rest) = 
             (A.update(labelMap,blknum,Label.newLabel ""); enterLabels rest)
        | enterLabels _ = error "enterLabels"

      (* Is the instruction an empty copy *)
      fun isEmptyCopy instr = 
          Props.instrKind instr = Props.IK_COPY andalso
          J.sdiSize(instr,regmap,Label.addrOf,0) = 0 
      (* 
       * Find the branch target of block blknum, return the first instruction
       * in the target block and its associated label. 
       *
       * BUG FIX: have to make sure that the first instruction cannot be
       * used to fill the delay slot in the target block!
       *)
      fun findTarget(blknum,[(F.BBLOCK{blknum=x,insns=insns1,...},_),
                             (F.BBLOCK{blknum=y,insns=insns2,...},_)]) =
          let fun extract(blknum,insns) =  
              let (* First we skip all the empty copies *)
                  fun find [] = NONE
                    | find(instrs as instr::rest) = 
                       if isEmptyCopy instr then find rest
                       else find' rest 

                  (* Okay, we are now guaranteed that the remaining 
                   * instructions will not be used in the delay slot of
                   * the current block.   Find the first instruction.
                   *)
                  and find' [first] = SOME(first,A.sub(labelMap,blknum))
                    | find' [] = NONE
                    | find' (_::rest) = find' rest
              in  case insns of
                    jmp::rest => if Props.instrKind jmp = Props.IK_JUMP then 
                                 find rest else find insns
                  | [] => NONE (* no first instruction *)
              end
          in  if x = blknum + 1 then extract(y,!insns2)
              else if y = blknum + 1 then extract(x,!insns1)
              else NONE 
          end
        | findTarget _ = NONE


      (* Convert a cluster into compressed form *)
      fun compress(F.PSEUDO pOp::rest) = PSEUDO pOp::compress rest
        | compress(F.LABEL lab::rest) = LABEL lab:: compress rest
        | compress(F.BBLOCK{blknum,insns,succ,...}::rest) = 
	let 
            (* WARNING!!! Assumes jump to exit is NOT a backward branch *)

            val backward = List.exists 
                   (fn (F.EXIT _,_) => false
                     | (b,_) => blknumOf b <= blknum) (!succ) 

            (* build the code list *)
            fun scan([],nonSdiInstrs,nonSdiSize,code) = 
                   group(nonSdiSize,nonSdiInstrs,code)
              | scan(instr::instrs,nonSdiInstrs,nonSdiSize,code) =
                let val {n,nOn,nOff,nop} = D.delaySlot{instr=instr,backward=backward}
                in  case (nOff,instrs) of
                        (D.D_ALWAYS,delaySlot::rest) => 
                        if D.delaySlotCandidate{jmp=instr,
                                                delaySlot=delaySlot} andalso
                           not(D.conflict{regmap=regmap,src=delaySlot,dst=instr}) 
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
                           D.conflict{regmap=regmap,src=delaySlot,dst=jmp} 
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
                    case (nOn,findTarget(blknum,!succ)) of
                      (D.D_TAKEN,SOME(delaySlot,label)) => 
                        if not(D.delaySlotCandidate{jmp=jmp,
                                              delaySlot=delaySlot}) orelse
                          D.conflict{regmap=regmap,src=delaySlot,dst=jmp} 
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

        

            (*
             * Try to remove the branch if it is unnecessary 
             *)
            and processBranch(jmp,body) =
                  case (!succ, noPseudo rest) of
                     ([(F.BBLOCK{blknum=id,...},_)],true) =>
                        if id = blknum + 1 then scan(body,[],0,[])
                        else fitDelaySlot(jmp,body)
                  |  _ => fitDelaySlot(jmp,body)

            and process([],others) = others
              | process(instrs as jmp::body,others) =
                CODE(A.sub(labelMap,blknum),
                     case Props.instrKind jmp of
                       Props.IK_JUMP => processBranch(jmp,body)
                     | _ => scan(instrs,[],0,[])
                    )::others
	in 
	    process(!insns,compress rest)
	end
      | compress [] = []
  in  enterLabels blocks;
      clusterList:=CLUSTER{comp = compress blocks, regmap=regmap}:: 
		    (!clusterList)
  end

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

    fun adjust(CLUSTER{comp, regmap, ...}::cluster, pos, changed) = 
    let fun scan(PSEUDO pOp::rest, pos, changed) = 
              scan(rest, pos+P.sizeOf(pOp,pos), changed)
	  | scan(LABEL _::rest, pos, changed) = scan(rest, pos, changed)
	  | scan(CODE(_,code)::rest, pos, changed) = 
              let val (pos,changed) = doCode(code,pos,changed)
              in  scan(rest,pos,changed) end
	  | scan([], pos, changed) = adjust(cluster, pos, changed)
        and doCode([],pos,changed) = (pos,changed)
          | doCode(code::rest,pos,changed) =
            case code of
              FIXED{size,...} => doCode(rest,pos+size,changed)
            | SDI{size, insn} =>
              let val newSize = J.sdiSize(insn, regmap, Label.addrOf, pos)
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
    fun emitCluster(CLUSTER{comp, regmap},loc) = let
      val emit = emit regmap
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
    in foldl process loc comp
    end

    val compressed = (rev (!clusterList)) before cleanUp()
    val size = fixpoint compressed
  in beginCluster size;
     foldl emitCluster 0 compressed handle e => raise e;
     ()
  end (*finish*)

end (* spanDep.sml *)

