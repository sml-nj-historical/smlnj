(* bbsched.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** bbsched.sml - invoke scheduling after span dependent resolution **)

(* Note: This module can be made to go a faster by changing vsub
 *  	to System.Unsafe.subscriptv.
 *)

signature BBSCHED = sig
  structure F : FLOWGRAPH
  structure P : INSN_PROPERTIES

  val bbsched : F.cluster -> unit
  val finish : unit -> unit
  val cleanUp : unit -> unit
end


functor BBSched
    (structure Flowgraph : FLOWGRAPH
     structure InsnProps : INSN_PROPERTIES
     structure Emitter : EMITTER_NEW
     structure Scheduler : SCHEDULER
     val icount : int -> InsnProps.I.instruction

     sharing Flowgraph.I = InsnProps.I = Emitter.I = Scheduler.I) : BBSCHED =
struct

  structure F = Flowgraph
  structure I = F.I
  structure P = InsnProps
  structure C = InsnProps.C
  structure E = Emitter

  fun error msg = MLRiscErrorMsg.impossible ("BBSched."^msg)

  val vsub = Vector.sub

  (* collect clusters as they arrive *)
  val clusterList : F.cluster list ref = ref []

  fun align n = Word.toIntX (Word.andb (Word.fromInt(n+7), Word.notb 0w7))

  fun cleanUp () = clusterList := []

  (* Note: blocks within cluster are in correct order, whereas
   * the clusters themselves are in reverse order
   *)
  fun bbsched(F.CLUSTER{blocks=blks, regmaps, ...}) = let 
      fun f(F.BBLOCK{insns,...}) = F.CODE(ref(Vector.fromList(!insns)))
	| f x = x
      val compressed = map f blks
    in
      clusterList:=
         F.CLUSTER{blocks=compressed,
		   regmaps=regmaps,
		   rematerial=[]} :: (!clusterList)
    end

		(* ---------------------- *)

  (* more compact representation for label resolution *)
  (* components of sdi are: 1. position of sdi in instruction vector,
   * 2. size of sdi under optimistic assumptions, and 3. size under
   * pessimistic assumptions. 
   * Note: the last instruction in the block is at offset 0.
   * Invariant: the sdi list is maintained in decreasing order over
   *	the first component. This is crucial to fixLabels.fix.sumSdiSize.
   *)
  datatype compressedBlk
      = DATA of int 
      | ALIGN
      | LABEL of Label.label
      | CODE of 
	 {code: I.instruction Vector.vector ref,
	  sdi: (int*int*int) list ref,
	  minSize: int,
	  maxSize: int,
	  regmaps : int Array.array ref list}

  datatype labelExtremety = LO | HI
  
  (* mkCompressedList - simplify the codeList to include just data, 
   * 	labels and code.
   *)
  fun mkCompressedList(F.CLUSTER{blocks,regmaps,...}::rest) = let
      fun f(F.MARK::blks,n) = f(blks,n+4)
	| f(F.REALconst(lab,_)::blks,n) =
	  if n=0 then ALIGN::DATA 8::LABEL lab::f(blks,8)
	  else DATA n::ALIGN::DATA 8::LABEL lab::f(blks,8)
	| f(F.STRINGconst(lab,_,s)::blks,n) = let val sSize = String.size s
	  in if n=0 then DATA 8::LABEL lab::f(blks,sSize)
	     else DATA(n+8)::LABEL lab::f(blks,sSize)
	  end
	| f(F.JMPtable(lab,labs)::blks,n) = let val tSize = 4 * length labs
	  in if n=0 then LABEL lab::f(blks,tSize)
	     else DATA n::LABEL lab::f(blks,tSize)
	  end
	| f(F.LABEL lab::blks,n) = 
	  if n=0 then LABEL lab::f(blks,0)
	  else DATA n::LABEL lab::f(blks,0)
	| f(F.CODE instrs::blks,n) = let
	    val v = !instrs
	    fun findSdis(~1,min,max,sdis) = (sdis,min,max)
	      | findSdis(n,min,max,sdis) = let
		  val insn = vsub(v,n)
		  val minSize = P.minSize insn
		  val maxSize = P.maxSize insn
		in
		    if P.isSdi insn then
			findSdis(n-1,min,max,(n,minSize,minSize)::sdis)
		    else
			findSdis(n-1,min+minSize,max+maxSize,sdis)
		end
	    val (sdis,min,max) = findSdis(Vector.length v-1,0,0,[])
	    val icount = 
	      if Word.andb(Word.fromInt(!Control.CG.misc4), 0w4096) <> 0w0
		then 4
	      else 0
	    val cn = CODE{code=instrs,
			  sdi=ref sdis,
			  minSize=min + icount,
			  maxSize=max + icount,
			  regmaps=regmaps}
	  in
	    if n=0 then cn::f(blks,0) else DATA n::cn::f(blks,0)
	  end
	| f(F.BBLOCK _::_,_) = error "mkCompressedList.f: BBLOCK"
	| f([],n) = 
	   if n=0 then mkCompressedList rest else DATA n::mkCompressedList rest
    in
      f(blocks,0)
    end
    | mkCompressedList [] = []

  local
      (* modifies the entries for SDIs and labmap 
       * according to extremety 
       *)
      fun fixLabels(compressed,labMap,extremety) = let
	  val lookup 	  = Intmap.map labMap
	  val enter  	  = Intmap.add labMap
	  val lookupLabel = lookup o Label.id

	  fun fix([],_,changed) = changed
	    | fix(ALIGN::blks,loc,changed) = fix(blks,loc+4,changed)
	    | fix(DATA n::blks,loc,changed) = fix(blks,loc+n,changed)
	    | fix(LABEL lab::blks,loc,changed) = let
	        val id = Label.id lab
	      in
		if changed then (enter(id,loc); fix(blks,loc,changed))
		else if lookup id = loc then fix(blks,loc,changed)
		else (enter(id,loc); fix(blks,loc,true))
    	      end
	    | fix(CODE{sdi=ref [],minSize,maxSize,...}::blks,loc,changed) = 
	        (case extremety 
	         of LO => fix(blks,minSize+loc,changed)
	          | HI => fix(blks,maxSize+loc,changed))
	    | fix(CODE{code,sdi=rsdi,minSize,maxSize,...}::blks,loc,changed) = let
	        val instrs  = !code 
		val codeLen = Vector.length instrs - 1
		val sdi     = !rsdi
		val codeSize = case extremety of LO =>minSize | HI =>maxSize

		(* sumSdiSize(SDIs,size,delta,acc)
		 *	delta - is the increase in size of SDIs needed
		 *	to calculate the correct value of loc.
		 *)
		fun sumSdiSize([],size,_,sdi') = (size,rev sdi')
		  | sumSdiSize((sdi as (n,lo,hi))::rest,size,delta,sdi') = let
		      val sdiLoc = (codeLen-n)*4+loc+delta
		      val s = P.sdiSize(vsub(instrs,n),lookupLabel,sdiLoc)
		    in
		      case extremety
		      of LO =>
			  if s <= lo then
			     sumSdiSize(rest,size+lo,delta+lo-4,sdi::sdi')
			  else
			     sumSdiSize(rest,size+s,delta+s-4,(n,s,hi)::sdi')
		       | HI =>
			  if s <= hi then
			     sumSdiSize(rest,size+hi,delta+hi-4,sdi::sdi')
			  else
			     sumSdiSize(rest,size+s,delta+s-4,(n,lo,s)::sdi')
		    end (* sumSdiSize *)
		val (sdiSize,sdi') = sumSdiSize(sdi,0,0,[])
     	      in
		  rsdi:=sdi';
		  fix(blks,loc+codeSize+sdiSize,changed)
	      end (* fix *)
	  fun fixpoint () = if fix(compressed,0,false) then fixpoint() else ()
        in
	    fixpoint()
        end (* fixLabels *)
  in
      fun fixLoLabels(compressed,labMap) = fixLabels(compressed,labMap,LO)
      fun fixHiLabels(compressed,labMap) = fixLabels(compressed,labMap,HI)
  end

  (* note: The result of initLabMaps can be used to short circuit 
   * all this, if we know that max < {some machine specifc constant} 
   * that  guarantees that all sdis will be their minimum size. Should 
   * add a flag minSdiGuarantee to machine description.
   *)
  fun process compressed = let
      exception BBSchedLabMap
      val lowLabMap : int Intmap.intmap = Intmap.new(16,BBSchedLabMap)
      val hiLabMap  : int Intmap.intmap = Intmap.new(16,BBSchedLabMap)
      val lookupHi  = Intmap.map hiLabMap

      (* initialize label maps based on extremety *)
      fun initLabMaps () = let
	  val enterLow = Intmap.add lowLabMap
	  val enterHi  = Intmap.add hiLabMap
	  fun iter([],_,hi) = hi
	    | iter(ALIGN::blks,low,hi) = iter(blks,low+4,hi+4)
	    | iter(DATA n::blks,low,hi) = iter(blks,low+n,hi+n)
	    | iter(LABEL lab::blks,low,hi) = let val id = Label.id lab
	      in
		  enterLow(id,low); enterHi(id,hi); iter(blks,low,hi)
	      end
	    | iter(CODE{minSize,maxSize,sdi,code,...}::blks,low,hi) = let
	        fun doSdi((n,_,_)::rest,size,acc) = let
		    val sdiSize=P.minSize(vsub(!code,n))
		  in
		    doSdi(rest,size+sdiSize,(n,sdiSize,sdiSize)::acc)
		  end
		  | doSdi([],size,acc) = (size,rev acc)

		val (sdiBaseSize,sdi') = doSdi(!sdi,0,[])
	      in
		sdi:=sdi';
		iter(blks,low+minSize+sdiBaseSize,hi+maxSize+sdiBaseSize)
	      end
	in
	    iter(compressed,0,0)
	end (* initLabMaps *)

      (* invokeSchedule: - expands sdis to native instructions before
       *	invoking the scheduler.
       *)
      fun invokeSchedule(codeRef,sdis,regmaps) = let
	  fun expand(instrs,sdis) = let
	      val ilen = Vector.length instrs
	      val vec = Array.array(ilen,0)
	      fun markSdi[] = ()
		| markSdi((n,_,hi)::rest) = 
		    (Array.update(vec,n,hi); markSdi rest)

	      fun f(~1,[],acc) = acc 
		| f(~1,curr,acc) = rev(Vector.fromList curr::acc)
		| f(n,curr,acc) = let
		    val insn = vsub(instrs,n)
		    fun add([],curr,acc) = (curr,acc)
		      | add(instr::rest,curr,acc) =
			(case P.instrKind instr
			 of P.IK_JUMP =>
			     add(rest,[], Vector.fromList(instr::curr)::acc)
			  | _  => add(rest,instr::curr,acc)
			 (* esac *))
		    val size = Array.sub(vec,n)
		  in
		    if (size = 0) then f(n-1,insn::curr,acc)
		    else let
			val sdiCode = P.expand(insn,size,lookupHi)
			val (curr',acc') = add(sdiCode,curr,acc)
		      in
			f(n-1,curr',acc')
		      end
		  end
	    in
		markSdi sdis; 
		f(ilen -1,[],[])
	    end (* expand *)

	  fun doit(iv::instrV,acc) = 
		doit(instrV,Scheduler.schedule(iv,regmaps) :: acc) 
	    | doit([],acc) = let
		fun vecToList(v, i, len) = 
		  if i = len then [] else Vector.sub(v, i)::vecToList(v, i+1, len)
		fun merge([], acc) = acc
		  | merge(v::vs, acc) = 
		      merge(vs, vecToList(v,0,Vector.length v) @ acc)
	      in
		Vector.fromList (merge(acc, []))
	      end
	  val newCode = doit(expand(!codeRef,sdis),[])
        in
	  codeRef:=newCode; DATA(Vector.length newCode * 4)
        end (* invokeSchedule *)

      fun procStableBBs([],cl,stable,unStable) = (rev cl,stable,unStable)
	| procStableBBs(CODE{code,sdi=ref [],regmaps,...}::blks,cl,s,u) = let
            val sched'd = Scheduler.schedule(!code, regmaps)
	    val nd = DATA(Vector.length sched'd * 4)
	  in
	     code:=sched'd; procStableBBs(blks,nd::cl,s+1,u)
	  end
	| procStableBBs((c as CODE{code,sdi,regmaps,...})::blks,cl,s,u) = let
	    val instrs = !code
	    val sdi' = !sdi   

	    fun isStable [] = true
	      | isStable((_,lo:int,hi)::rest) = lo=hi andalso isStable rest

	    fun initSdi [] = []
	      | initSdi((n,_,_)::rest) = let 
		  val min = P.minSize(vsub(instrs,n))
		in
		  (n,min,min)::initSdi rest
		end

	  in
	      if isStable(sdi') 
              then let val nd = invokeSchedule(code,sdi',regmaps)
		 in
		     procStableBBs(blks,nd::cl,s+1,u)
		 end
	      else
		 (sdi := initSdi sdi';
		  procStableBBs(blks,c::cl,s,u+1))
	  end
	| procStableBBs(blk::blks,cl,s,u) = procStableBBs(blks,blk::cl,s,u)

      fun procUnStableBBs([],cl) = rev cl
	| procUnStableBBs(CODE{code,sdi,regmaps,...}::blks,cl) = let
	    val nd = invokeSchedule(code,!sdi,regmaps)
	  in
	    procUnStableBBs(blks,nd::cl)
	  end
	| procUnStableBBs(blk::blks,cl) = procUnStableBBs(blks,blk::cl)

      val max = initLabMaps()
      val _ = (fixLoLabels(compressed,lowLabMap); 
	       fixHiLabels(compressed,hiLabMap))

      val (cl,nStable,nUnStable) = procStableBBs(compressed,[],0,0)
    in
	if nUnStable = 0 then cl
	else if nStable <> 0 then process cl
	else 
	    (print "processing unstable...\n";
	     initLabMaps(); 
	     fixHiLabels(cl,hiLabMap); 
	     procUnStableBBs(cl,[]))
    end (* process *)

  fun finish () = let
      fun initLabels([],loc) = loc
	| initLabels(ALIGN::blks,loc) = initLabels(blks,align loc)
	| initLabels(LABEL lab::blks,loc) =
	    (Label.setAddr(lab,loc); initLabels(blks,loc))
	| initLabels(DATA n::blks,loc) = initLabels(blks,loc+n)
	| initLabels(CODE _::_,_) = error "finish.initLabels"

      val clusters = rev (!clusterList) before clusterList := []
      val codeList = process(mkCompressedList clusters)
      val size = initLabels(codeList,0)

      fun emitInstrs(n,instrs,regmaps) =
	    (E.emitInstr(vsub(instrs,n),regmaps); 
	     emitInstrs(n+1,instrs,regmaps))
	    	handle _ => ()

      fun emit(F.CLUSTER{blocks,regmaps,...}) = let
	  fun emitBlock F.MARK 	               = E.mark ()
	    | emitBlock(F.LABEL lab) 	       = E.defineLabel lab
	    | emitBlock(F.REALconst arg)       = E.emitReal arg
	    | emitBlock(F.STRINGconst arg)     = E.emitString arg
	    | emitBlock(F.JMPtable(lab,labs))  = E.emitJmpTable(lab,labs)
	    | emitBlock(F.BBLOCK{insns,...})   = error "finish:F.BBLOCK"
	    | emitBlock(F.CODE(instrs))        = emitInstrs(0,!instrs,regmaps)
        in app emitBlock blocks
	end
    in
      E.init size;
      app emit (clusters)
    end (* finish *)

  val finish = Stats.doPhase (Stats.makePhase "Compiler 130 Schedule") finish

end (* BBSched *)

(*
 * $Log: bbsched.sml,v $
 * Revision 1.1.1.1  1997/04/19 18:14:20  george
 *   Version 109.27
 *
 *)
