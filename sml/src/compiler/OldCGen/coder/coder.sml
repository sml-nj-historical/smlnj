(* coder.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * This is a machine independent code scheduler for RISC machines with 32-bit
 * instructions.  We assume that the machine has delayed branches.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 * Also fiddled with by Lal George, Andrew Appel
 *
 *)

signature CODER =
sig
    eqtype label
    type 'label instruction
    type 'label sdi

    val baseLab : label   (* The symbolic base address of the current code block. *)

    val newLabel : unit -> label
    val define : label -> unit

    val emitLong : int -> unit
    val emitString : string -> unit
    exception BadReal of string
    val emitReal : string -> unit
    val emitLabel : (label * int) -> unit
	(* L3: emitLabel(L2, k) is equivalent to L3: emitLong(k+L2-L3) *)

    val mark : unit -> unit

    val emit : label instruction -> unit
    val emitSDI : label sdi -> unit

    val comment : string -> unit

    val finish : unit -> unit

end (* signature CODER *)

functor Coder (structure M : MACHINSTR and E : EMITTER
	       sharing type M.instruction = E.instruction
		   and type M.info = E.info) : CODER = 
struct

    structure V = Vector
    open M
    val error = ErrorMsg.impossible

    structure Label :> sig
	eqtype label
	val reset : unit -> unit
	val newLabel : unit -> label
	val nameOf : label -> string
	val numberOf: label -> int
	val count : unit -> int
	val baseLab : label
      end = struct 
	type label = int
	val cnt = ref 1
	fun reset() = cnt := 1
	fun newLabel() = let val x = !cnt in cnt := x+1; x end
	fun nameOf (id:label) = "L" ^ (Int.toString id)
	fun numberOf id = id
	fun count() = !cnt
	val baseLab = 0
    end

    open Label

    datatype data
      = LABEL of label
      | MARK
      | LONGconst of int
      | STRINGconst of string
      | REALconst of string
      | ADDRconst of (label * int)

    datatype block =
	CODEBLK  of {instrs: label instruction list, 
		     lo: int,		(* min size (bytes)*)
		     hi:int}		(* max size (bytes)*)
      | DATABLK  of (data * int) list	(* data and its size (bytes) *)
      | SDIBLK   of {sdi:label sdi, 
		     lo:int ref,	(* expansion under min conditions *)
		     hi:int ref,	(* expansion under max conditions *)
		     loLoc:int ref,	(* location counter under min cond *)
	             hiLoc:int ref}	(* location counter under max cond *)
      | BASICBLK of {cblks: block list, (* CODEBLK + SDIBLK *)
		     bbsize: int}	(* size assuming perf.sched+minsdi *)
      | SCHEDBLK of label instruction Vector.vector

    val codeList = ref [DATABLK[(LABEL baseLab,0)]]

    fun reset () = (Label.reset(); codeList := [DATABLK[(LABEL baseLab,0)]])

    fun dataListSize dl =
	let fun sum ([],acc) = acc
	      | sum ((_,size)::dl,acc) = sum(dl,acc+size)
	in sum (dl,0)
	end

    fun emit I = let
	val nopsSize = 4 * mayNeedNop I
	val newCList = 
	     (case !codeList  
		of CODEBLK{instrs,hi,lo}::blks => 
		     CODEBLK{instrs=I::instrs,lo=lo+4,hi=hi+4+nopsSize}::blks
		 | blks => CODEBLK{instrs=[I],lo=4,hi=4+nopsSize}::blks)
      in
	  codeList := (case instrKind I 
	                 of IK_JUMP => DATABLK[]::newCList
		          | _ => newCList)
      end

    fun emitData (D,size) = 
	(codeList := (case !codeList
		      of DATABLK dl::blks => 
			    DATABLK((D,size)::dl)::blks
		       | blks => DATABLK[(D,size)]::blks))

    fun padString s = (case ((size s) mod 4)
	   of 0 => s
	    | 1 => (s ^ "\000\000\000")
	    | 2 => (s ^ "\000\000")
	    | 3 => (s ^ "\000")
	    | _ => error "")


    fun emitLong i = emitData (LONGconst i, 4)

    fun emitString s = let val s' = padString s
		       in
			   emitData (STRINGconst s',size s')
		       end

    exception BadReal of string		(* not used yet! *)

    fun emitReal r = emitData(REALconst r, 8)

    fun emitLabel args = emitData(ADDRconst args, 4)

    fun define (lab) = emitData(LABEL lab, 0)

    fun mark () = emitData(MARK, 4)

    fun emitSDI I = let
	  val nd = SDIBLK{sdi=I,lo=ref 0,hi=ref 0,loLoc=ref 0,hiLoc=ref 0}
	in codeList := (if isSdiBranch I then  DATABLK[]::nd:: !codeList
			else nd:: !codeList)
	end

    val comment = E.comment

fun finish() =
let val labelmap = Array.array(count(),0)
    val labinfo = INFO{addrOf = fn lab => Array.sub(labelmap,(numberOf lab)),
		       nameOf = nameOf}
    val sizeOf = sizeOf labinfo
    val e_define = E.define labinfo
    val e_emitAddr = E.emitAddr labinfo
    val e_emitInstr = E.emitInstr labinfo
    val expand = M.expand labinfo

    (** label calculations **)

    local
      datatype labelExtremes = LO | HI

     (* initialize labels so that they are as close together.
      * returns: size of blks. 
      *)
      fun init([],loc) = loc
	| init(CODEBLK{lo,...}::blks,loc) = init(blks,loc+lo)
	| init(DATABLK dl::blks,loc) = 
	  let fun initData([],loc) = loc
		| initData((LABEL lab,_)::dl,loc) = 
		    (Array.update(labelmap, numberOf lab, loc); initData(dl,loc))
		| initData((_,size)::dl,loc) = initData(dl,loc+size)
	  in
	      init(blks,initData(dl,loc))
	  end
	| init(SDIBLK{sdi,lo,hi,loLoc,...}::blks,loc) = 
	  let val size = M.minSize sdi
	  in lo:=size; hi:=size; loLoc:=loc; init(blks,loc+size)
	  end
	| init(BASICBLK{bbsize,...}::blks,loc) = init(blks, bbsize+loc)
	| init(SCHEDBLK instrs::blks,loc) = init(blks,loc+4*V.length instrs)

     (* fixBlocks
      *    - iterates to a fixpoint computing labels.  uses the function
      * f to adjust basic blocks.  returns size of emitted code.
      * note: side-effects labels only and returns code size.
      *)
      fun adjust(blks,which) = 
      let fun fixBlocks size = 
	  let fun fixLabels([],loc) = loc
		| fixLabels(DATABLK dl::rest,loc) = 
		  let 
		      fun initData([],loc') = loc'
			| initData((LABEL lab,_)::dl,loc') =
			    (Array.update(labelmap, numberOf lab, loc');
			     initData(dl,loc'))
			| initData((_,size)::dl,loc') = initData(dl,loc'+size)
		  in 
		      fixLabels(rest,initData(dl,loc))
		  end
		| fixLabels(BASICBLK{cblks,...}::rest,loc) = 
		    fixLabels(rest,fixLabels(cblks,loc)) 
		| fixLabels(CODEBLK{hi,lo,...}::rest,loc) = 
		  (case which 
		     of HI => fixLabels(rest,hi+loc) 
		      | LO => fixLabels(rest,lo+loc))
		| fixLabels(SDIBLK{sdi,hi,lo,hiLoc,loLoc,...}::rest,loc) = let
		    val (_,size) = sizeOf(sdi,loc)
                  in case which
		       of LO => (loLoc:=loc; 
				 if size > (!lo) then lo :=size else ();
				 fixLabels(rest,loc+Int.max(!lo,size)))
		        | HI => (hiLoc:=loc; loLoc:=loc;
 				 if size > (!hi) then hi :=size else ();
				 lo:= !hi;
				 fixLabels(rest,loc+Int.max(!hi,size)))
	          end
		| fixLabels(SCHEDBLK instrs::rest,loc) =
		    fixLabels(rest,loc+4*V.length instrs)

	      val newSize =  fixLabels(blks,0)
	  in
	      if newSize <> size then  fixBlocks newSize
	      else newSize
	  end
      in fixBlocks (init(blks,0))
     end
    in
	fun adjustLow blks = adjust(blks,LO)
	fun adjustHigh blks = adjust(blks,HI)
    end (*local*)


    (** Instruction scheduling and machine code emission **)

    datatype instr_nd         (* Nodes in the resource dependency graph *)
      = IND of {
	id : int,               	(* unique id for equality testing *)
	instr : label instruction,    	(* The instruction *)
	nsuccs : int,           	(* The number of successors *)
	succs : instr_nd list ref,
	maxpathlen : int,       	(* Length of the longest path to leaf. *)
	npreds : int ref,       	(* The number of predecessors *)
        predLst : instr_nd list ref	(* list of predecessor nodes *)
      }

    fun member (IND{id = x, ...}, lst) = let
	  fun mem nil = false
	    | mem (IND{id = y, ...}::rest) = ((x = y) orelse (mem rest))
	  in
	    mem lst
	  end

    fun merge (nil, lst) = lst
      | merge (nd :: rest, lst) = if (member(nd, lst))
	  then merge (rest, lst)
	  else merge (rest, nd :: lst)



  (* Schedule and emit the instructions of a straight-line block of code. *)    
    fun schedBB (exitInstr, instrs) = let

	val exitDep = case exitInstr
		 of NONE => (fn _ => false)
		  | (SOME e) => let
		      val (exitUses, exitDefs) = rUseDef e
		      val f = List.exists(fn r => (List.exists (fn x => (r = x)) exitUses))
		      val g = List.exists(fn r => (List.exists (fn x => (r = x)) exitDefs))
		      in
			fn I => let
			      val (u, d) = rUseDef I
			      in
				(f d) orelse (g d) orelse (g u)
			      end
		      end

	(* for debugging *)
	fun printDag (n,roots) =  let
	    val visited =  Array.array(n+1,false)
	    fun printSuccs [] = Control.Print.say "\n"
	      | printSuccs (IND nd::rest) = 
		(app Control.Print.say ["(",Int.toString (#id nd),",",
				Int.toString (#nsuccs nd), ",",
				Int.toString (#maxpathlen nd), ",",
				Bool.toString (exitDep (#instr nd)), ",",
				Int.toString (!(#npreds nd)), ")"];
		 printSuccs rest)
	    fun visit (IND nd) = let
		val id = #id nd
	      in
		  app Control.Print.say [Int.toString(#id nd), " :: "];
		  printSuccs(!(#succs nd));
		  Array.update(visited,#id nd,true);
		  app (fn IND nd => if Array.sub(visited,#id nd) then ()
				    else visit (IND nd)) (!(#succs nd))
	      end
	  in
	      app (fn IND nd => if Array.sub(visited,#id nd) then () 
				else visit (IND nd)) roots
	  end

	(* make a new instr_nd *)
       fun mkINd (n, I, nil) =
	     IND{id = n, instr = I,
	       nsuccs = 0, succs = ref nil, 
	       maxpathlen = latency I, npreds = ref 0, predLst=ref nil}
	 | mkINd (n, I, succLst) = let
	     val lat = latency I
	     fun f (nil, len, mpl) = (len, mpl)
	       | f (IND{instr, maxpathlen, ...} :: rest, len, mpl) =
		   f (rest,len+1,if maxpathlen > mpl then  maxpathlen else mpl)
	     val (len, mpl) = f (succLst, 0, 0)
	   in
	       IND{id = n, instr = I,
		   nsuccs = len, succs = ref succLst, 
		   maxpathlen = mpl+lat, npreds = ref 0, predLst=ref nil}
	   end
	(* resource use/def vectors *)
       val lastUse = Array.array (numResources, nil)
       val lastDef = Array.array (numResources, nil)
	(* find resource dependencies *)
       fun findDeps rsrc = let
	   fun add (nil, lst) = lst
	     | add (r :: rest, lst) = 
	       add (rest, merge(Array.sub(rsrc,r), lst))
	 in
	     add
	 end
       val findUseDeps = findDeps lastUse
       val findDefDeps = findDeps lastDef
	(* update resource use/def vectors *)
       fun updateUseDefs nd = let
	   val ndl = [nd]
	   val updateUses =
	         app (fn r => Array.update(lastUse, r, 
					   nd::(Array.sub(lastUse,r))))
	   val updateDefs =
 	         app (fn r => (Array.update(lastDef, r, ndl); 
			       Array.update(lastUse, r, nil)))
	 in
	     fn (ruses, rdefs) => (updateDefs rdefs; updateUses ruses)
	 end
	(* extract the dependency graph roots from the use/def vectors *)
       fun roots () = let
	   fun isRoot (IND{npreds, ...}) = (!npreds = 0)
	   fun rootsOf (nil, lst) = lst
	     | rootsOf (nd::rest, lst) = if (isRoot nd)
					 then rootsOf (rest, nd::lst)
					 else rootsOf (rest, lst)
	   fun mergeRoots (~1, lst) = lst
	     | mergeRoots (i, lst) = let
		 val rlst = rootsOf (merge (Array.sub(lastDef,i), 
					    Array.sub(lastUse,i)), nil)
	       in
		   mergeRoots (i-1, merge (rlst, lst))
	       end
	   fun filterBranch [] = []
	     | filterBranch ((nd as IND{instr,...})::rest) = 
	       if instrKind instr = IK_JUMP then filterBranch rest
	       else nd::filterBranch rest
	 in
	     filterBranch(mergeRoots (numResources-1, nil))
	 end (* roots *)

       fun buildDepGraph instrs = let
	   fun incPreds (nil,_) = ()
	     | incPreds (IND{npreds,predLst,...} :: rest, nd) = 
	         (npreds := !npreds + 1; 
		  predLst:=nd:: (!predLst);
		  incPreds (rest,nd))

	   fun doInstrs (nil, n) = n
	     | doInstrs (I :: rest, n) = 
	       (case (instrKind I)
		  of IK_NOP => ()
	           | _ => 
		     let val (ruses, rdefs) = rUseDef I
			 (* find use/def, def/use and def/def dependencies *)
			 val succLst = findUseDeps (rdefs,
					  findDefDeps(rdefs,
						findDefDeps (ruses, nil)))
			 val nd = mkINd (n, I, succLst)
		     in
			 incPreds (succLst,nd);
			 updateUseDefs nd (ruses, rdefs)
		     end
		     (* end case *);
		     doInstrs (rest, n+1))
	 in
	     doInstrs(instrs,0)
	 end (* buildDepGraph *)

       fun mkNops n = if n <= 0 then [] else nop :: mkNops (n-1)

       fun chooseInstr (nd::ndl) = let
	   fun orderInstrPair (nd1 as IND a, nd2 as IND b) = let
	       fun pathBasis () = let 
		   val p1 = #maxpathlen a
		   val p2 = #maxpathlen b
		 in
		     if p1 = p2 then NONE
		     else if p1 > p2 then SOME(nd1,nd2)
			  else SOME(nd2,nd1)
		 end
	       fun succBasis () = let
		   val n1 = #nsuccs a
		   val n2 = #nsuccs b
		 in
		     if n1 > n2 then SOME(nd1,nd2)
		     else if n1 < n2 then SOME(nd2,nd1)
			  else NONE
		 end
	     in
		 case pathBasis () 
		   of SOME x => x
		    | NONE => 
			(case succBasis () 
			   of SOME x => x
			    | NONE => (nd1,nd2))
	     end
	   fun choose (choice,[],done) = (choice,done)
	     | choose (choice,nd::rest,done) = let
		 val (newChoice,reject) = orderInstrPair (choice,nd)
	       in
		   choose(newChoice,rest,reject::done)
	       end
	 in
	     choose (nd,ndl,[])
	 end

       fun enableSuccs([],[],candidates) = candidates
	 | enableSuccs(IND{succs,...}::rest,[],candidates) =
	     enableSuccs(rest,!succs,candidates)
	 | enableSuccs (nds,(nd as IND{npreds,instr,...})::rest,candidates) =
	   if instrKind instr = IK_JUMP
           then enableSuccs(nds,rest,candidates)
           else let
               val n = !npreds
             in
		 npreds:=n-1;
		 if n=1 then enableSuccs(nds,rest,nd::candidates)
		 else enableSuccs(nds,rest,candidates)
	     end

       (* 
       ** Perform a time simulation of instructions 
       ** to be executed 
       *)
       fun traverse ([],[],cl) = cl
	 | traverse ([], blocked, cl) = let
	    (* no root instructions *)
	     exception Advance

	     fun advance [] = raise Advance 
	       | advance blocked = let
		 val infinity = 1000000
		 fun findMin([],ans) = ans
		   | findMin((_,t,lat)::rest,ans) = 
		     if lat-t < ans then findMin(rest,lat-t) 
		     else findMin(rest,ans)
		 fun advanceby(_,[],acc,blked) = (acc,blked)
		   | advanceby(N,(nd,t,lat)::rest,acc,blked) =
		     if t+N >= lat then advanceby(N,rest,nd::acc,blked)
		     else advanceby(N,rest,acc,(nd,t+N,lat)::blked)
	       in
		   advanceby(findMin(blocked,infinity),blocked,[],[])
	       end

	     val (nds,blocked') = advance blocked
	     val candidates = enableSuccs(nds,[],[])
	   in
	       traverse(candidates,blocked',cl)
	   end
	 | traverse (candidates,[],cl) = let
	     val (nd as(IND{instr,succs,...}),newCandidates) =
		    chooseInstr candidates
	     val newCl = instr::cl
	     val instrLat = latency instr
           in
	      if instrLat = 1 
	      then let 
                  val newCandidates' = enableSuccs([nd],[],newCandidates)
                in
		    traverse(newCandidates',[],newCl)
                end
	      else traverse(newCandidates,[(nd,0,instrLat)],newCl)
           end
	 | traverse (candidates,blocked,cl) = let
	     fun executeBlocked [] = ([],[])
	       | executeBlocked blocked = let
		   fun tick ([],ndl,blocked) = (ndl,blocked)
		     | tick ((nd,t,lat)::rest,ndl,blocked) =
			if t+1 >= lat then tick(rest,nd::ndl,blocked)
			else tick(rest,ndl,(nd,t+1,lat)::blocked)
		 in
		    tick(blocked,[],[])
		 end
	     val (nds,blocked') = executeBlocked blocked
	     val (nd as(IND{instr,succs,...}), newCandidates) = 
 		    chooseInstr (enableSuccs(nds,[],candidates))
	     val instrLat = latency instr
           in
	       traverse(newCandidates,(nd,0,instrLat)::blocked',instr::cl)
	   end

	fun findDelaySlotInstr roots = let
	    val visited = Array.array(length instrs+1,false)

	    fun found (IND node) = let 
		fun nopFree [] = true
		  | nopFree (IND x::xs) = 
		    if mayNeedNop(#instr x)>1 then false else nopFree xs
	      in
		  (#nsuccs node) = 0  
		  andalso latency (#instr node) <= 1 
		  andalso instrKind (#instr node) <> IK_JUMP 
		  andalso nopFree (!(#predLst node)) 
		  andalso not (exitDep (#instr node))
	      end

	    fun visit (IND node) = 
		if found (IND node) then SOME(IND node)
		else let val adj = #succs node
		     in
			 Array.update(visited,#id node,true);
			 travRoots (!adj)
		     end

	    and travRoots [] = NONE
	      | travRoots (IND nd::rest) =
		if Array.sub(visited,#id nd) then travRoots rest
		else (case visit (IND nd) 
		        of NONE => travRoots rest
		         | SOME n => SOME n)

	   (** deletes branch delay instruction from DAG **)
	    fun deleteBranchDelayInstr (IND{id,predLst,...}) = let
		exception BranchDelay
		fun delete [] = []
		  | delete ((x as IND{id=id',...})::xs) = 
		    if id = id' then xs else x::delete xs
	      in
		  app (fn IND{succs,...} => succs := delete (!succs)) 
		      (!predLst)
	      end

	    fun newRoots (IND ds, roots) = let
		fun del ([],acc) = acc
		  | del (IND nd::rest, acc) =
		    if #id nd = #id ds then rest@acc
		    else del(rest, IND nd::acc)
	      in
		  del(roots,[])
	      end
	  in
	      case travRoots roots
		of NONE 	=> (nop,roots)
	         | SOME(IND nd)	=> (deleteBranchDelayInstr (IND nd);
				    (#instr nd,newRoots(IND nd,roots)))
	  end(* findDelaySlotInstr *)

      (* inserts nops and reverses the instruction stream *)
	fun insertNops instrs = let
	    fun insert ([],acc) = acc
	      | insert (x::xs,acc) = let
		  val n = needsNop(x,xs) 
	        in
		    if n > 0 then insert(xs,mkNops n @(x::acc))
		    else insert(xs,x::acc)
	        end
          in
	      insert(instrs,[]) 
          end

	fun assignOrder () = let
	      val roots = roots()
	    in
	      case roots
		of [] => let val SOME e = exitInstr
			 in if branchDelayedArch then [nop, e] else [e]
			 end
		 | nds => 
		     (case exitInstr 
			of NONE => let val cl = traverse(roots,[],[])
				       val n = mayNeedNop (hd cl)
				   in mkNops n @ cl
				   end
			 | SOME e => 
			   if branchDelayedArch 
			   then let
			       val (ds,roots') = findDelaySlotInstr roots
			       val cl = traverse(roots',[],[])
			     in
			       ds::e::cl
			     end
			   else  e::traverse(roots,[],[]))

	    end
	  val allInstrs = case exitInstr of NONE => instrs
				          | SOME e => e::instrs
	  val ndcount = buildDepGraph allInstrs
(*        val _ = (printDag(ndcount,roots()); Control.Print.say "\n\n")   *)
      in
	  insertNops(assignOrder ())
      end (* schedBB *)

    fun schedule blks = let 
	fun isStable (BASICBLK{cblks,...}) =
	    let fun check [] = true
		  | check (CODEBLK _ ::blks) = check blks
		  | check (SDIBLK{lo,hi,...}::blks) = 
		      if (!lo) <> (!hi) then false else check blks
		  | check _ = error "Coder.isStable.check:"
	    in 
		check cblks
	    end
	  | isStable _ = error "Coder.isStable:"

       (* replace stable blocks with their scheduled code sequence *)
	fun schedStableBBs blks = let
	    fun collectCodeLists ([],[],acc) = acc
	      | collectCodeLists ([],codeList,acc) = let
		  val l = case codeList
		            of I::rest => (case instrKind I
					     of IK_JUMP => (SOME I,rest)
					      | _ 	=> (NONE,codeList))
			     | _       => (NONE,codeList)
                in
		    l::acc
                end
	      | collectCodeLists (CODEBLK{instrs,...}::blks,l,acc) =
		  collectCodeLists (blks,instrs@l,acc)
	      | collectCodeLists (SDIBLK{sdi,lo,loLoc,...}::blks,l,acc) = let
		  fun insertSdiCode ([],l,acc) = collectCodeLists(blks,l,acc)
		    | insertSdiCode (i::instrs,l,acc) = 
		      (case instrKind i
			 of IK_JUMP => insertSdiCode(instrs,[],(SOME i,l)::acc)
			  | _ 	  => insertSdiCode(instrs,i::l,acc))
		in
		    insertSdiCode(expand(sdi,!lo,!loLoc),l,acc)
		end
	      | collectCodeLists _ = error "collectCodeLists:"

	    fun schedCodeLists ([],schd) = schd
	      | schedCodeLists (cl::codeList,schd) = 
		  schedCodeLists(codeList,schedBB cl @ schd)

	    fun sched ([],bl,unstab,stab) = (rev bl,unstab,stab)
	      | sched ((blk as DATABLK dl)::rest,blks,u,s) = 
		  sched(rest,blk::blks,u,s)
	      | sched ((blk as SCHEDBLK _)::rest,blks,u,s) = 
		  sched(rest,blk::blks,u,s)
	      | sched ((blk as BASICBLK bb)::rest,blks,u,s) =
		if not(isStable blk) 
		    then sched(rest,blk::blks,u+1,s)
		else 
                  let val codeList = collectCodeLists(#cblks bb,[],[])
		      val sch'd = schedCodeLists(codeList,[])
		      val newBlk = SCHEDBLK (V.fromList sch'd)
		  in 
		      sched(rest,newBlk::blks,u,s+1)
		  end
	      | sched _ = error "Coder.schedule.schedStableBBs.sched:"

	  in
	      sched(blks,[],0,0)
	  end (* schedStableBBs *)

        fun schedUnstableBBs blks = let
	    fun stabBB (BASICBLK{cblks,...}) = let
		  fun f [] =  ()
		    | f (CODEBLK _ ::blks) = f blks	 
		    | f (SDIBLK{lo,hi,...}::blks) = 
		      (if !lo > !hi then hi := !lo else lo := !hi; 
			   f blks)
		    | f _ = error "Coder.schedUnstableBB.stabBB.f"
		in 
		    f cblks
		end
	      | stabBB _ = ()
	    val _ = app stabBB blks
	    val (newBlks,_,_) = schedStableBBs blks
          in
	      newBlks
	  end

        fun schedLoop blks = let
	    val _ = adjustHigh blks
	    val _ = adjustLow blks
	    val (newBlks,nUnstab,nStab) = schedStableBBs blks
          in
	      if nUnstab = 0 then newBlks
	      else if nStab <> 0 then schedLoop newBlks
		   else schedUnstableBBs newBlks
	  end
    in
	schedLoop blks
    end (* schedule *)

    fun noSched blks = let
	fun noSched ([],sched'd) = rev sched'd
	  | noSched ((dl as DATABLK _)::blks,sched'd) = noSched(blks,dl::sched'd)
	  | noSched (BASICBLK{cblks,...}::blks,sched'd) = 
	    let
		fun insertInstr([],acc) = acc
		  | insertInstr(i::rest,acc) = let
		      fun mkNops 0 = []
			| mkNops n = nop::mkNops(n-1)
		      val acc' = i::(mkNops(needsNop(i,acc)) @ acc)
		    in
			case instrKind i
			  of IK_JUMP => if branchDelayedArch 
					then insertInstr(rest,nop::acc')
					else insertInstr(rest,acc')
			   | _ 	     => insertInstr(rest,acc')
                    end
		fun noSchedBB([],acc) = rev acc
		  | noSchedBB(CODEBLK{instrs,...}::blks,acc) = 
		    noSchedBB(blks,insertInstr(rev instrs,acc))
		  | noSchedBB(SDIBLK{sdi,hi,hiLoc,...}::blks,acc) =
		    noSchedBB(blks,insertInstr(expand(sdi,!hi,!hiLoc),acc))
		  | noSchedBB _ = error "Coder.noSched.noSchedBB"
		val newInstrs = noSchedBB(cblks,[])
		val newBlk = SCHEDBLK (V.fromList newInstrs)
	    in
		noSched(blks,newBlk::sched'd)
	    end
	  | noSched _ = error "Coder.noSched"
	val _ = adjustHigh blks
      in
	  noSched(blks,[])
      end

      fun mkBasicBlocks blks = let
	   fun collect(nil,acc,size) = (acc,nil,size)
	     | collect(blk::blks,acc,size) =
	       case blk
	         of SDIBLK{sdi,...} => collect(blks,blk::acc,size+minSize sdi)
		  | CODEBLK{instrs,lo,...} =>
		      collect(blks,blk::acc,size+lo)
  	  	  | _ => (acc,blk::blks,size)
	   fun mkBBs([],acc) = acc
	     | mkBBs(blk::blks,acc) = let
	         fun f initSize = let
		         val (cbs,rest,size) = collect(blks,[blk],initSize)
		       in
			   mkBBs(rest,BASICBLK{cblks=cbs,bbsize=size}::acc)
		       end
                   in
		       case blk
			 of DATABLK dl => mkBBs(blks,DATABLK(rev dl)::acc)
		          | CODEBLK{instrs,lo,...} => f lo
			  | SDIBLK{sdi,...} => f (minSize sdi)
			  | _ => error "coder/coder/mkBasicBlocks"
                   end
         in
	     mkBBs(blks,[])
	 end

       fun sched cl =  if (!Control.CG.scheduling) then schedule cl
		       else noSched cl
       fun emitDataList nil = ()
	 | emitDataList (d :: rest) = (case (#1 (d:(data*int)))
		of (LABEL lab) => e_define lab
		 | MARK => E.mark ()
		 | (LONGconst n) => E.emitLong n
		 | (STRINGconst s) => E.emitString s
		 | (REALconst r) => E.emitReal r
		 | (ADDRconst args) => e_emitAddr args
	       (* end case *);
	       emitDataList rest)
       fun emitInstructions instrs = let
	     val len = V.length instrs
	     fun iter i = if i = len then ()
			else (e_emitInstr (V.sub(instrs,i)); iter (i+1))
           in
	       iter 0
           end
       fun emitBlk [] = ()
	 | emitBlk (SCHEDBLK instrs::blks) = (emitInstructions instrs;
					      emitBlk blks)
	 | emitBlk (DATABLK dl::rest) = (emitDataList dl; emitBlk rest)
	 | emitBlk _ = (ErrorMsg.impossible "[Coder.finish.emitBlk]")
       fun schedSize ([],acc) = acc
	 | schedSize (SCHEDBLK instrs::blks,acc) = 
	     schedSize(blks,acc+4*V.length instrs)
	 | schedSize (DATABLK dl::blks,acc) = 
	     schedSize(blks,acc+dataListSize dl)
	 | schedSize _ = error "Coder.finish.sched'dSize:"

       val schedBlocks = sched (mkBasicBlocks(!codeList before codeList:=[]))
       val _ = adjustLow schedBlocks
     in
	 E.init (schedSize(schedBlocks,0));
	 emitBlk schedBlocks;
	 reset()
     end (* finish *)

val finish = Stats.doPhase (Stats.makePhase "Compiler 130 Schedule") finish

end


(*
 * $Log: coder.sml,v $
 * Revision 1.2  1997/01/31 20:39:56  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:29  george
 *   Version 109.24
 *
 *)
