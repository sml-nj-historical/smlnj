(* schedule.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** schedule.sml - postpass scheduling **)
    
functor Scheduler
    (structure InsnProps : INSN_PROPERTIES
     val icount : int -> InsnProps.I.instruction) : SCHEDULER =
struct
  structure P = InsnProps
  structure I = P.I

  val vsub = Vector.sub

  datatype instr_nd             (* Nodes in the resource dependency graph *)
    = IND of {
      id : int,    	        (* unique id for equality testing *)
      instr : I.instruction,    (* The instruction *)
      nsuccs : int,           	(* The number of successors *)
      succs : instr_nd list ref,
      maxpathlen : int,       	(* Length of the longest path to leaf. *)
      npreds : int ref,       	(* The number of predecessors *)
      predLst : instr_nd list ref (* list of predecessor nodes *)
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

  fun schedBB(instrs,regmaps) = let
      val regmaps = map ! regmaps
       (* make a new instr_nd *)
      fun mkINd (n, I, nil) =
	    IND{id = n, instr = I,
	      nsuccs = 0, succs = ref nil, 
	      maxpathlen = P.latency I, npreds = ref 0, predLst=ref nil}
	| mkINd (n, I, succLst) = let
	    val lat = P.latency I
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
      val lastUse = Array.array (P.numResources, nil)
      val lastDef = Array.array (P.numResources, nil)

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
	      if P.instrKind instr = P.IK_JUMP then filterBranch rest
	      else nd::filterBranch rest
	in
	    filterBranch(mergeRoots (P.numResources-1, nil))
	end (* roots *)

      val defUse = P.bdefUse regmaps

      fun buildDepGraph instrs = let
	  fun incPreds (nil,_) = ()
	    | incPreds (IND{npreds,predLst,...} :: rest, nd) = 
		(npreds := !npreds + 1; 
		 predLst:=nd:: (!predLst);
		 incPreds (rest,nd))

	  val maxIndx = Vector.length instrs
	  fun iter n = if n = maxIndx then ()
		       else let 
			    val insn = Vector.sub(instrs,n)
			    val (defs,uses) = defUse insn
			    val succLst = findUseDeps (defs,
					     findDefDeps(defs,
						 findDefDeps(uses,nil)))
			    val nd = mkINd (n, insn, succLst)
			  in
			      incPreds(succLst,nd);
			      updateUseDefs nd (uses,defs);
			      iter (n+1)
			  end
	in
	    iter 0
	end (* buildDepGraph *)

      fun mkNops n = if n <= 0 then [] else P.nop()::mkNops (n-1)

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
	  if P.instrKind instr = P.IK_JUMP
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
	    val instrLat = P.latency instr
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
	    val instrLat = P.latency instr
	  in
	      traverse(newCandidates,(nd,0,instrLat)::blocked',instr::cl)
	  end

       fun findDelaySlotInstr roots = let
	   val visited = Array.array(Vector.length instrs+1,false)

	   fun found (IND node) = let 
	       fun nopFree [] = true
		 | nopFree (IND x::xs) = 
		   if P.mayNeedNops(#instr x)>=1 then false else nopFree xs
	     in
		 (#nsuccs node) = 0  
		 andalso P.latency (#instr node) <= 1 
		 andalso P.instrKind (#instr node) <> P.IK_JUMP 
		 andalso nopFree (!(#predLst node)) 
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
	       of NONE 	=> (P.nop(),roots)
		| SOME(IND nd)	=> (deleteBranchDelayInstr (IND nd);
				   (#instr nd,newRoots(IND nd,roots)))
	 end(* findDelaySlotInstr *)

        (* inserts nops and reverses the instruction stream *)
       fun insertNops instrs = let
	   fun insert ([],acc) = acc
	     | insert (x::xs,acc) = let
		 val n = P.needsNop(x,xs) 
	       in
		   if n > 0 then insert(xs,mkNops n @(x::acc))
		   else insert(xs,x::acc)
	       end
	 in
	     insert(instrs,[])
	 end

       fun assignOrder () = let
	   val roots = roots()
	   val exitInstr = let val insn = Vector.sub(instrs,0) 
	       in
		 if P.instrKind insn <> P.IK_JUMP then NONE
		 else SOME insn
	       end
	   fun icounting() = false
	 in
	   case roots
	     of [] => let val SOME e = exitInstr
		 in 
		     if P.branchDelayedArch then 
			if icounting() then
			    [icount 3,e]
			else 
			    [P.nop(),e]
		     else
			 if icounting() then
			     [e,icount 2]
			 else
			     [e]
		 end 
	      | nds => 
		(case exitInstr 
		 of NONE => let 
		       val cl = traverse(roots,[],[])
		       val n = P.mayNeedNops (hd cl)
		    in 
			if icounting() then
			    icount(1+n+length cl)::(mkNops n @ cl)
			else
			    mkNops n @ cl
		    end
		  | SOME e => 
		    if P.branchDelayedArch then let
			  val (ds,roots') = findDelaySlotInstr roots
			  val cl = traverse(roots',[],[])
                       in
			   if icounting() then 
			       ds::e::icount(3+length cl)::cl
			   else
			       ds::e::cl
		       end
		    else let
			  val cl = traverse(roots,[],[])
		       in
			   if icounting() then
			       e::icount(length cl+2)::cl
			   else
			       e::cl
		       end)
	 end
       val ndcount = buildDepGraph instrs
    in
	Vector.fromList (insertNops(assignOrder ()))
    end (* schedBB *)

  fun schedule(instrs,regmaps) = let
      fun noSched(~1,acc) = Vector.fromList(rev acc)
	| noSched(n,acc) = let
	    fun mkNops 0 = []
	      | mkNops n = P.nop()::mkNops(n-1)
	    val insn = vsub(instrs,n) 	
	    val acc' = insn :: (mkNops(P.needsNop(insn,acc)) @ acc)
	  in case P.instrKind insn
	     of P.IK_JUMP => 
		 if P.branchDelayedArch then noSched(n-1,P.nop()::acc')
		 else  noSched(n-1,acc')
	     | _ => noSched(n-1,acc')
	  end
    in
      if Vector.length instrs = 0 then Vector.fromList [] 
      else if Word.andb(Word.fromInt(!Control.CG.misc4), 0w1024) <> 0w0 
	   then schedBB(instrs,regmaps)
	   else noSched(Vector.length instrs-1,[]) 
    end (* schedule *)
end

(*
 * $Log$
 *)
