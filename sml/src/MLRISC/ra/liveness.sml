(* liveness.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** liveness.sml - computes live variables **)


(* I've moved the parameters of the functor to the function arguments 
 * so that it is more flexible.
 *
 * -- Allen 4/28/00
 *)

(* TODO: The liveness module should take a 
 *  C.cellset list IntHashTable.hash_table  
 *)

signature LIVENESS = sig

  structure CFG : CONTROL_FLOW_GRAPH

  type liveness_table = 
         CellsBasis.SortedCells.sorted_cells IntHashTable.hash_table

  val liveness : {
	  defUse : CFG.I.instruction
			-> (CellsBasis.cell list * CellsBasis.cell list),
	  getCell    : CellsBasis.CellSet.cellset -> CellsBasis.cell list 
	} -> CFG.cfg 
	    -> {liveIn  : liveness_table,
                liveOut : liveness_table
               }

end


functor Liveness(Flowgraph : CONTROL_FLOW_GRAPH) : LIVENESS = struct
  structure CFG = Flowgraph
  structure I   = CFG.I
  structure SC  = CellsBasis.SortedCells	
  structure CS  = CellsBasis.CellSet
  structure HT  = IntHashTable
  structure G   = Graph

  type liveness_table = SC.sorted_cells HT.hash_table

  fun error msg = MLRiscErrorMsg.error("Liveness",msg)

  val NotFound = General.Fail("Liveness: Not Found")		(* exception *)

  fun prList(l,msg:string) = let
      fun pr([]) = print "\n"
        | pr(x::xs) = (print(Int.toString x ^ " "); pr xs)
    in print msg; pr l
    end

  fun liveness {defUse,getCell} = let
    val getCell = SC.uniq o getCell

    fun dataflow (cfg as G.GRAPH graph) = let
      val blocks = #nodes graph ()
      val nNodes = #order graph ()

      val liveIn : SC.sorted_cells  HT.hash_table = HT.mkTable(nNodes, NotFound)
      val liveOut : SC.sorted_cells HT.hash_table = HT.mkTable(nNodes, NotFound)
      val uses : SC.sorted_cells HT.hash_table = HT.mkTable(nNodes, NotFound)
      val defs : SC.sorted_cells HT.hash_table = HT.mkTable(nNodes, NotFound)

      (* compute block aggregate definition use. *)
      fun initDefUse(nid, CFG.BLOCK{insns, ...}) = let
	fun defuse (insn::insns,def,use) = let
	      val (d,u) = defUse insn
	      val u' = SC.difference(SC.uniq u,def)
	      val use' = SC.union(u', use)
	      val d' = SC.difference(SC.uniq d,use')
	    in defuse(insns, SC.union(d',def), use')
	    end
	  | defuse([],def,use) = 
	      (HT.insert uses (nid, use);  HT.insert defs (nid, def))
      in
	defuse(rev(!insns), SC.empty, SC.empty)
      end

      (* gather the liveOut information *)
      fun initLiveOut(nid, CFG.BLOCK{annotations, ...}) = 
	(case #get CFG.LIVEOUT (!annotations)
	  of NONE => HT.insert liveOut (nid, SC.empty)
	   | SOME cs => HT.insert liveOut (nid, getCell cs)
        (*esac*))


      fun initLiveIn () = 
	#forall_nodes graph (fn (nid, _) => HT.insert liveIn (nid, SC.empty))

      fun init() = ( 
	    #forall_nodes graph initDefUse;  
	    #forall_nodes graph initLiveOut;
	    initLiveIn())

      fun inB(nid) = let
	val use = HT.lookup uses nid 
	val def = HT.lookup defs nid
	val liveOut = HT.lookup liveOut nid
        val livein = SC.union(use, SC.difference(liveOut, def))
        val changed = SC.notEq(HT.lookup liveIn nid, livein)
      in
	HT.insert liveIn (nid, livein); changed
      end


      fun outB(nid, CFG.BLOCK{annotations, ...}) = let
	fun inSucc([], acc) = acc
	  | inSucc(nid::ns, acc) = 
	     inSucc(ns, SC.union(HT.lookup liveIn nid, acc))
	val oldLiveOut = HT.lookup liveOut nid 
	val newLiveOut = inSucc(#succ graph nid, SC.empty)
      in 
	HT.insert liveOut (nid, newLiveOut);
	SC.notEq(oldLiveOut, newLiveOut)
      end

      fun bottomup() = let
	  val visitedTbl : bool HT.hash_table = HT.mkTable(nNodes, NotFound)
	  fun isVisited nid = 
	    (case HT.find visitedTbl nid of NONE => false | _ => true)
	  fun visit(nid, changed) = let
	    fun visitSucc([], changed') = changed'
	      | visitSucc(nid::ns, changed') = let
		  val CFG.BLOCK{kind, ...} = #node_info graph nid
		in case kind
		    of CFG.STOP => visitSucc(ns, changed')
		     | CFG.NORMAL =>
			if isVisited nid then visitSucc(ns, changed')
			else visitSucc(ns, visit(nid, changed'))
		     | _ => error "visit.visitSucc"
		end

	    val _ = HT.insert visitedTbl (nid, true)

	    val changed' = visitSucc(#succ graph nid, changed);
	    val block = #node_info graph nid
	    val change1 = outB(nid, block)
	    val change2 = inB(nid)
	  in
	    changed' orelse change1 orelse change2
	  end

	  fun forall([], changed) = changed
	    | forall((nid,block)::rest, changed) = 
	       if isVisited(nid) then forall(rest, changed)
	       else forall(rest, visit(nid, changed))
	in
	  forall(blocks, false)
	end 

      fun repeat n = if bottomup() then repeat(n+1) else (n+1)

    in 
      init(); repeat 0; {liveIn=liveIn, liveOut=liveOut}
    end  

  in dataflow
  end
end

