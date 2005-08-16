functor SpillTable(MachSpec : MACH_SPEC) : 
sig

   val spillInit  : unit -> unit
   val getRegLoc  : RAGraph.spillLoc -> int
   val getFregLoc : RAGraph.spillLoc -> int

end =
struct

   structure G = RAGraph

   fun error msg = MLRiscErrorMsg.error(MachSpec.architecture^".SpillTable",msg)
  
   val itow = Word.fromInt

   exception RegSpills and FregSpills
   val spillOffset = ref MachSpec.initialSpillOffset
   val regspills : int G.SpillLocHashTable.hash_table =
       G.SpillLocHashTable.mkTable(0,RegSpills)
   val fregspills : int G.SpillLocHashTable.hash_table =
       G.SpillLocHashTable.mkTable(0,FregSpills)
   val lookupReg  = G.SpillLocHashTable.lookup regspills
   val enterReg   = G.SpillLocHashTable.insert regspills
   val lookupFreg = G.SpillLocHashTable.lookup fregspills
   val enterFreg  = G.SpillLocHashTable.insert fregspills

   fun spillInit() =
      ((* Reset the regspills/fregspills map by need. *)
       if !spillOffset = MachSpec.initialSpillOffset then ()
       else (G.SpillLocHashTable.clear regspills;
             G.SpillLocHashTable.clear fregspills
            )
       ;
       spillOffset := MachSpec.initialSpillOffset
      )

   fun newOffset offset =
       if offset >= MachSpec.spillAreaSz then error "spill area too small"
       else spillOffset := offset

   (* Get spill location for integer registers *)
   fun getRegLoc loc =
       lookupReg loc handle _ =>
       let val offset = !spillOffset
       in  newOffset(offset+4);
           enterReg (loc,offset);
           offset
       end

   (* Get spill location for floating point registers *)
   fun getFregLoc loc =
       lookupFreg loc handle _ =>
       let val offset = !spillOffset
           val aligned = Word.toIntX (Word.andb(itow (offset+7), itow ~8))
       in
           newOffset(aligned+8);
           enterFreg (loc, aligned);
           aligned
       end

end
