(*
 * This is a hack module for removing dead code that are discovered by 
 * the register allocator.  This module acts as a wrapper
 * for the generic RA flowgraph module.
 *
 * -- Allen
 *)

functor RADeadCodeElim
   (Flowgraph : RA_FLOWGRAPH)
   ((* check for dead code on these cellkinds only *)
    val cellkind : Flowgraph.I.C.cellkind -> bool
    val deadRegs : bool Intmap.intmap (* Dead registers are stored here. *)
    val affectedBlocks : bool Intmap.intmap (* Affected blocks *)
   ) : RA_FLOWGRAPH =
struct
   structure F = Flowgraph

   open F

   val mode = RACore.SAVE_COPY_TEMPS

   (*
    * New services that also removes dead code 
    *)
   fun services f =
   let val {build, spill, blockNum, instrNum, programPoint} = F.services f
       (* 
        * The following build method marks all pseudo registers
        * that are dead, and record their definition points.
        *)
       fun findDeadCode(G.GRAPH{nodes, copyTmps, ...}) = 
       let val dead     = Intmap.add deadRegs 
           val affected = Intmap.add affectedBlocks
           val affectedList = app (fn d => affected(blockNum d, true))

           (* Mark all copy temporaries *)
           val marker = [0]
           fun markCopyTmps [] = ()  
             | markCopyTmps(G.NODE{uses, ...}::tmps) =
                 (uses := marker; markCopyTmps tmps)
           fun unmarkCopyTmps [] = ()
             | unmarkCopyTmps(G.NODE{uses, ...}::tmps) =
                 (uses := []; unmarkCopyTmps tmps)

           fun enter(_, G.NODE{uses=ref [], defs, number=reg, ...}) =
               (* This is dead, but make sure it is not a copy temporary.
                * Those cannot be eliminated.
                *)
                (affectedList (!defs); dead(reg, true))
             | enter _ = ()

       in  markCopyTmps(!copyTmps);
           Intmap.app enter nodes;
           unmarkCopyTmps(!copyTmps);
           copyTmps := []
       end

       (*
        * Build the graph, then remove dead code.
        *)
       fun buildIt(graph, kind) =  
       let val moves = build(graph, kind)
       in  if cellkind kind then findDeadCode(graph) else ();
           moves
       end
   in  {build=buildIt, spill=spill, programPoint=programPoint,
        blockNum=blockNum, instrNum=instrNum}
   end

end
