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

signature LIVENESS = sig

  structure F : FLOWGRAPH
  structure I : INSTRUCTIONS
  structure C : CELLS
    sharing F.I = I
    sharing I.C = C

  val liveness : 
      { defUse     : I.instruction -> C.cell list * C.cell list,
        updateCell : C.cellset * C.cell list -> C.cellset,
        getCell    : C.cellset -> C.cell list,
        blocks     : F.block list
      } -> F.block list
end


functor Liveness(Flowgraph : FLOWGRAPH) : LIVENESS = 
struct

  structure F  = Flowgraph
  structure I  = F.I
  structure C  = I.C
  structure SC = C.SortedCells

  fun error msg = MLRiscErrorMsg.error("Liveness",msg)

  fun prList(l,msg:string) = let
      fun pr([]) = print "\n"
        | pr(x::xs) = (print(Int.toString x ^ " "); pr xs)
    in print msg; pr l
    end

  fun liveness{defUse,getCell,updateCell,blocks} = let
      val getCell = SC.uniq o getCell
      fun codeBlocks [] = []
        | codeBlocks((blk as F.BBLOCK _)::blks) = blk::codeBlocks blks
        | codeBlocks(_::blks) = codeBlocks blks

      fun dataflow blkArr = let
          val M                                    = Array.length blkArr
          val useArr : SC.sorted_cells Array.array = Array.array(M,SC.empty)
          val defArr : SC.sorted_cells Array.array = Array.array(M,SC.empty)

          fun init ~1 = ()
            | init n  = let
                val F.BBLOCK{blknum,insns,liveIn,...} = Array.sub(blkArr,n)
                fun defuse(insn::insns,def,use) = let
                      val (d,u) = defUse insn
                      val u' = SC.difference(SC.uniq u,def)
                      val use' = SC.union(u', use)
                      val d' = SC.difference(SC.uniq d,use')
                    in
                      defuse(insns, SC.union(d',def), use')
                    end
                  | defuse([],def,use) = 
                      (Array.update(useArr,blknum,use);
                       Array.update(defArr,blknum,def))
              in
                  defuse(rev(!insns),SC.empty,SC.empty);
                  liveIn := updateCell(!liveIn,[]);
                  init(n-1)
              end

          fun outB(F.BBLOCK{succ=ref [], ...}) = false
            | outB(F.BBLOCK{succ=ref [(F.EXIT _,_)], ...}) = false
            | outB(F.BBLOCK{succ, liveOut,...}) = let
                fun inSuccs([], acc) = acc
                  | inSuccs((F.EXIT _,_)::sl, acc) = inSuccs(sl, acc)
                  | inSuccs((F.BBLOCK{blknum,liveIn,...},_)::sl, acc) = 
                      inSuccs(sl, SC.union(getCell(!liveIn), acc))
                val liveout = inSuccs(!succ, SC.empty)
                val change = SC.notEq(getCell(!liveOut),liveout)
              in liveOut:= updateCell(!liveOut,SC.return liveout); change
              end
            | outB _ = error "liveness.dataflow.outB"

          fun inB(F.BBLOCK{blknum,liveIn,liveOut,...}) = let
              val use    = Array.sub(useArr,blknum)
              val def    = Array.sub(defArr,blknum)
              val livein = SC.union(use, SC.difference(getCell(!liveOut),def))
              val change = SC.notEq(getCell(!liveIn),livein)
            in
              liveIn := updateCell(!liveIn, SC.return livein); change
            end
            | inB _ = error "liveness.dataflow.inB"

          fun bottomup() = let
              val visited = Array.array(M,false)
              fun visit(n, changed) = let
                  fun visitSucc([],changed') = changed'
                    | visitSucc((F.EXIT _,_)::ns, changed') =
                       visitSucc(ns, changed')
                    | visitSucc((F.BBLOCK{blknum=n, ...},_)::ns,changed') =
                       if Array.sub(visited,n) then visitSucc(ns,changed')
                       else visitSucc(ns,visit(n,changed'))

                  val block as(F.BBLOCK{succ,...}) = Array.sub(blkArr, n)
                  val _ = Array.update(visited,n,true)

                  val changed' = visitSucc(!succ,changed);
                  val change1 = outB block
                  val change2 = inB block
                in
                  changed' orelse change1 orelse change2
                end
              fun visitAll(n,last,changed) = 
                  if n=last then changed
                  else if Array.sub(visited,n) then visitAll(n+1,last,changed)
                       else visitAll(n+1,last,visit(n,changed))
            in
              visitAll(0,M,false)
            end

          fun repeat n = if bottomup() then repeat(n+1) else (n+1)
        in
            init (M-1); repeat 0
        end  
    in 
        dataflow (Array.fromList (codeBlocks blocks));
        blocks
    end
end


