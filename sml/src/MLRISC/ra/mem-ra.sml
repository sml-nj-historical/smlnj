(* 
 * This module implements the memory coalescing capability of the 
 * register allocator.
 *)
functor MemoryRA(Flowgraph : RA_FLOWGRAPH) : RA_FLOWGRAPH =
struct

  structure G = RAGraph
  structure A = Array
  structure W = Word

  open G RACore

  val ra_spill_coal = MLRiscControl.getCounter "ra-spill-coalescing"
  val ra_spill_prop = MLRiscControl.getCounter "ra-spill-propagation"

  local

  fun error msg = MLRiscErrorMsg.error("RACore", msg)
 
  (* No overflow checking necessary here *)
  fun x + y = W.toIntX(W.+(W.fromInt x, W.fromInt y))
  fun x - y = W.toIntX(W.-(W.fromInt x, W.fromInt y))

  fun concat([], b) = b
    | concat(x::a, b) = concat(a, x::b)

  fun chase(NODE{color=ref(ALIASED n),...}) = chase n
    | chase n = n

  in

  fun isOn(flag,mask) = Word.andb(flag,mask) <> 0w0

  (*
   * Spill coalescing.
   * Coalesce non-interfering moves between spilled nodes, 
   * in non-increasing order of move cost.
   *)
  fun spillCoalescing(GRAPH{bitMatrix, ...}) =
  let val member = BM.member(!bitMatrix)
      val addEdge = BM.add(!bitMatrix)
  in  fn nodesToSpill =>
      let 
          (* Find moves between two spilled nodes *)
          fun collectMoves([], mv') = mv'
            | collectMoves(NODE{movelist, color=ref(SPILLED _), ...}::ns, mv') =
              let fun ins([], mv') = collectMoves(ns, mv')
                    | ins(MV{status=ref(COALESCED | CONSTRAINED), ...}::mvs,
                          mv') = ins(mvs, mv')
                    | ins((mv as MV{dst, src, ...})::mvs, mv') =
                       (case (chase dst, chase src) of
                          (NODE{color=ref(SPILLED x), number=d, ...},
                           NODE{color=ref(SPILLED y), number=s, ...}) =>
                              if d = s orelse            (* trival move *)
                                 (x >= 0 andalso y >= 0) (* both are fixed *)
                              then ins(mvs, mv')
                              else ins(mvs, MV.add(mv, mv'))
                        | _ => ins(mvs, mv')
                       )
              in  ins(!movelist, mv') end
            | collectMoves(_::ns, mv') = collectMoves(ns, mv')

          val mvs = collectMoves(nodesToSpill, MV.EMPTY)

          (* Coalesce moves between two spilled nodes *)
          fun coalesceMoves(MV.EMPTY) = ()
            | coalesceMoves(MV.TREE(MV{dst, src, cost, ...}, _, l, r)) =
              let val dst as NODE{color=colorDst, ...} = chase dst
                  val src = chase src
    
                  (* Make sure that dst is the non-mem reg node *)
                  val (dst, src) =
                       case !colorDst of
                         SPILLED ~1 => (dst, src)
                       | _ => (src, dst)
    
                  val dst as NODE{number=d, color=colorDst, adj=adjDst, 
                                  defs=defsDst, uses=usesDst,  ...} = dst
                  val src as NODE{number=s, color=colorSrc, adj=adjSrc, 
                                  defs=defsSrc, uses=usesSrc, ...} = src

                  (* combine adjacency lists *)
                  fun union([], adjSrc) = adjSrc
                    | union((n as NODE{color, adj=adjT, 
                                       number=t, ...})::adjDst, adjSrc) = 
                      (case !color of
                         (SPILLED _ | PSEUDO) =>
                           if addEdge(s, t) then 
                              (adjT := src :: !adjT; union(adjDst, n::adjSrc))
                           else union(adjDst, adjSrc)
                       | COLORED _ =>
                           if addEdge(s, t) then union(adjDst, n::adjSrc) 
                           else union(adjDst, adjSrc)
                       | _ => union(adjDst, adjSrc)
                      )
                  val mvs = MV.merge(l,r)
              in  if d = s then          (* trivial *)
                     coalesceMoves mvs
                  else
                  (case !colorDst of
                    SPILLED x =>
                       if x >= 0 orelse  (* both dst and src are mem regs *)
                          member(d, s)   (* they interfere *)
                       then 
                         ((* print("Bad "^Int.toString d ^
                                   "<->"^Int.toString s^"\n")*))
                       else 
                         ((* print(Int.toString d ^"<->"^Int.toString s^"\n");*)
                          ra_spill_coal := !ra_spill_coal + 1;
                           (* unify *)
                          colorDst := ALIASED src; 
                          adjSrc := union(!adjDst, !adjSrc);
                          if x >= 0 then ()
                          else                      
                            (defsSrc := concat(!defsDst, !defsSrc);
                             usesSrc := concat(!usesDst, !usesSrc))
                         )
                   | _ => error "coalesceMoves"; 
                   coalesceMoves mvs
                  )
              end
     in  coalesceMoves mvs
     end
  end

  (*
   * Spill propagation.
   *)
  fun spillPropagation(G as GRAPH{bitMatrix, memRegs, ...}) nodesToSpill =
  let val spillCoalescing = spillCoalescing G
      exception SpillProp
      val visited = Intmap.new(32, SpillProp) : bool Intmap.intmap
      val hasBeenVisited = Intmap.mapWithDefault (visited, false)
      val markAsVisited = Intmap.add visited
      val member = BM.member(!bitMatrix)  

      (* compute savings due to spill coalescing.
       * The move list must be associated with a colorable node.
       * The pinned flag is to prevent the spill node from coalescing
       * two different fixed memory registers.
       *)
      fun coalescingSavings([], pinned, sc) = (pinned, sc+sc)
        | coalescingSavings(MV{status=ref(CONSTRAINED | COALESCED), ...}::mvs,
                            pinned, sc) = coalescingSavings(mvs, pinned, sc)
        | coalescingSavings(MV{dst, src, cost, ...}::mvs, pinned, sc) =
          let val NODE{number=d, color=dstCol, ...} = chase dst
              val NODE{number=s, color=srcCol, ...} = chase src
              fun savings(x) =
                  if member(d, s) then coalescingSavings(mvs, pinned, sc) 
                  else if x = ~1 then coalescingSavings(mvs, pinned, sc+cost)
                  else if pinned >= 0 andalso pinned <> x then 
                     (* already coalesced with another mem reg *)
                     coalescingSavings(mvs, pinned, sc) 
                  else
                     (* coalescingSavings(mvs, x, sc+cost) *) (* XXX *)
                     coalescingSavings(mvs, x, sc+cost)
          in  if d = s then
                 coalescingSavings(mvs, pinned, sc)
              else
                 case (!dstCol, !srcCol) of
                   (SPILLED x, PSEUDO) => savings(x)
                 | (PSEUDO, SPILLED x) => savings(x)
                 | _ => coalescingSavings(mvs, pinned, sc)
          end

      (* Insert all spillable neighbors onto the worklist *)
      fun insert([], worklist) = worklist
        | insert((node as NODE{color=ref PSEUDO, number, ...})::adj, worklist) =
          if hasBeenVisited number 
          then insert(adj, worklist)
          else (markAsVisited (number, true);
                insert(adj, node::worklist))
        | insert(_::adj, worklist) = insert(adj, worklist)

      val marker = SPILLED(~1)

      (* Process all nodes from the worklist *)
      fun propagate([], spilled) = spilled
        | propagate((node as NODE{color as ref PSEUDO, 
                                  pri=ref spillcost, number, 
                                  adj, movelist, ...})::worklist, 
                    spilled) =
          let val (pinned, savings) = coalescingSavings(!movelist, ~1, 0)
          in  (* if (if pinned >= 0 then savings > spillcost
                 else savings >= spillcost) *) (* XXX *)
              if savings >= spillcost 
              then  (* propagate spill *)
                 (ra_spill_prop := !ra_spill_prop + 1;
                  color := marker; (* spill the node *)
                  (* print("Propagating "^Int.toString number^" "^
                        "savings="^Int.toString(savings)^
                       " cost="^Int.toString spillcost^"\n"); *)
                  (* run spill coalescing *)
                  spillCoalescing [node]; 
                  propagate(insert(!adj, worklist), node::spilled)
                 )
              else
                 propagate(worklist, spilled)
          end
        | propagate(_::worklist, spilled) = 
            propagate(worklist, spilled)

      (* Initialize worklist *)
      fun init([], worklist) = worklist
        | init(NODE{adj, color=ref(SPILLED _), ...}::rest, worklist) =
            init(rest, insert(!adj, worklist))
        | init(_::rest, worklist) = init(rest, worklist)

      (* 
       * Iterate between spill coalescing and propagation 
       *)
      fun iterate(spillWorkList, spilled) = 
      let (* run one round of coalescing first *)
          val _ = spillCoalescing spillWorkList
          val propagationWorkList = init(spillWorkList, []) 
          (* iterate on our own spill nodes *)
          val spilled = propagate(propagationWorkList, spilled)
          (* try the memory registers too *)
          val spilled = propagate(!memRegs, spilled)
      in  spilled
      end

  in  iterate(nodesToSpill, nodesToSpill)
  end

  (*
   * Spill coloring.
   * Assign logical spill locations to all the spill nodes.
   *
   * IMPORTANT BUG FIX:
   *    Spilled copy temporaries are assigned its own set of colors and
   * cannot share with another other nodes.   They can share colors with 
   * themselves however.
   *)
  fun spillColoring(GRAPH{spillLoc, copyTmps, mode, ...}) nodesToSpill = 
  let val proh     = A.array(length nodesToSpill, ~1)
      val firstLoc = !spillLoc
      val _ = spillLoc := firstLoc - 1 (* allocate one location first *)

      fun colorCopyTmps(tmps) =
      let fun loop([], found) = found
            | loop(NODE{color as ref(SPILLED ~1), ...}::tmps, found) = 
                (color := SPILLED firstLoc; loop(tmps, true))
            | loop(_::tmps, found) = loop(tmps, found)
      in  if loop(tmps, false) then
             (spillLoc := !spillLoc - 1; firstLoc - 1)
          else firstLoc
      end

      fun selectColor([], firstColor, currLoc) = ()
        | selectColor(NODE{color as ref(SPILLED ~1), number, adj, ...}::nodes,
                      firstColor, currLoc) = 
          let fun neighbors [] = ()
                | neighbors(n::ns) = 
                  let fun mark(NODE{color=ref(SPILLED loc), ...}) =
                           (if loc >= ~1 then () (* no location yet *) 
                            else A.update(proh, firstLoc - loc, number);
                            neighbors ns
                           )
                        | mark(NODE{color=ref(ALIASED n), ...}) = mark n 
                        | mark _ = neighbors ns
                  in  mark n end
              val _ = neighbors(!adj)
              fun findColor(loc, startingPoint) = 
                  let val loc = if loc < firstColor then !spillLoc + 1 else loc
                  in  if A.sub(proh, firstLoc - loc) <> number then loc (* ok *)
                      else if loc = startingPoint then (* new location *)
                      let val loc = !spillLoc 
                      in  spillLoc := loc - 1; loc end
                      else findColor(loc - 1, startingPoint)
                  end
              val currLoc = if currLoc < firstColor then !spillLoc + 1 
                            else currLoc
              val loc = findColor(currLoc, currLoc)
              (* val _ = print("Spill("^Int.toString number^")="^
                            Int.toString loc^"\n") *)
          in  color := SPILLED loc; (* mark with color *)
              selectColor(nodes, firstColor, loc - 1)
          end
        | selectColor(_::nodes, firstColor, currLoc) = 
              selectColor(nodes, firstColor, currLoc)

      (* color the copy temporaries first *)
       val firstColor = if isOn(mode, RACore.HAS_PARALLEL_COPIES) 
                        then colorCopyTmps(!copyTmps) else firstLoc
      (* color the rest of the spilled nodes *)
  in  selectColor(nodesToSpill, firstColor, !spillLoc) 
  end

  end (* local *)
  
  structure F = Flowgraph

  open F 

  val SPILL_COALESCING     = 0wx100
  val SPILL_COLORING       = 0wx200
  val SPILL_PROPAGATION    = 0wx400

  (*
   * New services that also perform memory allocation 
   *)
  fun services f =
  let val {build, spill=spillMethod, 
           blockNum, instrNum, programPoint} = F.services f

      (* Mark nodes that are immediately aliased to mem regs;
       * These are nodes that need also to be spilled
       *)
      fun markMemRegs [] = ()
        | markMemRegs(NODE{number=r, color as ref(ALIASED
                     (NODE{color=ref(col as SPILLED c), ...})), ...}::ns) =
           (if c >= 0 then color := col else ();
            markMemRegs ns)
        | markMemRegs(_::ns) = markMemRegs ns
 
      (*
       * Actual spill phase.  
       *   Perform the memory coalescing phases first, before doing an 
       *   actual spill.
       *)
      fun spillIt{graph = G as GRAPH{mode, ...}, nodes,
                  copyInstr, spill, spillSrc, spillCopyTmp,
                  reload, reloadDst, renameSrc, cellkind} =
      let val nodes = if isOn(mode,SPILL_PROPAGATION) then   
                          spillPropagation G nodes else nodes
          val _ = if isOn(mode,SPILL_COALESCING) then 
                     spillCoalescing G nodes else ()
          val _ = if isOn(mode,SPILL_COLORING) then 
                     spillColoring G nodes else ()
          val _ = if isOn(mode,SPILL_COALESCING+SPILL_PROPAGATION) 
                  then markMemRegs nodes else ()
      in  spillMethod
               {graph=G, nodes=nodes, copyInstr=copyInstr,
                spill=spill, spillSrc=spillSrc, spillCopyTmp=spillCopyTmp,
                reload=reload, reloadDst=reloadDst, 
                renameSrc=renameSrc, cellkind=cellkind} 
      end
   in  {build=build, spill=spillIt, programPoint=programPoint,
        blockNum=blockNum, instrNum=instrNum}
   end

end 
