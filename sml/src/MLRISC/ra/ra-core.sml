(*
 *
 * Overview
 * ========
 * This implementation of iterated coalescing differ from the old one in
 * various substantial ways:
 *
 * 1. The move list is prioritized.  Higher ranking moves are coalesced first.
 *    This tends to favor coalescing of moves that has higher priority.
 *
 * 2. The freeze list is prioritized.  Lower ranking nodes are unfrozen
 *    first.  Since freeze disable moves, this tends to disable moves
 *    of low priority.
 *
 * 3. The simplify worklist is not kept explicitly during the 
 *    simplify/coalesce/freeze phases.  Instead, whenever a non-move
 *    related node with degree < K is discovered, we call simplify
 *    to remove it from the graph immediately.  
 *
 *    I think this has a few advantages.
 *    (a) There is less bookkeeping.
 *    (b) Simplify adds coalescable moves to the move list.
 *        By doing simplify eagerly, moves are added to the move list
 *        faster, allowing higher ranking moves to ``preempt'' low
 *        ranking moves.
 *
 * 4. Support for register pairs
 *
 * Important Invariants
 * ====================
 *   1. Adjacency list
 *      a. All nodes on the adjacency list are distinct
 *      b. nodes with color ALIASED or REMOVED are NOT consider to be
 *         on the adjacency list
 *      c. If a node x is COLORED, then we DON'T keep track of 
 *         its adjacency list 
 *      d. When a node has been removed, there aren't any moves associated
 *         with it.    
 *   2. Moves
 *      a. Moves marked WORKLIST are on the worklist.
 *      b. Moves marked MOVE are NOT on the worklist.
 *      c. Moves marked LOST are frozen and are in fact never considered again.
 *      d. Moves marked CONSTRAINED cannot be coalesced because the src and dst
 *         interfere
 *      e. Moves marked COALESCED have been coalesced.  
 *      f. The movecnt in a node is always the number of nodes 
 *         currently marked as WORKLIST or MOVE, i.e. the moves that
 *         are associated with the node.  When this is zero, the node is
 *         considered to be non-move related.
 *      g. Moves on the move worklist are always distinct.
 *   3.
 *
 * Allen.
 *
 *)

structure RACore : RA_CORE =
struct

  structure G    = RAGraph
  structure A    = Array
  structure W    = Word
  structure W8A  = Word8Array
  structure W8   = Word8

  (* For debugging, uncomment Unsafe. *)
  structure UA   = Unsafe.Array 
  structure UW8A = Unsafe.Word8Array

  open G 

  val verbose       = MLRiscControl.getFlag "ra-verbose"
  val ra_spill_coal = MLRiscControl.getCounter "ra-spill-coalescing"
  val ra_spill_prop = MLRiscControl.getCounter "ra-spill-propagation"

  fun error msg = MLRiscErrorMsg.error("RACore", msg)
 
  (* No overflow checking necessary here *)
  fun x + y = W.toIntX(W.+(W.fromInt x, W.fromInt y))
  fun x - y = W.toIntX(W.-(W.fromInt x, W.fromInt y))

  fun concat([], b) = b
    | concat(x::a, b) = concat(a, x::b)

  (*
   * Bit Matrix routines
   *)
  structure BM = 
  struct
     fun hashFun(i, j, shift, size) = 
         W.toIntX(W.andb(W.+(W.<<(W.fromInt i, shift), 
                  W.fromInt(i+j)), W.fromInt(size -1)))

     val empty = BM{table=SMALL(ref(A.array(0, [])), 0w0), elems=ref 0, edges=0}

     (*
     val indices = A.array(1024,0)

     fun init(i,j) =
         if i < 1024 then
            (A.update(indices, i, j); init(i+1, i+j+1))
         else ()

     val _ = init(0, 0)
      *)

     fun edges(BM{table=SMALL(ref table, _), ...}) = A.length table
       | edges(BM{table=LARGE(ref table, _), ...}) = A.length table
     (*| edges(BM{table=BITMATRIX _, edges, ...}) = edges *)

     fun member(BM{table=SMALL(table, shift), ...}) =
         (fn (NODE{number=i,...}, NODE{number=j,...}) =>
          let val (i,j) = if i < j then (i, j) else (j, i)
              val k = W.+(W.<<(W.fromInt i, 0w15), W.fromInt j)
              fun find [] = false
                | find(k'::b) = k = k' orelse find b
              val tab = !table
          in  find(UA.sub(tab, hashFun(i, j, shift, A.length tab))) end
         )
       | member(BM{table=LARGE(table, shift), ...}) =
         (fn (NODE{number=i,...}, NODE{number=j,...}) =>
          let val (i,j) = if i < j then (i, j) else (j, i)
              fun find NIL = false
                | find(B(i',j',b)) = i = i' andalso j = j' orelse find b
              val tab = !table
          in  find(UA.sub(tab, hashFun(i, j, shift, A.length tab))) end
         )
     (*
       | member(BM{table=BITMATRIX table, ...}) =
         (fn (NODE{number=i,...}, NODE{number=j,...}) =>
          let val (i,j) = if i > j then (i, j) else (j, i)
              val bit   = W.fromInt(UA.sub(indices, i) + j)
              val index = W.toIntX(W.>>(bit, 0w3))
              val mask  = W.<<(0w1, W.andb(bit, 0w7))
          in  W.andb(W.fromInt(W8.toInt(UW8A.sub(table, index))), mask) <> 0w0 
          end
         )
     *)

     fun add (BM{table=SMALL(table, shift), elems, ...}) =
         let fun insert(i, j) =
             let val (i,j) = if i < j then (i, j) else (j, i)
                 val tab = !table
                 val len = A.length tab
             in  if !elems < len then
                 let val index = hashFun(i, j, shift, len)
                     val k = W.+(W.<<(W.fromInt i, 0w15), W.fromInt j)
                     fun find [] = false
                       | find(k'::b) = k = k' orelse find b
                     val b = UA.sub(tab, index)
                 in  if find b then false
                     else (UA.update(tab, index, k::b); 
                           elems := !elems + 1; true)
                 end
                 else (* grow table *)
                 let val oldTable = tab
                     val oldSize  = A.length oldTable
                     val newSize  = oldSize + oldSize
                     val newTable = A.array(newSize,[])
                     fun enter n =
                     if n < oldSize then
                     let fun loop([],a,b) = 
                               (UA.update(newTable, n, a);
                                UA.update(newTable, n + oldSize, b);
                                enter(n+1))
                           | loop(k::l,a,b) =
                             let val i = W.toIntX(W.>>(k, 0w15))  
                                 val j = W.toIntX(W.-(k,W.<<(W.fromInt i, 0w15)))
                             in  if hashFun(i, j, shift, newSize) = n 
                                 then loop(l, k::a, b)
                                 else loop(l, a, k::b)
                             end
                     in  loop(UA.sub(oldTable, n), [], []) end
                     else ()
                 in  table := newTable;
                     enter 0; 
                     insert(i, j)
                 end 
             end
         in  insert
         end
       | add (BM{table=LARGE(table, shift), elems, ...}) =
         let fun insert(i, j) =
             let val (i,j) = if i < j then (i, j) else (j, i)
                 val tab = !table
                 val len = A.length tab
             in  if !elems < len then
                 let val index = hashFun(i, j, shift, len)
                     fun find NIL = false
                       | find(B(i',j',b)) = i = i' andalso j = j' orelse find b
                     val b = UA.sub(tab, index)
                 in  if find b then false
                     else (UA.update(tab, index, B(i,j,b)); 
                           elems := !elems + 1; true)
                 end
                 else (* grow table *)
                 let val oldTable = tab
                     val oldSize  = A.length oldTable
                     val newSize  = oldSize + oldSize
                     val newTable = A.array(newSize,NIL)
                     fun enter n =
                     if n < oldSize then
                     let fun loop(NIL,a,b) = 
                               (UA.update(newTable, n, a);
                                UA.update(newTable, n + oldSize, b);
                                enter(n+1))
                           | loop(B(i,j,l),a,b) =
                                if hashFun(i, j, shift, newSize) = n 
                                then loop(l, B(i,j,a), b)
                                else loop(l, a, B(i,j,b))
                     in  loop(UA.sub(oldTable, n), NIL, NIL) end
                     else ()
                 in  table := newTable;
                     enter 0; 
                     insert(i, j)
                 end 
             end
         in  insert
         end
     (*
       | add(BM{table=BITMATRIX table, ...}) =
         (fn (i, j) =>
          let val (i,j) = if i > j then (i, j) else (j, i)
              val bit   = W.fromInt(UA.sub(indices, i) + j)
              val index = W.toIntX(W.>>(bit, 0w3))
              val mask  = W.<<(0w1, W.andb(bit, 0w7))
              val value = W.fromInt(W8.toInt(UW8A.sub(table, index)))
          in  if W.andb(value, mask) <> 0w0 then false
              else (UW8A.update(table, index, 
                      W8.fromInt(W.toIntX(W.orb(value, mask)))); true) 
          end
         )
      *)

     fun delete (BM{table=SMALL(table, shift), elems, ...}) =
         (fn (i,j) =>
          let val k = W.+(W.<<(W.fromInt i, 0w15), W.fromInt j)
              fun find [] = []
                | find(k'::b) =
                  if k = k' then (elems := !elems - 1; b) else find b
              val tab = !table
              val index = hashFun(i, j, shift, A.length tab)
              val n = !elems
          in  UA.update(tab, index, find(UA.sub(tab, index)));
              !elems <> n
          end
         )
       | delete (BM{table=LARGE(table, shift), elems, ...}) =
         (fn (i,j) =>
          let fun find NIL = NIL
                | find(B(i', j', b)) =
                  if i = i' andalso j = j' then (elems := !elems - 1; b)
                  else B(i', j', find b)
              val tab = !table
              val index = hashFun(i, j, shift, A.length tab)
              val n = !elems
          in  UA.update(tab, index, find(UA.sub(tab, index)));
              !elems <> n
          end
         )
     (*
       | delete(BM{table=BITMATRIX table, ...}) =
         (fn (i, j) =>
          let val (i,j) = if i > j then (i, j) else (j, i)
              val bit   = W.fromInt(UA.sub(indices, i) + j)
              val index = W.toIntX(W.>>(bit, 0w3))
              val mask  = W.-(W.<<(0w1, W.andb(bit, 0w7)), 0w1)
              val value = W.fromInt(W8.toInt(UW8A.sub(table, index)))
          in  if W.andb(value, mask) = 0w0 then false
              else (UW8A.update(table, index, 
                            W8.fromInt(W.toIntX(W.andb(value,W.notb mask)))); 
                    true) 
          end
         )
      *)
  end
    

  (*
   * Priority Queue.  Let's hope the compiler will inline it for performance
   *)
  functor PriQueue(type elem val less : elem * elem -> bool) =
  struct

                       
   (* A leftist tree is a binary tree with priority ordering
    * with the invariant that the left branch is always the taller one         
    *)
   type elem = elem
   datatype pri_queue = TREE of elem * int * pri_queue * pri_queue
                      | EMPTY

   fun merge'(EMPTY, EMPTY) = (EMPTY, 0)
     | merge'(EMPTY, a as TREE(_, d, _, _)) = (a, d)
     | merge'(a as TREE(_, d, _, _), EMPTY) = (a, d)
     | merge'(a as TREE(x, d, l, r), b as TREE(y, d', l', r')) =
       let val (root, l, r1, r2) = 
               if less(x, y) then (x, l, r, b) else (y, l', r', a) 
           val (r, d_r) = merge'(r1, r2)
           val d_l      = case l of
                            EMPTY => 0
                          | TREE(_, d, _, _) => d 
           val (l, r) = if d_l >= d_r then (l, r) else (r, l)
           val d_t = 1+(if d_l > d_r then d_l else d_r)
       in  (TREE(root, d_t, l, r), d_t) end

   fun merge(EMPTY, a) = a
     | merge(a, EMPTY) = a
     | merge(a, b) = #1(merge'(a, b))

   fun add (x, EMPTY) =  TREE(x, 1, EMPTY, EMPTY)
     | add (x, b as TREE(y, d', l', r')) = 
       if less(x,y) then TREE(x, d'+1, b, EMPTY)
       else #1(merge'(TREE(x, 1, EMPTY, EMPTY), b))

  end

  structure FZ = PriQueue
     (type elem=node 
      fun less(NODE{movecost=ref p1,...}, NODE{movecost=ref p2,...}) = p1 < p2
     )
  structure MV = PriQueue
     (type elem=G.move 
      fun less(MV{cost=p1,...}, MV{cost=p2,...}) = p1 > p2
     )
  type move_queue = MV.pri_queue
  type freeze_queue = FZ.pri_queue


  (*  
   * Utility functions
   *)
  fun chase(NODE{color=ref(ALIASED r), ...}) = chase r
    | chase x = x

  fun nodeNumber(NODE{number,...}) = number

  val debug = false

  fun colorOf(G.GRAPH{showReg,...}) (NODE{number, color, pri,...}) =
       showReg number^
           (case !color of
              PSEUDO    => ""
            | REMOVED   => "r"
            | ALIASED _ => "a"
            | COLORED c => "["^showReg c^"]"
            | SPILLED _ => "s"
           )

  fun show G (node as NODE{pri,...}) = 
      colorOf G node^(if !verbose then "("^Int.toString(!pri)^")" else "")

  (*
   * Dump the interference graph
   *)
  fun dumpGraph(G as G.GRAPH{nodes, showReg, K,...}) stream =
  let fun pr s = TextIO.output(stream, s)
      val show = show G
      val colorOf = colorOf G
      fun prMove(MV{src, dst, status=ref(WORKLIST | MOVE), cost,...}) = 
            pr(colorOf(chase dst)^" <- "^colorOf(chase src)^
               "("^Int.toString(cost)^") ")
        | prMove _ = ()
 
      fun prAdj(n, n' as NODE{color=ref(SPILLED _),...}) = 
           pr(show n'^" spilled\n")
        | prAdj(n, n' as NODE{adj, degree, color, 
                              (*pair,*) pri, movecnt, movelist, ...}) =
         (pr(show n');
          (* if pair then pr(" pair ") else (); *)
          if !verbose then pr(" deg="^Int.toString(!degree)) else ();
          pr(" <-->");
          app (fn n => (pr " "; pr(show n))) (!adj);
          pr "\n";
          if !verbose andalso !movecnt > 0 then 
            (pr("\tmoves "^Int.toString(!movecnt)^": ");
             app prMove (!movelist);
             pr "\n"
            ) 
          else ()
         )
  in  pr("=========== K="^Int.toString K^" ===========\n");
      app prAdj (ListMergeSort.sort (fn ((x, _),(y, _)) => x > y)
                    (Intmap.intMapToList nodes))
  end


  (*
   * Function to create new nodes.
   * Note: it is up to the caller to remove all dedicated registers.
   *)
  fun newNodes(G.GRAPH{nodes, firstPseudoR,  ...}) =
  let val getnode = Intmap.map nodes
      val addnode = Intmap.add nodes

      fun defUse{defs, uses, pt, cost} =
      let  fun def reg =
           let val node as NODE{pri, defs,...} = getnode reg
           in  pri := !pri + cost;(* increment the priority by the cost *)
               defs := pt :: !defs;
               node
           end
           handle _ =>
           let val col = if reg < firstPseudoR then COLORED(reg) else PSEUDO
               val node = 
                   NODE{number=reg, color=ref col, degree=ref 0,
                        adj=ref [], movecnt=ref 0, movelist = ref [],
                        movecost=ref 0, (* pair=false, *) pri=ref cost, 
                        defs=ref [pt], uses=ref []}
           in addnode(reg, node); node
           end
           fun use reg =
           let val node as NODE{pri, uses,...} = getnode reg
           in  pri := !pri + cost; (* increment the priority by the cost *)
               uses := pt :: !uses
           end
           handle _ =>
           let val col = if reg < firstPseudoR then COLORED(reg) else PSEUDO
               val node = 
                   NODE{number=reg, color=ref col, degree=ref 0,
                        adj=ref [], movecnt=ref 0, movelist = ref [],
                        movecost=ref 0, (* pair=false, *)
                        pri=ref cost, defs=ref [], uses=ref[pt]
                       }
           in addnode(reg, node)
           end
           fun defAll([],ds) = ds | defAll(r::rs,ds) = defAll(rs,def r::ds)
           fun useAll [] = () | useAll(r::rs) = (use r; useAll rs)
           val defs = defAll(defs,[])
           val _    = useAll uses
      in   defs
      end
  in  defUse
  end


  (*
   * Add an edge (x, y) to the interference graph.
   * Nop if the edge already exists.
   * Note: adjacency lists of colored nodes are not stored 
   *       within the interference graph to save space.
   *)
  fun addEdge(GRAPH{bitMatrix,...}) =
  let val addBitMatrix = BM.add(!bitMatrix)
      fun add(NODE{color, adj, degree, (* pair=p1, *) number=n1, ...},
              s as NODE{number=n2, (* pair=p2, *) ...}) =
          (case !color of
             PSEUDO        => (adj := s :: !adj; 
                               (* check for pair <-> pair interference *)
                               (* if p1 andalso p2 then degree := 2 + !degree
                               else *) degree := 1 + !degree
                              )
           | COLORED _     => () (* not stored *)
           | ALIASED _     => error "addEdge: ALIASED"
           | REMOVED       => error "addEdge: REMOVED"
           | SPILLED _     => error "addEdge: SPILLED"
          )
  in  fn (x as NODE{number=xn, ...}, y as NODE{number=yn, ...}) => 
          if xn = yn then ()
          else if addBitMatrix(xn, yn) then
               (add(x, y); add(y, x))
          else () 
  end

  (*
   * Remove an edge from the bitmatrix 
   *)
  fun removeEdge(GRAPH{bitMatrix,...}) =
  let val rmvBitMatrix = BM.delete(!bitMatrix)
      fun filter(c,[], adj') = adj'
        | filter(c,(n as NODE{color,...})::adj, adj') =
            filter(c, adj, if c = color then adj' else n::adj')
      fun rmv(NODE{color, adj, degree, (* pair=p1, *)...}, 
              s as NODE{(* pair=p2, *) color=c2,...}) =
          (case !color of
             PSEUDO        => (adj := filter(c2,!adj,[]); 
                               (* check for pair <-> pair interference *)
                               (* if p1 andalso p2 then degree := !degree - 2
                               else *) degree := !degree - 1
                              )
           | COLORED _     => () (* not stored *)
           | ALIASED _     => error "removeEdge: ALIASED"
           | REMOVED       => error "removeEdge: REMOVED"
           | SPILLED _     => error "removeEdge: SPILLED"
          )
  in  fn (x as NODE{number=xn, ...}, y as NODE{number=yn, ...}) => 
          if xn = yn then ()
          else if rmvBitMatrix(if xn < yn then (xn, yn) else (yn, xn)) then
               (rmv(x, y); rmv(y, x))
          else () 
  end

  (*
   * Initialize a list of worklists
   *)
  fun initWorkLists 
        (GRAPH{nodes, K, bitMatrix, regmap, firstPseudoR, deadCopies, ...}) 
        {moves, deadCopyElim} =
  let (* Filter moves that already have an interference
       * Also initialize the movelist and movecnt fields at this time.
       *)
      val member = BM.member(!bitMatrix)

      fun setInfo(NODE{color=ref PSEUDO, movecost, movecnt, movelist,...}, 
                  mv, cost) =
           (movelist := mv :: !movelist; 
            movecnt := !movecnt + 1;
            movecost := !movecost + cost
           )
        | setInfo _ = ()

      fun filter([], mvs') = mvs'
        | filter(MV{src=NODE{color=ref(COLORED _),...},
                    dst=NODE{color=ref(COLORED _),...},...}::mvs, mvs') =
            filter(mvs, mvs')
        | filter((mv as MV{src, dst, cost,...})::mvs, mvs') = 
            if member(src, dst)     (* moves that interfere *)
            then filter(mvs, mvs') 
            else (setInfo(src, mv, cost);
                  setInfo(dst, mv, cost);
                  filter(mvs, MV.add(mv, mvs'))
                 )

      fun decDegree [] = ()
        | decDegree(NODE{color=ref PSEUDO, degree, ...}::adj) =
            (degree := !degree - 1; decDegree adj)
        | decDegree(_::adj) = decDegree adj

      fun elimUses([], _, uses, pri, cost) = (uses, pri)
        | elimUses(pt::pts, pt' : int, uses, pri, cost) =
          if pt = pt' then elimUses(pts, pt', uses, pri-cost, cost)
          else elimUses(pts, pt', pt::uses, pri, cost)

      fun filter'([], mvs', dead) = (mvs', dead)
        | filter'(MV{src=NODE{color=ref(COLORED _),...},
                     dst=NODE{color=ref(COLORED _),...},...}::mvs, mvs', dead) =
            filter'(mvs, mvs', dead)
        | filter'(MV{dst as NODE{number=r, color as ref PSEUDO, defs=ref [pt], 
                              uses=ref [], adj, ...},
                     src as NODE{uses, pri, ...}, cost, ...}::mvs, 
                  mvs', dead) = 
            (* eliminate dead copy *)
            let val (uses', pri') = elimUses(!uses, pt, [], !pri, cost);
            in  pri := pri';
                uses := uses';
                color := ALIASED src;
                decDegree(!adj);
                filter'(mvs, mvs', r::dead)
            end
        | filter'((mv as MV{src, dst, cost,...})::mvs, mvs', dead) = 
            if member(src, dst)     (* moves that interfere *)
            then filter'(mvs, mvs', dead) 
            else (setInfo(src, mv, cost);
                  setInfo(dst, mv, cost);
                  filter'(mvs, MV.add(mv, mvs'), dead)
                 )


      (*
       * Scan all nodes in the graph and check which worklist they should
       * go into.
       *)
      fun collect([], simp, fz, moves, spill) =
          {simplifyWkl = simp,
           moveWkl     = moves,
           freezeWkl   = fz,
           spillWkl    = spill
          }
        | collect(node::rest, simp, fz, moves, spill) = 
          (case node of
              NODE{color=ref PSEUDO, movecnt, degree, ...} =>
                 if !degree >= K then
                    collect(rest, simp, fz, moves, node::spill)
                 else if !movecnt > 0 then
                    collect(rest, simp, FZ.add(node, fz), moves, spill)
                 else
                    collect(rest, node::simp, fz, moves, spill)  
           |  _ => collect(rest, simp, fz, moves, spill)
          )

      (* First build the move priqueue *)
      val mvs = if deadCopyElim then
                let val (mvs, dead) = filter'(moves, MV.EMPTY, [])
                in  deadCopies := dead; mvs
                end
                else filter(moves, MV.EMPTY)

      (* if copy propagation was done prior to register allocation
       * then some nodes may already be aliased.  This function updates the
       * aliasing.
       *)
      fun updateAliases() =
      let val alias = Intmap.mapInt regmap   
          val getnode = Intmap.map nodes
          fun fixup(num, NODE{color, ...}) =
              if num < firstPseudoR then ()
              else let val reg = alias num
                   in  if reg=num then () else
                       color := ALIASED(getnode reg)
                   end
      in  Intmap.app fixup nodes end

  in  (* updateAliases(); *)
      collect(Intmap.values nodes, [], FZ.EMPTY, mvs, [])
  end

  (*
   * Return a regmap that reflects the current interference graph.
   * Spilled registers are given the special value ~1
   *)
  fun regmap(G.GRAPH{nodes,...}) = 
  let val getnode = Intmap.map nodes
      fun num(NODE{color=ref(COLORED r),...}) = r
        | num(NODE{color=ref(ALIASED n),...}) = num n
        | num(NODE{color=ref(SPILLED s),number,...}) = ~1
        | num(NODE{number, color=ref PSEUDO,...}) = number
        | num _ = error "regmap"
      fun lookup r = num(getnode r) handle _ => r (* XXX *)
  in  lookup 
  end

  (*
   * Return a regmap that reflects the current interference graph, 
   * during spilling.
   *)
  fun spillRegmap(G.GRAPH{nodes,...}) = 
  let val getnode = Intmap.map nodes
      fun num(NODE{color=ref(COLORED r),...}) = r
        | num(NODE{color=ref(ALIASED n),...}) = num n
        | num(NODE{color=ref(SPILLED _),number,...}) = number
        | num(NODE{number, color=ref PSEUDO,...}) = number
        | num _ = error "spillRegmap"
      fun lookup r = num(getnode r) handle _ => r (* XXX *)
  in  lookup 
  end

  (*
   * Return a regmap that returns the current spill location
   * during spilling.
   *)
  fun spillLoc(G.GRAPH{nodes,...}) = 
  let val getnode = Intmap.map nodes
      fun num(NODE{color=ref(ALIASED n), ...}) = num n
        | num(NODE{color=ref(SPILLED ~1), number, ...}) = number
        | num(NODE{color=ref(SPILLED c), ...}) = c
        | num(NODE{number, ...}) = number
      fun lookup r = num(getnode r) handle _ => r (* XXX *)
  in  lookup 
  end

  (*
   * Core phases:
   *   Simplify, coalesce, freeze.
   *
   * NOTE: When a node's color is REMOVED or ALIASED, 
   *       it is not considered to be part of the adjacency list
   *
   *  1.  The move list has no duplicate
   *  2.  The freeze list may have duplicates
   *)
  fun iteratedCoalescingPhases
       (G as GRAPH{K, bitMatrix, spillFlag, trail, stamp, ...}) =
  let val member = BM.member(!bitMatrix)
      val addEdge = addEdge G
      val show = show G

      (*
       * SIMPLIFY node:
       *   precondition: node must be part of the interference graph (PSEUDO)
       *)
      fun simplify(node as NODE{color, number, adj, (* pair, *)...}, 
                                mv, fz, stack) =
      let val _ = if debug then print("Simplifying "^show node^"\n") else ()
          fun forallAdj([], mv, fz, stack) = (mv, fz, stack)
            | forallAdj(n::adj, mv, fz, stack) =
              (case n of
                NODE{(* pair=p2, *) color=ref PSEUDO,...} =>
                  let val (mv, fz, stack) = 
                         decDegree(n, (* pair andalso p2, *) mv, fz, stack)
                  in  forallAdj(adj, mv, fz, stack) end
              | _ => forallAdj(adj, mv, fz, stack)
              )
      in  color := REMOVED;
          forallAdj(!adj, mv, fz, node::stack) (* push onto stack *)
      end (* simplify *)

      (*
       * Decrement the degree of a pseudo node.
       *   precondition: node must be part of the interference graph
       *   If the degree of the node is now K-1.
       *   Then if (a) the node is move related, freeze it.
       *           (b) the node is non-move related, simplify it
       *
       *   node  -- the node to decrement degree
       *   mv    -- queue of move candidates to be coalesced
       *   fz    -- queue of freeze candidates
       *   stack -- stack of removed nodes
       *)
      and decDegree(node as NODE{degree as ref d, movecnt, adj, color,...},
                    (* false, *) mv, fz, stack) = (* normal edge *)
          (if debug then 
           print("DecDegree "^show node^" d="^Int.toString(d-1)^"\n") else (); 
           degree := d - 1;
           if d = K then 
             (* node is now low degree!!! *)
             let val mv = enableMoves(node :: !adj, mv)
             in  if !movecnt > 0 then (* move related *)
                    (mv, FZ.add(node, fz), stack)
                 else (* non-move related, simplify now! *)
                    simplify(node, mv, fz, stack)
             end
           else
             (mv, fz, stack)
          )
       (*
        | decDegree(node as NODE{degree as ref d, movecnt, adj, color,...},
                    true, mv, fz, stack) = (* register pair edge *)
          (degree := d - 2;
           if d >= K andalso !degree < K then 
             (* node is now low degree!!! *)
             let val mv = enableMoves(node :: !adj, mv)
             in  if !movecnt > 0 then (* move related *)
                    (mv, FZ.add(node, fz), stack)
                 else (* non-move related, simplify now! *)
                    simplify(node, mv, fz, stack)
             end
           else
             (mv, fz, stack)
          )
        *)

      (*
       * Enable moves:
       *   given: a list of nodes (some of which are not in the graph)
       *   do:    all moves associated with these nodes are inserted
       *          into the move worklist
       *)
      and enableMoves([], mv) = mv
        | enableMoves(n::ns, mv) =
          let (* add valid moves onto the worklist.
               * there are no duplicates on the move worklist!
               *)
              fun addMv([], mv) = mv
                | addMv((m as MV{status,dst,src,...})::rest, mv) = 
                  (case !status of
                     MOVE => (status := WORKLIST;     
                              addMv(rest, MV.add(m, mv)))
                   | _    => addMv(rest, mv)
                  )
          in  (* make sure the nodes are actually in the graph *)
              case n of
                NODE{movelist, color=ref PSEUDO, movecnt,...} =>
                  if !movecnt > 0 then (* is it move related? *)
                     enableMoves(ns, addMv(!movelist, mv))
                  else
                     enableMoves(ns, mv)
              | _ => enableMoves(ns, mv)
          end (* enableMoves *)

     (*
      * Simplify a list of nodes
      *)
     fun simplifyAll([], mv, fz, stack) = (mv, fz, stack)
       | simplifyAll(n::ns, mv, fz, stack) = 
         let val (mv, fz, stack) = simplify(n, mv, fz, stack)
         in  simplifyAll(ns, mv, fz, stack) end 

     (*
      *  Brigg's conservative coalescing test:
      *    given: an unconstrained move (x, y)  
      *    return: true or false
      *)
     fun conservative(x as NODE{degree=ref dx, adj=xadj, (* pair=px, *) ...},
                      y as NODE{degree=ref dy, adj=yadj, (* pair=py, *) ...}) =
         dx + dy < K orelse
         let (*  
              *  K-k -- is the number of nodes with deg > K
              *  n -- the number of nodes that have deg = K but not
              *       neighbors of both x and y.
              *  We use the movecnt as a flag indicating whether
              *  a node has been visited.  A negative count is used to mark
              *  a visited node.
              *)
             fun undo([], v) = v
               | undo(movecnt::tr, v) = (movecnt := ~1 - !movecnt; undo(tr, v))
             fun loop([], [], 0, _, tr) = undo(tr, false)
               | loop([], [], k, n, tr) = undo(tr, k > n)
               | loop([], yadj, k, n, tr) = loop(yadj, [], k, n, tr)
               | loop(NODE{color, movecnt as ref m, degree=ref deg, ...}::vs, 
                      yadj, k, n, tr) =
                      (case !color of
                         COLORED _ =>
                           if m < 0 then
                              (* node has been visited before *)
                              if deg = K then loop(vs, yadj, k, n-1, tr)
                              else loop(vs, yadj, k, n, tr)
                           else
                             (movecnt := ~1 - m;  (* mark as visited *)
                              loop(vs, yadj, k-1, n, movecnt::tr))
                       | PSEUDO =>
                           if deg < K then loop(vs, yadj, k, n, tr)
                           else if m >= 0 then
                             (* node has never been visited before *)
                              (movecnt := ~1 - m;  (* mark as visited *)
                               if deg = K 
                               then loop(vs, yadj, k, n+1, movecnt::tr)
                               else loop(vs, yadj, k-1, n, movecnt::tr)
                              )
                           else
                              (* node has been visited before *)
                              if deg = K then loop(vs, yadj, k, n-1, tr)
                              else loop(vs, yadj, k, n, tr)
                       | _ => loop(vs, yadj, k, n, tr) (* REMOVED/ALIASED *)
                      )
         in loop(!xadj, !yadj, K, 0, []) end

     (*
      *  Heuristic used to determine whether a pseudo and machine register     
      *  can be coalesced. 
      *  Precondition:
      *     The two nodes are assumed not to interfere.
      *)
     fun safe(r, NODE{adj, ...}) =
     let fun loop [] = true
           | loop(n::adj) =
             (case n of
               NODE{color=ref(COLORED _),...} => loop adj (* can't be r! *)
             | NODE{color=ref(ALIASED _),...} => loop adj (* not real *)
             | NODE{color=ref(SPILLED _),...} => loop adj (* gone! *)
             | NODE{degree,...} => (* PSEUDO or REMOVED *)
                (!degree < K orelse member(n, r)) andalso loop adj
             )
     in  loop(!adj) end

     (*
      *  Decrement the active move count of a node.
      *  When the move count reaches 0 and the degree < K
      *  simplify the node immediately.    
      *      Precondition: node must be a node in the interference graph
      *      The node can become a non-move related node.
      *)
     fun decMoveCnt
         (node as NODE{movecnt, color=ref PSEUDO, degree, movecost,...}, 
          cnt, cost, mv, fz, stack) =
         let val newCnt = !movecnt - cnt
         in  movecnt := newCnt;
             movecost := !movecost - cost;
             if newCnt = 0 andalso !degree < K (* low degree and movecnt = 0 *)
             then simplify(node, mv, fz, stack)
             else (mv, fz, stack)
         end
       | decMoveCnt(_, _, _, mv, fz, stack) = (mv, fz, stack)

     (*
      * Combine two nodes u and v into one.
      *   v is replaced by u  
      *   u is the new combined node
      *   Precondition: u <> v and u and v must be unconstrained
      *
      *  u, v   -- two nodes to be merged, must be distinct!
      *  mvcost -- the cost of the move that has been eliminated
      *  mv     -- the queue of moves
      *  fz     -- the queue of freeze candidates
      *  stack  -- stack of removed nodes
      *)
     fun combine(u, v, mvcost, mv, fz, stack) =
     let val NODE{color=vcol, pri=pv, movecnt=cntv, movelist=movev, adj=adjv,
                  defs=defsv, uses=usesv, ...} = v
         val NODE{color=ucol, pri=pu, movecnt=cntu, movelist=moveu, adj=adju,
                  defs=defsu, uses=usesu, ...} = u
  
         (* merge movelists together, taking the opportunity
          * to prune the lists
          *)
         fun mergeMoveList([], mv) = mv
           | mergeMoveList((m as MV{status,...})::rest, mv) = 
              (case !status of
                (MOVE | WORKLIST) => mergeMoveList(rest, m::mv)
              | _ => mergeMoveList(rest, mv)
              )

         (* Form combined node; add the adjacency list of v to u *)
         fun union([], mv, fz, stack) = (mv, fz, stack)
           | union((t as NODE{color, (* pair=pt, *) ...})::adj, mv, fz, stack) =
              (case !color of
                 COLORED _ => (addEdge(t, u); union(adj, mv, fz, stack))
               | PSEUDO =>
                   (addEdge(t, u);
                    let val (mv, fz, stack) = decDegree(t, mv, fz, stack)
                    in  union(adj, mv, fz, stack) end
                   ) 
               | _ => union(adj, mv, fz, stack)
              )        
 
     in  vcol    := ALIASED u; 
                  (* combine the priority of both: 
                   * note that since the mvcost has been counted twice
                   * in the original priority, we substract it twice
                   * from the new priority.
                   *)
         pu    := !pu + !pv;
                  (* combine the def/use pts of both nodes.
                   * Strictly speaking, the def/use points of the move
                   * should also be removed.  But since we never spill
                   * a coalesced node and only spilling makes use of these
                   * def/use points, we are safe for now.  
                   *
                   * New comment: with spill propagation, it is necessary
                   * to keep track of the spilled program points.
                   *)
         defsu := concat(!defsu, !defsv); 
         usesu := concat(!usesu, !usesv);
         case !ucol of
           PSEUDO => 
             (if !cntv > 0 then moveu := mergeMoveList(!movev, !moveu) else (); 
              movev := []; (* XXX kill the list to free space *)
              cntu  := !cntu + !cntv
             )
         | _ => ()
         ;
         cntv := 0;

             (* Update the move count of the combined node *)
         let val (mv, fz, stack) = union(!adjv, mv, fz, stack)
         in  decMoveCnt(u, 2, mvcost + mvcost, mv, fz, stack) 
         end
     end

     (*
      *  COALESCE:
      *    Repeat coalescing and simplification until mv is empty.
      *)
     fun coalesce(MV.EMPTY, fz, stack, undo) = (fz, stack, undo)
       | coalesce(MV.TREE(MV{src, dst, status, cost, ...}, _, l, r), 
                          fz, stack, undo)= 
         let val u = chase src
             val v as NODE{color=ref vcol, ...} = chase dst
               (* make u the colored one *)
             val (u as NODE{number=u', color=ref ucol, ...},
                  v as NODE{number=v', color=ref vcol, ...}) = 
                     case vcol of
                       COLORED _ => (v, u)
                     | _         => (u, v)
             val _ = if debug then print ("Coalescing "^show u^"<->"^show v
                         ^" ("^Int.toString cost^")") else ()
             val mv = MV.merge(l, r)
             fun coalesceIt(status, v, undo) = 
                 (status := COALESCED;
                  if !spillFlag then UNDO(v, status, undo) else undo
                 )

             (* mark u and v as constrained *)
             fun constrained(status, u, v, mv, fz, stack, undo) =
                 let val _ = status := CONSTRAINED
                     val (mv, fz, stack) = decMoveCnt(u, 1, cost, mv, fz, stack)
                     val (mv, fz, stack) = decMoveCnt(v, 1, cost, mv, fz, stack)
                 in  (mv, fz, stack, undo) end

             (* coalesce u and v *)
             fun union(status, u, v, mv, fz, stack, undo) =
                 let val undo = coalesceIt(status, v, undo)
                     val (mv, fz, stack) = combine(u, v, cost, mv, fz, stack) 
                 in  (mv, fz, stack, undo) end

         in  if u' = v' then (* trivial move *)
                let val undo = coalesceIt(status, v, undo)
                    val (mv, fz, stack) = 
                         decMoveCnt(u, 2, cost+cost, mv, fz, stack)
                    val _ = if debug then print(" Trivial\n") else ()
                in  coalesce(mv, fz, stack, undo) end
             else 
                (case vcol of
                  COLORED _ => 
                      (* two colored nodes cannot be coalesced *)
                     (if debug then print(" Both Colored\n") else (); 
                      status := CONSTRAINED;
                      coalesce(mv, fz, stack, undo))
                | _ =>
                  if member(u, v) then 
                     (* u and v interfere *)
                    (if debug then print(" Interfere\n") else ();  
                     coalesce(constrained(status, u, v, mv, fz, stack, undo)))
                  else
                  case ucol of 
                    COLORED _ =>  (* u is colored, v is not *)
                    if safe(u, v) then 
                       (if debug then print(" Safe\n") else (); 
                        coalesce(union(status, u, v, mv, fz, stack, undo)))
                    else
                      (status := MOVE; (* remove it from the move list *)
                       if debug then print(" Unsafe\n") else (); 
                       coalesce(mv, fz, stack, undo)
                      )
                  |  _ => (* u, v are not colored *)
                   if conservative(u, v) then 
                     (if debug then print(" OK\n") else (); 
                      coalesce(union(status, u, v, mv, fz, stack, undo)))
                   else (* conservative test failed *)
                      (status := MOVE; (* remove it from the move list *)
                       if debug then print(" Non-conservative\n") else (); 
                       coalesce(mv, fz, stack, undo)
                      )
                )
         end

      (* mark a node n as frozen: 
       *  Go thru all the moves (n, m), decrement the move count of m
       *  precondition: degree must be < K
       *                movecnt must be > 0
       *    node  -- the node to be frozen
       *    fz    -- a queue of freeze candidates
       *    stack -- stack of removed nodes
       *)
      fun markAsFrozen(node as NODE{number=me, movelist, movecnt as ref mc,...},
                       fz, stack) = 
      let val _ = if debug then print("Mark as frozen "^Int.toString me^"\n")
                  else ()
          (* eliminate all moves, return a list of nodes that
           * can be simplified
           *)
          fun elimMoves([], simp) = simp
            | elimMoves(MV{status, src, dst, ...}::mvs, simp) =
              case !status of 
                WORKLIST => error "elimMoves"
              | MOVE => (* mark move as lost *)
                let val _ = status := LOST
                    val src as NODE{number=s,...} = chase src
                    val dst = chase dst
                    val you = if s = me then dst else src
                in  case you of
                      NODE{color=ref(COLORED _),...} => 
                        elimMoves(mvs, simp)
                    | NODE{movecnt as ref c, degree, ...} => (* pseudo *)
                        (movecnt := c - 1; 
                         if c = 1 andalso !degree < K then 
                            elimMoves(mvs, you::simp) 
                         else 
                            elimMoves(mvs, simp)
                        )
                end
              |  _   => elimMoves(mvs, simp)

          val (mv, fz, stack) = 
              if mc = 0 then (MV.EMPTY, fz, stack)
              else 
              let val _ = movecnt := 0; 
                  val simp = elimMoves(!movelist, [])
              in  simplifyAll(simp, MV.EMPTY, fz, stack) end
      in  simplify(node, mv, fz, stack)
      end

      (*
       * FREEZE: 
       *   Repeat picking 
       *   a node with degree < K from the freeze list and freeze it.
       *   fz    -- queue of freezable nodes 
       *   stack -- stack of removed nodes
       *   undo  -- trail of coalesced moves after potential spill
       *)
      fun freeze(fz, stack, undo) = 
      let fun loop(FZ.EMPTY, FZ.EMPTY, stack, undo) = (stack, undo)
            | loop(FZ.EMPTY, newFz, _, _) = error "no freeze candidate"
              (*
              let fun loop(FZ.TREE(n,_,l,r)) =
                      (print("Non candidate "^show n^"\n"); 
                       loop(FZ.merge(l,r)))
                    | loop(FZ.EMPTY) = ()
              in  dumpGraph G TextIO.stdOut;
                  loop newFz;
                  error "no freeze candidate"
              end
              *)
            | loop(FZ.TREE(node, _, l, r), newFz, stack, undo) =
              let val fz = FZ.merge(l, r)
              in  case node of
                     (* This node has not been simplified 
                      * This must be a move-related node.
                      *)
                     NODE{color=ref PSEUDO, degree, movecnt, movecost, ...} =>
                     if !degree >= K (* can't be frozen? *)
                     then (* if !movecnt = 0 (* non-freeze candidate *)
                          then loop(fz, newFz, stack, undo)
                          else *) loop(fz, FZ.add(node, newFz), stack, undo)
                     else (* freeze node *)
                     let val _ = if debug then 
                                  print("Freezing "^show node
                                       ^" movecnt="^Int.toString(!movecnt)^
                                        " ("^Int.toString(!movecost)^")\n") 
                                 else ()
                         val (mv, fz, stack) = markAsFrozen(node, fz, stack)
                         val (fz, stack, undo) = coalesce(mv, fz, stack, undo)
                     in  loop(FZ.merge(fz, newFz), FZ.EMPTY, stack, undo)
                     end
                  | _ => loop(fz, newFz, stack, undo)
              end
      in  loop(fz, FZ.EMPTY, stack, undo)
      end

      (*
       * Iterate over simplify, coalesce, freeze
       *)
      fun iterate{simplifyWkl, moveWkl, freezeWkl, stack} =
      let (* simplify everything *)
          val (mv, fz, stack)   = 
                  simplifyAll(simplifyWkl, moveWkl, freezeWkl, stack)
          val (fz, stack, undo) = coalesce(mv, fz, stack, !trail)
          val (stack, undo)     = freeze(fz, stack, undo)
      in  trail := undo;
          {stack=stack}
      end
  in  {markAsFrozen=markAsFrozen, iterate=iterate}
  end

  (*
   * The main entry point for the iterated coalescing algorithm
   *)
  fun iteratedCoalescing G = 
  let val {iterate,...} = iteratedCoalescingPhases G
  in  iterate end


  (*
   * Potential Spill:
   *   Find some node on the spill list and just optimistically
   * remove it from the graph.
   *)
  fun potentialSpillNode (G as G.GRAPH{spillFlag,...}) =
  let val {markAsFrozen,...} = iteratedCoalescingPhases G
  in  fn {node, stack} => 
      let val _ = spillFlag := true (* potential spill found *)
          val (mv, fz, stack) = markAsFrozen(node, FZ.EMPTY, stack) 
      in  {moveWkl=mv, freezeWkl=fz, stack=stack}
      end
  end



  (*
   *  SELECT: 
   *    Using optimistic spilling
   *)
  fun select(G as GRAPH{getreg, getpair, trail, firstPseudoR, stamp, 
                        spillFlag, proh, ...}) 
             {stack, biased} =
  let fun undoCoalesced END = ()
        | undoCoalesced(UNDO(NODE{number, color, ...}, status, trail)) =
          (status := MOVE;
           if number < firstPseudoR then () else color := PSEUDO;
           undoCoalesced trail
          )
      val show = show G

      (* Fast coloring, assume no spilling can occur *)
      fun fastcoloring([], stamp) = ([], stamp)
        | fastcoloring((node as NODE{color, (* pair, *) adj, ...})::stack, 
                       stamp) =
          let (* set up the proh array *)
              fun neighbors [] = ()
                | neighbors(r::rs) = 
                  let fun mark(NODE{color=ref(COLORED c), ...}) =
                           (UA.update(proh, c, stamp); neighbors rs)
                        | mark(NODE{color=ref(ALIASED n), ...}) = mark n
                        | mark _ = neighbors rs
                  in  mark r end
              val _ = neighbors(!adj)
          in  color := COLORED(getreg{pref=[], proh=proh, stamp=stamp});
              fastcoloring(stack, stamp+1) 
          end

      (* Briggs' optimistic spilling heuristic *)
      fun optimistic([], spills, stamp) = (spills, stamp)
        | optimistic((node as NODE{color, (* pair, *) adj, ...})::stack, 
                     spills, stamp) =
          let (* set up the proh array *)
              fun neighbors [] = ()
                | neighbors(r::rs) = 
                  let fun mark(NODE{color=ref(COLORED c), ...}) =
                           (UA.update(proh, c, stamp); neighbors rs)
                        | mark(NODE{color=ref(ALIASED n), ...}) = mark n
                        | mark _ = neighbors rs
                  in  mark r end
              val _ = neighbors(!adj)
              val spills = 
                  let val col = getreg{pref=[], proh=proh, stamp=stamp}
                  in  color := COLORED col; spills
                  end handle _ => node::spills
          in  optimistic(stack, spills, stamp+1) end

      (* Briggs' optimistic spilling heuristic, with biased coloring *)
      fun biasedColoring([], spills, stamp) = (spills, stamp)
        | biasedColoring(
             (node as NODE{number, color, adj, 
                           (* pair, *) movecnt, movelist,...})::stack, 
             spills, stamp) =
          let (* set up the proh array *)
              fun neighbors [] = ()
                | neighbors(r::rs) = 
                  (case chase r of
                     NODE{color=ref(COLORED c), ...} => 
                        (UA.update(proh, c, stamp); neighbors rs)
                   | _ => neighbors rs
                  )
              (* 
               * Look at lost moves and see if it is possible to 
               * color the move with the same color
               *)
              fun getPref([], pref) = pref
                | getPref(MV{status=ref LOST, src, dst, ...}::mvs, pref) =
                  let val src as NODE{number=s,...} = chase src
                      val dst = chase dst
                      val other = if s = number then dst else src 
                  in  case other of
                        NODE{color=ref(COLORED c),...} => getPref(mvs, c::pref)
                      | _ => getPref(mvs, pref)
                  end
                | getPref(_::mvs, pref) = getPref(mvs, pref)

              val _    = neighbors(!adj)
              val pref = getPref(!movelist,[])
              val spills = 
                  let val col = getreg{pref=[], proh=proh, stamp=stamp}
                  in  color := COLORED col; spills
                  end handle _ => node::spills
          in  biasedColoring(stack, spills, stamp+1) end
      val (spills, st) = if biased 
                         then biasedColoring(stack, [], !stamp)
                         else if !spillFlag then
                                  optimistic(stack, [], !stamp)
                              else
                                  fastcoloring(stack, !stamp)
  in  stamp := st;
      case spills of
        [] => {spills=[]}
      | spills => 
         (app (fn node as NODE{color,...} => color := PSEUDO) stack;
          undoCoalesced (!trail);
          trail := END;
          {spills=spills}
         )
  end

  (*
   * Spill coalescing.
   * Coalesce non-interfering moves between spilled nodes, 
   * in non-increasing order of move cost.
   *)
  fun spillCoalescing(GRAPH{bitMatrix, ...}) nodesToSpill = 
  let fun chase(NODE{color=ref(ALIASED n), ...}) = chase n
        | chase n = n
      fun collectMoves([], mv') = mv'
        | collectMoves(NODE{movelist, color=ref(SPILLED ~1), ...}::ns, mv') =
          let fun addMoves([], mv') = collectMoves(ns, mv')
                | addMoves((mv as MV{dst,src,status=ref LOST, ...})::mvs, mv') =
                   (case (chase dst, chase src) of
                      (NODE{color=ref(SPILLED ~1), number=d, ...},
                       NODE{color=ref(SPILLED ~1), number=s, ...}) =>
                          if d = s then addMoves(mvs, mv')
                          else addMoves(mvs, MV.add(mv, mv'))
                    | _ => addMoves(mvs, mv')
                   )
                | addMoves(_::mvs, mv') = addMoves(mvs, mv')
          in  addMoves(!movelist, mv') end
        | collectMoves(_::ns, mv') = collectMoves(ns, mv')

      val mvs = collectMoves(nodesToSpill, MV.EMPTY)

      val member = BM.member(!bitMatrix)
      val addEdge = BM.add(!bitMatrix)

      fun coalesceMoves(MV.EMPTY) = ()
        | coalesceMoves(MV.TREE(MV{dst, src, ...}, _, l, r)) =
          let val dst as NODE{number=d, color, adj=adjDst,
                              defs=defsDst, uses=usesDst, ...} = chase dst
              val src as NODE{number=s, adj=adjSrc, 
                              defs=defsSrc, uses=usesSrc, ...} = chase src
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
          in  if d = s orelse member(dst, src) then ()
              else ((* print(Int.toString d ^"<->"^Int.toString s^"\n"); *)
                    ra_spill_coal := !ra_spill_coal + 1;
                    color := ALIASED src; 
                    adjSrc := union(!adjDst, !adjSrc);
                    defsSrc := concat(!defsDst, !defsSrc);
                    usesSrc := concat(!usesDst, !usesSrc)
                   );
              coalesceMoves(MV.merge(l,r))
          end
  in  coalesceMoves mvs
  end

  (*
   * Spill propagation.
   *)
  fun spillPropagation(G as GRAPH{bitMatrix, ...}) nodesToSpill =
  let val spillCoalescing = spillCoalescing G
      exception SpillProp
      val visited = Intmap.new(32, SpillProp) : bool Intmap.intmap
      val hasBeenVisited = Intmap.mapWithDefault (visited, false)
      val markAsVisited = Intmap.add visited
      val member = BM.member(!bitMatrix)

      fun chase(NODE{color=ref(ALIASED n), ...}) = chase n
        | chase n = n
 
      (* compute savings due to spill coalescing.
       * The move list must be associated with a colorable node.
       *)
      fun coalescingSavings([], sc) = sc+sc
        | coalescingSavings(MV{dst, src, status=ref LOST, cost, ...}::mvs, sc) =
          (case (chase dst, chase src) of
             (dst as NODE{number=d, color=ref(SPILLED ~1), ...},
              src as NODE{number=s, ...}) => 
               if d = s orelse member(dst, src) then coalescingSavings(mvs, sc)
               else coalescingSavings(mvs, sc+cost)
           | (dst as NODE{number=d, ...},
              src as NODE{number=s, color=ref(SPILLED ~1),...}) => 
               if d = s orelse member(dst, src) then coalescingSavings(mvs, sc)
               else coalescingSavings(mvs, sc+cost)
           | _ => coalescingSavings(mvs, sc)
          )
        | coalescingSavings(_::mvs, sc) = coalescingSavings(mvs, sc)

      (* Insert all colorable neighbors into worklist *)
      fun insert([], worklist) = worklist
        | insert((node as NODE{color=ref PSEUDO, number, ...})::adj, worklist) =
          if hasBeenVisited number then insert(adj, worklist)
          else (markAsVisited (number, true);
                insert(adj, node::worklist))
        | insert(_::adj, worklist) = insert(adj, worklist)

      val marker = SPILLED(~1)

      (* Process all nodes from the worklist *)
      fun propagate([], new, spilled) = (new, spilled)
        | propagate(node::worklist, new, spilled) =
          let val NODE{pri=ref spillcost, color, number, 
                       adj, movelist, ...} = node
              val savings = coalescingSavings(!movelist, 0)
          in  if savings >= spillcost then  (* propagate spill *)
                 (ra_spill_prop := !ra_spill_prop + 1;
                  color := marker; (* spill the node *)
                  (* print("Propagating "^Int.toString number^"\n"); *)
                  propagate(insert(!adj, worklist), node::new, node::spilled)
                 )
              else
                 propagate(worklist, new, spilled)
          end

      (* Initialize worklist *)
      fun init([], worklist) = worklist
        | init(NODE{adj, color=ref(SPILLED ~1), ...}::rest, worklist) =
            init(rest, insert(!adj, worklist))
        | init(_::rest, worklist) = init(rest, worklist)

      (* 
       * Iterate between spill coalescing and propagation 
       *)
      fun iterate(spillWorkList, spilled) = 
      let val _ = spillCoalescing spillWorkList
          val propagationWorkList = init(spillWorkList, []) 
          val (newNodes, spilled) = propagate(propagationWorkList, [], spilled)
      in  case newNodes of
            [] => spilled
          | _  => iterate(newNodes, spilled)
      end
  in  iterate(nodesToSpill, nodesToSpill)
  end

  (*
   * Spill coloring.
   * Assign logical spill locations to all the spill nodes.
   *)
  fun spillColoring(GRAPH{spillLoc, ...}) nodesToSpill = 
  let val proh     = A.array(length nodesToSpill, ~1)
      val firstLoc = !spillLoc
      val _ = spillLoc := firstLoc - 1 (* allocate one location *)
      fun selectColor([], currLoc) = ()
        | selectColor(NODE{color as ref(SPILLED _), number, adj, ...}::nodes,
                      currLoc) = 
          let fun chase(NODE{color=ref(ALIASED n), ...}) = chase n 
                | chase n = n
              fun neighbors [] = ()
                | neighbors(n::ns) = 
                  (case chase n of
                      NODE{color=ref(SPILLED loc), ...} =>
                       if loc >= ~1 then () (* no location yet *) 
                       else A.update(proh, firstLoc - loc, number)
                   | _ => ();
                  neighbors ns
                 )
              val _ = neighbors(!adj)
              fun findColor(loc, startingPoint) = 
                  let val loc = if loc < firstLoc then !spillLoc + 1 else loc
                  in  if A.sub(proh, firstLoc - loc) <> number then loc (* ok *)
                      else if loc = startingPoint then (* new location *)
                      let val loc = !spillLoc 
                      in  spillLoc := loc - 1; loc end
                      else findColor(loc - 1, startingPoint)
                  end
              val currLoc = if currLoc < firstLoc then !spillLoc + 1 
                            else currLoc
              val loc = findColor(currLoc, currLoc)
          in  color := SPILLED loc;
              selectColor(nodes, loc - 1)
          end
        | selectColor(_::nodes, currLoc) = selectColor(nodes, currLoc)
  in  selectColor(nodesToSpill, firstLoc)
  end

  (*
   * Update the regmap, after finishing register allocation.
   * All nodes must have been colored.
   *)
  fun finishRA(GRAPH{regmap, nodes, deadCopies, ...}) = 
  let val enter = Intmap.add regmap
      fun set(r, NODE{color=ref(COLORED c),...}) = enter(r, c)
        | set(r, NODE{color=ref(ALIASED n),...}) = set(r, n)
        | set(r, NODE{color=ref(SPILLED _),...}) = enter(r,~1) (* XXX *)
        | set _ = error "finishRA"
  in  Intmap.app set nodes;
      case !deadCopies of
        [] => ()
      | dead => app (fn r => enter(r, ~1)) dead
  end

  (*
   * Update the regmap, after copy propagation
   *)
  fun finishCP(GRAPH{regmap, nodes,...}) =
  let val enter = Intmap.add regmap
  in  Intmap.app    
        (fn (r, node as NODE{color as ref(ALIASED _),...}) =>
            (case chase node of
               NODE{color=ref(COLORED c),...} => enter(r, c)
             | NODE{color=ref PSEUDO, number,...} => enter(r, number)
             | NODE{color=ref REMOVED, number,...} => enter(r, number)
             | _ => error "finishCP"
            )
          | _ => ()
        ) nodes
  end

  (*
   * Clear the interference graph, but keep the nodes 
   *)
  fun clearGraph(GRAPH{bitMatrix, maxRegs, trail, spillFlag, deadCopies, ...}) =
  let val edges = BM.edges(!bitMatrix)
  in  trail      := END;
      spillFlag  := false;
      bitMatrix  := BM.empty;
      deadCopies := [];
      bitMatrix  := G.newBitMatrix{edges=edges, maxRegs=maxRegs()}
  end 

  fun clearNodes(GRAPH{nodes,...}) =
  let fun init(_, NODE{pri, degree, adj, movecnt, movelist,
                       movecost, defs, uses, ...}) =
            (pri := 0; degree := 0; adj := []; movecnt := 0; movelist := [];
             defs := []; uses := []; movecost := 0)
  in  Intmap.app init nodes
  end

end 
