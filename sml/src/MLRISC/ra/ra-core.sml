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

  val debug = false
(*  val tally = false *)


  val verbose       = MLRiscControl.getFlag "ra-verbose"
  val ra_spill_coal = MLRiscControl.getCounter "ra-spill-coalescing"
  val ra_spill_prop = MLRiscControl.getCounter "ra-spill-propagation"

(*
  val good_briggs   = MLRiscControl.getCounter "good-briggs"
  val bad_briggs    = MLRiscControl.getCounter "bad-briggs"
  val good_george   = MLRiscControl.getCounter "good-george"
  val bad_george    = MLRiscControl.getCounter "bad-george"
  val good_freeze   = MLRiscControl.getCounter "good-freeze"
  val bad_freeze    = MLRiscControl.getCounter "bad-freeze"
 *)

  val NO_OPTIMIZATION     = 0wx0
  val BIASED_SELECTION    = 0wx1
  val DEAD_COPY_ELIM      = 0wx2
  val COMPUTE_SPAN        = 0wx4
  val SAVE_COPY_TEMPS     = 0wx8 
  val HAS_PARALLEL_COPIES = 0wx10
  val SPILL_COALESCING       = 0wx100
  val SPILL_COLORING         = 0wx200
  val SPILL_PROPAGATION      = 0wx400
  val MEMORY_COALESCING      = 
      SPILL_COALESCING + SPILL_COLORING + SPILL_PROPAGATION


  local

  fun isOn(flag,mask) = Word.andb(flag,mask) <> 0w0

  fun error msg = MLRiscErrorMsg.error("RACore", msg)
 
  (* No overflow checking necessary here *)
  fun x + y = W.toIntX(W.+(W.fromInt x, W.fromInt y))
  fun x - y = W.toIntX(W.-(W.fromInt x, W.fromInt y))

  fun concat([], b) = b
    | concat(x::a, b) = concat(a, x::b)

  in

  (*
   * Bit Matrix routines
   *)
  structure BM = 
  struct
     fun hashFun(i, j, shift, size) = 
     let val i    = W.fromInt i
         val j    = W.fromInt j
         val h    = W.+(W.<<(i, shift), W.+(i, j))
         val mask = W.-(W.fromInt size, 0w1)
     in  W.toIntX(W.andb(h, mask)) end

     val empty = BM{table=SMALL(ref(A.array(0, [])), 0w0), elems=ref 0, edges=0}

     (*
     val indices = A.array(1024,0)

     fun init(i,j) =
         if i < 1024 then
            (A.update(indices, i, j); init(i+1, i+j+1))
         else ()

     val _ = init(0, 0)
      *)
     fun size (BM{elems, ...}) = !elems

     fun edges(BM{table=SMALL(ref table, _), ...}) = A.length table
       | edges(BM{table=LARGE(ref table, _), ...}) = A.length table
     (*| edges(BM{table=BITMATRIX _, edges, ...}) = edges *)

     fun member(BM{table=SMALL(table, shift), ...}) =
         (fn (i, j) => 
          let val (i,j) = if i < j then (i, j) else (j, i)
              val k = W.+(W.<<(W.fromInt i, 0w15), W.fromInt j)
              fun find [] = false
                | find(k'::b) = k = k' orelse find b
              val tab = !table
          in  find(UA.sub(tab, hashFun(i, j, shift, A.length tab))) end
         )
       | member(BM{table=LARGE(table, shift), ...}) =
         (fn (i, j) => 
          let val (i,j) = if i < j then (i, j) else (j, i)
              fun find NIL = false
                | find(B(i',j',b)) = i = i' andalso j = j' orelse find b
              val tab = !table
          in  find(UA.sub(tab, hashFun(i, j, shift, A.length tab))) end
         )
       (*
       | member(BM{table=BITMATRIX table, ...}) =
         (fn (i, j) => 
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
   datatype pri_queue = TREE of elem * int * pri_queue * pri_queue | EMPTY

   fun merge'(EMPTY, EMPTY) = (EMPTY, 0)
     | merge'(EMPTY, a as TREE(_, d, _, _)) = (a, d)
     | merge'(a as TREE(_, d, _, _), EMPTY) = (a, d)
     | merge'(a as TREE(x, d, l, r), b as TREE(y, d', l', r')) =
       let val (root, l, r1, r2) = 
               if less(x, y) then (x, l, r, b) else (y, l', r', a) 
           val (r, d_r) = merge'(r1, r2)
           val d_l = case l of EMPTY => 0 | TREE(_, d, _, _) => d 
           val (l, r, d_t) = if d_l >= d_r then (l, r, d_l+1) else (r, l, d_r+1)
       in  (TREE(root, d_t, l, r), d_t) end

   fun merge(a, b) = #1(merge'(a, b))

   fun add (x, EMPTY) =  TREE(x, 1, EMPTY, EMPTY)
     | add (x, b as TREE(y, d', l', r')) = 
       if less(x,y) then TREE(x, d'+1, b, EMPTY)
       else #1(merge'(TREE(x, 1, EMPTY, EMPTY), b))
  end

  structure FZ = PriQueue
     (type elem=node 
      fun less(NODE{movecost=ref p1,...}, NODE{movecost=ref p2,...}) = p1 <= p2
     )
  structure MV = PriQueue
     (type elem=G.move 
      fun less(MV{cost=p1,...}, MV{cost=p2,...}) = p1 >= p2
     )

  type move_queue = MV.pri_queue
  type freeze_queue = FZ.pri_queue


  (*  
   * Utility functions
   *)
  fun chase(NODE{color=ref(ALIASED r), ...}) = chase r
    | chase x = x

  fun colorOf(G.GRAPH{showReg,...}) (NODE{number, color, pri,...}) =
       showReg number^
           (case !color of
              PSEUDO     => ""
            | REMOVED    => "r"
            | ALIASED _  => "a"
            | COLORED c  => "["^showReg c^"]"
            | SPILLED ~1 => "s"
            | SPILLED c  => (if c >= 0 then "m" else "s")^
                            (if c >= 0 andalso number = c then ""
                             else "{"^Int.toString c^"}")
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
      fun prMove(MV{src, dst, status=ref(WORKLIST | BRIGGS_MOVE | GEORGE_MOVE),
                    cost,...}) = 
            pr(colorOf(chase dst)^" <- "^colorOf(chase src)^
               "("^Int.toString(cost)^") ")
        | prMove _ = ()

      fun prAdj(n,n' as NODE{adj, degree, uses, defs,
                             color, pri, movecnt, movelist, ...}) =
          (pr(show n');
           if !verbose then pr(" deg="^Int.toString(!degree)) else ();
           (case !color of
              ALIASED n => (pr " => "; pr(show n); pr "\n")
            | _ =>
              (pr(" <-->");
               app (fn n => (pr " "; pr(show n))) (!adj); pr "\n";
               if !verbose andalso !movecnt > 0 then 
                 (pr("\tmoves "^Int.toString(!movecnt)^": ");
                  app prMove (!movelist);
                  pr "\n"
                 ) 
               else ()
              )
           )
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
   * Now we allow spilled node to be added to the edge; these do not
   * count toward the degree. 
   *)
  fun addEdge(GRAPH{bitMatrix,...}) =
  let val addBitMatrix = BM.add(!bitMatrix)
  in  fn (x as NODE{number=xn, color=colx, adj=adjx, degree=degx, ...}, 
          y as NODE{number=yn, color=coly, adj=adjy, degree=degy, ...}) => 
          if xn = yn then ()
          else if addBitMatrix(xn, yn) then
          (case (!colx, !coly) of
            (PSEUDO,    PSEUDO)    => (adjx := y :: !adjx; degx := !degx + 1;
                                       adjy := x :: !adjy; degy := !degy + 1)
          | (PSEUDO,    COLORED _) => (adjx := y :: !adjx; degx := !degx + 1)
          | (PSEUDO,    SPILLED _) => (adjx := y :: !adjx; adjy := x :: !adjy)
          | (COLORED _, PSEUDO)    => (adjy := x :: !adjy; degy := !degy + 1)
          | (COLORED _, COLORED _) => ()
          | (COLORED _, SPILLED _) => ()
          | (SPILLED _, PSEUDO)    => (adjx := y :: !adjx; adjy := x :: !adjy)
          | (SPILLED _, COLORED _) => ()
          | (SPILLED _, SPILLED _) => ()
          | _ => error "addEdge"
          )
          else () (* edge already there *)
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
        (GRAPH{nodes, K, bitMatrix, regmap, pseudoCount, blockedCount,
               firstPseudoR, deadCopies, memMoves, mode, ...}) {moves} =
  let 
      (* Remove duplicates from the moves *)
      (*
      local
         fun idOf(mv as MV{src=NODE{number=x, ...}, 
                           dst=NODE{number=y, ...}, ...}) = 
           if x < y then (x,y,mv) else (y,x,mv)
         fun order((a,b,mv1), (c,d,mv2)) = a < c orelse a = c andalso b < d 
         fun elimDup((a,b,mv)::mvs, mvs') = elimDup'(a,b,mv,mvs,mvs')
           | elimDup([], mvs') = mvs'
         and elimDup'(a,b,mv,[],mvs') = mv::mvs'
           | elimDup'(a:int,b:int,mv1 as MV{cost=c1,status,src,dst,hicount,...},
                      (c,d,mv2 as MV{cost=c2,...})::rest,mvs') =
             if a=c andalso b=d then 
                 elimDup'(a,b,MV{cost=c1+c2,status=status,
                                 src=src,dst=dst,hicount=hicount},
                          rest, mvs')
             else
                 elimDup'(c,d,mv2,rest,mv1::mvs')
          val moves = ListMergeSort.sort order (map idOf moves)
      in  
          val moves = elimDup(moves, [])
      end
      *)


      (* Filter moves that already have an interference
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

      fun filter([], mvs', mem) = (mvs', mem)
        | filter((mv as MV{src as NODE{number=x, color=ref colSrc,...},
                           dst as NODE{number=y, color=ref colDst,...}, 
                           cost, ...})::mvs, 
                 mvs', mem) =
          (case (colSrc, colDst) of
            (COLORED _, COLORED _) => filter(mvs, mvs', mem)
          | (SPILLED _, SPILLED _) => filter(mvs, mvs', mem)
          | (SPILLED _, _)         => filter(mvs, mvs', mv::mem)
          | (_, SPILLED _)         => filter(mvs, mvs', mv::mem)
          | _ => 
            if member(x, y)     (* moves that interfere *)
            then filter(mvs, mvs', mem) 
            else (setInfo(src, mv, cost);
                  setInfo(dst, mv, cost);
                  filter(mvs, MV.add(mv, mvs'), mem)
                 )
          )

      fun filter'([], mvs', mem, dead) = (mvs', mem, dead)
        | filter'((mv as 
                  MV{src as NODE{number=x, color as ref colSrc, 
                                 pri, adj, uses,...},
                     dst as NODE{number=y, color=ref colDst, 
                                 defs=dstDefs, uses=dstUses,...},
                     cost, ...})::mvs, 
                 mvs', mem, dead) =
          (case (colSrc, colDst, dstDefs, dstUses) of
            (COLORED _, COLORED _, _, _) => filter'(mvs, mvs', mem, dead)
          | (SPILLED _, SPILLED _, _, _) => filter'(mvs, mvs', mem, dead)
          | (SPILLED _, _, _, _)         => filter'(mvs, mvs', mv::mem, dead)
          | (_, SPILLED _, _, _)         => filter'(mvs, mvs', mv::mem, dead)
          | (_, PSEUDO, ref [pt], ref []) => 
            (* eliminate dead copy *)
            let fun decDegree [] = ()
                  | decDegree(NODE{color=ref PSEUDO, degree, ...}::adj) =
                      (degree := !degree - 1; decDegree adj)
                  | decDegree(_::adj) = decDegree adj
                fun elimUses([], _, uses, pri, cost) = (uses, pri)
                  | elimUses(pt::pts, pt' : int, uses, pri, cost) =
                    if pt = pt' then elimUses(pts, pt', uses, pri-cost, cost)
                    else elimUses(pts, pt', pt::uses, pri, cost)
                val (uses', pri') = elimUses(!uses, pt, [], !pri, cost);
            in  pri := pri';
                uses := uses';
                color := ALIASED src;
                decDegree(!adj);
                filter'(mvs, mvs', mem, y::dead)
            end
          | _ =>  (* normal moves *)
            if member(x, y)     (* moves that interfere *)
            then filter'(mvs, mvs', mem, dead) 
            else (setInfo(src, mv, cost);
                  setInfo(dst, mv, cost);
                  filter'(mvs, MV.add(mv, mvs'), mem, dead)
                 )
         )
            
      (*
       * Scan all nodes in the graph and check which worklist they should
       * go into.
       *)
      fun collect([], simp, fz, moves, spill, pseudos, blocked) =
         (pseudoCount := pseudos;
          blockedCount := blocked;
          {simplifyWkl = simp,
           moveWkl     = moves,
           freezeWkl   = fz,
           spillWkl    = spill
          }
         )
        | collect(node::rest, simp, fz, moves, spill, pseudos, blocked) = 
          (case node of
              NODE{color=ref PSEUDO, movecnt, degree, ...} =>
                 if !degree >= K then
                    collect(rest, simp, fz, moves, node::spill, 
                            pseudos+1, blocked)
                 else if !movecnt > 0 then
                    collect(rest, simp, FZ.add(node, fz), 
                            moves, spill, pseudos+1, blocked+1)
                 else
                    collect(rest, node::simp, fz, moves, spill, 
                            pseudos+1, blocked)  
           |  _ => collect(rest, simp, fz, moves, spill, pseudos, blocked)
          )

      (* First build the move priqueue *)
      val (mvs, mem) = 
                if isOn(mode, DEAD_COPY_ELIM) then
                let val (mvs, mem, dead) = filter'(moves, MV.EMPTY, [], [])
                in  deadCopies := dead; (mvs, mem)
                end
                else filter(moves, MV.EMPTY, [])

  in  memMoves := mem;  (* memory moves *)
      collect(Intmap.values nodes, [], FZ.EMPTY, mvs, [], 0, 0)
  end

  (*
   * Return a regmap that reflects the current interference graph.
   * Spilled registers are given the special value ~1
   *)
  fun regmap(G.GRAPH{nodes,...}) = 
  let val getnode = Intmap.map nodes
      fun num(NODE{color=ref(COLORED r),...}) = r
        | num(NODE{color=ref(ALIASED n),...}) = num n
        | num(NODE{color=ref(SPILLED s),...}) = if s >= 0 then s else ~1
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
       (G as GRAPH{K, bitMatrix, spillFlag, trail, stamp, mode,
                   pseudoCount, blockedCount, ...}) =
  let val member = BM.member(!bitMatrix)
      val addEdge = addEdge G
      val show = show G
      val memoryCoalescingOn = isOn(mode, MEMORY_COALESCING)

      val blocked = blockedCount 

      (*
       * SIMPLIFY node:
       *   precondition: node must be part of the interference graph (PSEUDO)
       *)
      fun simplify(node as NODE{color, number, adj, degree, (*pair,*)...},
                   mv, fz, stack) =
      let val _ = if debug then print("Simplifying "^show node^"\n") else ()
          fun forallAdj([], mv, fz, stack) = (mv, fz, stack)
            | forallAdj((n as NODE{color=ref PSEUDO, degree as ref d,...})::adj,
                        mv, fz, stack) =
              if d = K then 
              let val (mv, fz, stack) = lowDegree(n, mv, fz, stack)
              in  forallAdj(adj, mv, fz, stack) end
              else (degree := d - 1; forallAdj(adj, mv, fz, stack))
            | forallAdj(_::adj, mv, fz, stack) = forallAdj(adj, mv, fz, stack)
      in  color := REMOVED;
          pseudoCount := !pseudoCount - 1;
          forallAdj(!adj, mv, fz, node::stack) (* push onto stack *)
      end (* simplify *)

      and simplifyAll([], mv, fz, stack) = (mv, fz, stack)
        | simplifyAll(node::simp, mv, fz, stack) =
          let val (mv, fz, stack) = simplify(node, mv, fz, stack)
          in  simplifyAll(simp, mv, fz, stack) end

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
      and lowDegree(node as NODE{degree as ref d, movecnt, adj, color,...},
                    (* false, *) mv, fz, stack) = 
           (* normal edge *)
          (if debug then 
           print("DecDegree "^show node^" d="^Int.toString(d-1)^"\n") else (); 
           degree := K - 1;
           (* node is now low degree!!! *)
           let val mv = enableMoves(!adj, mv)
           in  if !movecnt > 0 then (* move related *)
                  (blocked := !blocked + 1; (mv, FZ.add(node, fz), stack))
               else (* non-move related, simplify now! *)
                  simplify(node, mv, fz, stack)
           end
          )
       (*
        | decDegree(node as NODE{degree as ref d, movecnt, adj, color,...},
                    true, mv, fz, stack) = (* register pair edge *)
          (degree := d - 2;
           if d >= K andalso !degree < K then 
             (* node is now low degree!!! *)
             let val mv = enableMoves(node :: !adj, mv)
             in  if !movecnt > 0 then (* move related *)
                    (blocked := !blocked + 1; (mv, FZ.add(node, fz), stack))
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
              fun addMv([], ns, mv) = enableMoves(ns, mv)
                | addMv((m as MV{status, hicount as ref hi, ...})::rest,
                        ns, mv) = 
                  (case !status of
                     (BRIGGS_MOVE | GEORGE_MOVE) => 
                       (* decrements hi, when hi <= 0 enable move *)
                       if hi <= 1 then
                         (status := WORKLIST; addMv(rest, ns, MV.add(m, mv)))
                       else
                         (hicount := hi-1; addMv(rest, ns, mv))
                   | _    => addMv(rest, ns, mv)
                  )
          in  (* make sure the nodes are actually in the graph *)
              case n of
                NODE{movelist, color=ref PSEUDO, movecnt,...} =>
                  if !movecnt > 0 then (* is it move related? *)
                     addMv(!movelist, ns, mv)
                  else
                     enableMoves(ns, mv)
              | _ => enableMoves(ns, mv)
          end (* enableMoves *)

     (*
      *  Brigg's conservative coalescing test:
      *    given: an unconstrained move (x, y)  
      *    return: true or false
      *)
     fun conservative(hicount,
                      x as NODE{degree=ref dx, adj=xadj, (* pair=px, *) ...},
                      y as NODE{degree=ref dy, adj=yadj, (* pair=py, *) ...}) =
         dx + dy < K orelse
         let (*  
              *  hi -- is the number of nodes with deg > K (without duplicates)
              *  n -- the number of nodes that have deg = K but not neighbors
              *         of both x and y
              *  We use the movecnt as a flag indicating whether
              *  a node has been visited.  A negative count is used to mark
              *  a visited node.
              *)
             fun undo([], extraHi) = 
                 extraHi <= 0 orelse (hicount := extraHi; false)
               | undo(movecnt::tr, extraHi) = 
                   (movecnt := ~1 - !movecnt; undo(tr, extraHi))
             fun loop([], [], hi, n, tr) = undo(tr, (hi + n) - K + 1)
               | loop([], yadj, hi, n, tr) = loop(yadj, [], hi, n, tr)
               | loop(NODE{color, movecnt as ref m, degree=ref deg, ...}::vs, 
                      yadj, hi, n, tr) =
                 (case !color of
                    COLORED _ =>
                      if m < 0 then
                         (* node has been visited before *)
                         loop(vs, yadj, hi, n, tr)
                      else
                        (movecnt := ~1 - m;  (* mark as visited *)
                         loop(vs, yadj, hi+1, n, movecnt::tr))
                  | PSEUDO =>
                      if deg < K then loop(vs, yadj, hi, n, tr)
                      else if m >= 0 then
                         (* node has never been visited before *)
                         (movecnt := ~1 - m;  (* mark as visited *)
                          if deg = K 
                          then loop(vs, yadj, hi, n+1, movecnt::tr)
                          else loop(vs, yadj, hi+1, n, movecnt::tr)
                         )
                      else
                         (* node has been visited before *)
                         if deg = K then loop(vs, yadj, hi, n-1, tr)
                         else loop(vs, yadj, hi, n, tr)
                  | _ => loop(vs, yadj, hi, n, tr) (* REMOVED/ALIASED *)
                 )
         in loop(!xadj, !yadj, 0, 0, []) end

     (*
      *  Heuristic used to determine whether a pseudo and machine register     
      *  can be coalesced. 
      *  Precondition:
      *     The two nodes are assumed not to interfere.
      *)
     fun safe(hicount, reg, NODE{adj, ...}) =
     let fun loop([], hi) = hi = 0 orelse (hicount := hi; false)
           | loop(n::adj, hi) =
             (case n of
               (* Note: Actively we only have to consider pseudo nodes and not
                * nodes that are removed, since removed nodes either have
                * deg < K or else optimistic spilling must be in effect!
                *)
               NODE{degree,number,color=ref(PSEUDO | REMOVED), ...} => 
               if !degree < K orelse member(reg, number) then loop(adj, hi)
               else loop(adj, hi+1)
             | _ => loop(adj, hi)
             )
     in  loop(!adj, 0) end

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
             then (blocked := !blocked - 1; simplify(node, mv, fz, stack))
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
      *  coloingv -- is u a colored node?
      *  mvcost -- the cost of the move that has been eliminated
      *  mv     -- the queue of moves
      *  fz     -- the queue of freeze candidates
      *  stack  -- stack of removed nodes
      *)
     fun combine(u, v, coloringv, mvcost, mv, fz, stack) =
     let val NODE{color=vcol, pri=pv, movecnt=cntv, movelist=movev, adj=adjv,
                  defs=defsv, uses=usesv, degree=degv, ...} = v
         val NODE{color=ucol, pri=pu, movecnt=cntu, movelist=moveu, adj=adju,
                  defs=defsu, uses=usesu, degree=degu, ...} = u

         (* merge movelists together, taking the opportunity
          * to prune the lists
          *)
         fun mergeMoveList([], mv) = mv
           | mergeMoveList((m as MV{status,hicount,src,dst,...})::rest, mv) = 
              (case !status of
                BRIGGS_MOVE =>  
                  (* if we are changing a copy from v <-> w to uv <-> w
                   * makes sure we reset its trigger count, so that it
                   * will be tested next.
                   *)
                  (if coloringv then 
                      (status := GEORGE_MOVE; 
                       hicount := 0;
                       if debug then 
                          print ("New george "^show src^"<->"^show dst^"\n")
                       else ()
                      )
                   else ();
                   mergeMoveList(rest, m::mv)
                  )
              | GEORGE_MOVE => 
                  (* if u is colored and v is not, then the move v <-> w
                   * becomes uv <-> w where w is colored.  This can always
                   * be discarded.
                   *)
                  (if coloringv then mergeMoveList(rest, mv)
                   else mergeMoveList(rest, m::mv)
                  )
              | WORKLIST => mergeMoveList(rest, m::mv)
              | _ => mergeMoveList(rest, mv)
              )

         (* Form combined node; add the adjacency list of v to u *)
         fun union([], mv, fz, stack) = (mv, fz, stack)
           | union((t as NODE{color, (* pair=pt, *)degree, ...})::adj, 
                   mv, fz, stack) =
              (case !color of
                 (COLORED _ | SPILLED _) => 
                   (addEdge(t, u); union(adj, mv, fz, stack))
               | PSEUDO =>
                   (addEdge(t, u);
                    let val d = !degree
                    in  if d = K then 
                        let val (mv, fz, stack) = lowDegree(t, mv, fz, stack)
                        in  union(adj, mv, fz, stack) end
                        else (degree := d - 1; union(adj, mv, fz, stack))
                    end
                   ) 
               | _ => union(adj, mv, fz, stack)
              )        

     in  vcol    := ALIASED u; 
                  (* combine the priority of both: 
                   * note that since the mvcost has been counted twice
                   * in the original priority, we substract it twice
                   * from the new priority.
                   *)
         pu    := !pu + !pv - mvcost - mvcost;
                  (* combine the def/use pts of both nodes.
                   * Strictly speaking, the def/use points of the move
                   * should also be removed.  But since we never spill
                   * a coalesced node and only spilling makes use of these
                   * def/use points, we are safe for now.  
                   *
                   * New comment: with spill propagation, it is necessary
                   * to keep track of the spilled program points.
                   *)
         if memoryCoalescingOn then
           (defsu := concat(!defsu, !defsv); 
            usesu := concat(!usesu, !usesv)
           )
         else ();
         case !ucol of
           PSEUDO => 
             (if !cntv > 0 then 
                 (if !cntu > 0 then blocked := !blocked - 1 else ();
                  moveu := mergeMoveList(!movev, !moveu) 
                 )
              else (); 
              movev := []; (* XXX kill the list to free space *)
              cntu  := !cntu + !cntv
             )
         | _ => ()
         ;
         cntv := 0;

         let val removingHi = !degv >= K andalso (!degu >= K orelse coloringv) 
             (* Update the move count of the combined node *)
             val (mv, fz, stack) = union(!adjv, mv, fz, stack)
             val (mv, fz, stack) = 
                 decMoveCnt(u, 2, mvcost + mvcost, mv, fz, stack)  
             (* If either v or u are high degree then at least one high degree
              * node is removed from the neighbors of uv after coalescing
              *)
             val mv = if removingHi then enableMoves(!adju, mv) else mv
         in  coalesce(mv, fz, stack)
         end
     end

     (*
      *  COALESCE:
      *    Repeat coalescing and simplification until mv is empty.
      *)
     and coalesce(MV.EMPTY, fz, stack) = (fz, stack)
       | coalesce(MV.TREE(MV{src, dst, status, hicount, cost, ...}, _, l, r), 
                  fz, stack) = 
         let (* val _ = coalesce_count := !coalesce_count + 1 *)
             val u = chase src
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
             fun coalesceIt(status, v) = 
                (status := COALESCED;
                 if !spillFlag then trail := UNDO(v, status, !trail) else ()
                )
         in  if u' = v' then (* trivial move *)
                let val _ = if debug then print(" Trivial\n") else ()
                    val _ = coalesceIt(status, v)
                in  coalesce(decMoveCnt(u, 2, cost+cost, mv, fz, stack))
                end
             else 
                (case vcol of
                  COLORED _ => 
                      (* two colored nodes cannot be coalesced *)
                     (status := CONSTRAINED;
                      if debug then print(" Both Colored\n") else (); 
                      coalesce(mv, fz, stack))
                | _ =>
                  if member(u', v') then 
                     (* u and v interfere *)
                    let val _ = status := CONSTRAINED
                        val _ = if debug then print(" Interfere\n") else ();  
                        val (mv, fz, stack) = 
                                decMoveCnt(u, 1, cost, mv, fz, stack)
                    in  coalesce(decMoveCnt(v, 1, cost, mv, fz, stack)) end
                  else
                  case ucol of 
                    COLORED _ =>  (* u is colored, v is not *)
                    if safe(hicount, u', v) then 
                      (if debug then print(" Safe\n") else (); 
                       (*if tally then good_george := !good_george+1 else ();*)
                       coalesceIt(status, v);
                       combine(u, v, true, cost, mv, fz, stack)
                      ) 
                    else
                      ((* remove it from the move list *)
                       status := GEORGE_MOVE;
                       (*if tally then bad_george := !bad_george + 1 else ();*)
                       if debug then print(" Unsafe\n") else (); 
                       coalesce(mv, fz, stack)
                      )
                  |  _ => (* u, v are not colored *)
                   if conservative(hicount, u, v) then 
                      (if debug then print(" OK\n") else (); 
                       (*if tally then good_briggs := !good_briggs+1 else ();*)
                       coalesceIt(status, v);
                       combine(u, v, false, cost, mv, fz, stack)
                      )
                   else (* conservative test failed *)
                      ((* remove it from the move list *)
                       status := BRIGGS_MOVE;
                       (*if tally then bad_briggs := !bad_briggs + 1 else ();*)
                       if debug then print(" Non-conservative\n") else (); 
                       coalesce(mv, fz, stack)
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
      fun markAsFrozen(
            node as NODE{number=me, degree, 
                         adj, movelist, movecnt as ref mc,...},
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
              | (BRIGGS_MOVE | GEORGE_MOVE) => (* mark move as lost *)
                let val _ = status := LOST
                    val src as NODE{number=s,...} = chase src
                    val you = if s = me then chase dst else src
                in  case you of
                      NODE{color=ref(COLORED _),...} => 
                        elimMoves(mvs, simp)
                    | NODE{movecnt as ref c, degree, ...} => (* pseudo *)
                        (movecnt := c - 1; 
                         if c = 1 andalso !degree < K then 
                           (blocked := !blocked - 1; 
                            elimMoves(mvs, you::simp))
                         else 
                            elimMoves(mvs, simp)
                        )
                end
              |  _   => elimMoves(mvs, simp)

          (* Note:
           * We are removing a high degree node, so try to enable all moves 
           * associated with its neighbors.
           *)

          val mv = if !degree >= K then enableMoves(!adj, MV.EMPTY) 
                   else MV.EMPTY

      in  if mc = 0 
          then simplify(node, mv, fz, stack)
          else 
             (movecnt := 0; 
              simplifyAll(node::elimMoves(!movelist, []), mv, fz, stack) 
             )
      end

      (*
       * FREEZE: 
       *   Repeat picking 
       *   a node with degree < K from the freeze list and freeze it.
       *   fz    -- queue of freezable nodes 
       *   stack -- stack of removed nodes
       *   undo  -- trail of coalesced moves after potential spill
       *)
      fun freeze(fz, stack) = 
      let fun loop(FZ.EMPTY, FZ.EMPTY, stack) = stack
            | loop(FZ.EMPTY, newFz, _) = error "no freeze candidate"
            | loop(FZ.TREE(node, _, l, r), newFz, stack) =
              let val fz = FZ.merge(l, r)
              in  case node of
                     (* This node has not been simplified 
                      * This must be a move-related node.
                      *)
                     NODE{color=ref PSEUDO, degree, ...} =>
                     if !degree >= K (* can't be frozen yet? *)
                     then 
                        ((*if tally then bad_freeze := !bad_freeze+1 else ();*)
                         loop(fz, FZ.add(node,newFz), stack))
                     else (* freeze node *)
                     let val _ = 
                            if debug then print("Freezing "^show node^"\n")
                            else ()
                         (*val _ = 
                            if tally then good_freeze := !good_freeze + 1
                            else ()*)
                         val _ = blocked := !blocked - 1; 
                         val (mv, fz, stack) = markAsFrozen(node, fz, stack)
                         val (fz, stack) = coalesce(mv, fz, stack)
                     in  if !blocked = 0 
                         then ((* print "[no freezing again]"; *) stack)
                         else ((* print("[freezing again "^
                               Int.toString(!blocked)^"]"); *)
                               loop(FZ.merge(fz, newFz), FZ.EMPTY, stack))
                     end
                  | _ => 
                    ((*if tally then bad_freeze := !bad_freeze + 1 else ();*)
                     loop(fz, newFz, stack))
              end
      in  if !blocked = 0 then ((* print "[no freezing]"; *) stack)
          else ((* print("[freezing "^Int.toString(!blocked)^"]"); *)
                loop(fz, FZ.EMPTY, stack))
      end

      (* 
       * Sort simplify worklist in increasing degree.
       * Matula and Beck suggests that we should always remove the
       * node with the lowest degree first.  This is an approximation of
       * the idea. 
       *)
      (*
      val buckets = A.array(K, []) : G.node list A.array
      fun sortByDegree nodes =
      let fun insert [] = ()
            | insert((n as NODE{degree=ref deg, ...})::rest) =
              (UA.update(buckets, deg, n::UA.sub(buckets, deg)); insert rest)
          fun collect(~1, L) = L
            | collect(deg, L) = collect(deg-1, concat(UA.sub(buckets, deg), L))
      in  insert nodes; 
          collect(K-1, [])
      end
       *)

      (*
       * Iterate over simplify, coalesce, freeze
       *)
      fun iterate{simplifyWkl, moveWkl, freezeWkl, stack} =
      let (* simplify everything *)
          val (mv, fz, stack) = 
                 simplifyAll((* sortByDegree *) simplifyWkl, 
                             moveWkl, freezeWkl, stack)
          val (fz, stack) = coalesce(mv, fz, stack)
          val stack       = freeze(fz, stack)
      in  {stack=stack}
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
      val spilled = SPILLED ~1
  in  fn {node, cost, stack} => 
      let val _ = spillFlag := true (* potential spill found *)
          val (mv, fz, stack) = markAsFrozen(node, FZ.EMPTY, stack)
      in  if cost < 0.0 then
             let val NODE{color, ...} = node in color := spilled end
          else ();
          {moveWkl=mv, freezeWkl=fz, stack=stack}
      end
  end



  (*
   *  SELECT: 
   *    Using optimistic spilling
   *)
  fun select(G as GRAPH{getreg, getpair, trail, firstPseudoR, stamp, 
                        spillFlag, proh, mode, ...}) {stack} =
  let fun undoCoalesced END = ()
        | undoCoalesced(UNDO(NODE{number, color, ...}, status, trail)) =
          (status := BRIGGS_MOVE;
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
        | optimistic((node as NODE{color=ref(SPILLED _), ...})::stack,  
                     spills, stamp) =
             optimistic(stack, node::spills, stamp)
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
        | biasedColoring((node as NODE{color=ref(SPILLED _), ...})::stack,  
                         spills, stamp) =
             biasedColoring(stack, node::spills, stamp)
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
                | getPref(MV{status=ref(LOST | BRIGGS_MOVE | GEORGE_MOVE), 
                             src, dst, ...}::mvs, pref) =
                  let val src as NODE{number=s,...} = chase src
                      val other = if s = number then chase dst else src 
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
      val (spills, st) = if isOn(mode, BIASED_SELECTION) 
                         then biasedColoring(stack, [], !stamp)
                         else if !spillFlag then
                                  optimistic(stack, [], !stamp)
                              else
                                  fastcoloring(stack, !stamp)
  in  stamp := st;
      case spills of
        [] => {spills=[]}
      | spills => 
        let fun undo [] = ()
              | undo(NODE{color,...}::nodes) = (color := PSEUDO; undo nodes)
        in  undo stack;
            undoCoalesced (!trail);
            trail := END;
            {spills=spills}
        end
  end

  (*
   * Incorporate memory<->register moves into the interference graph
   *)
  fun initMemMoves(GRAPH{memMoves, ...}) =
  let fun move(NODE{movelist, movecost, ...}, mv, cost) = 
          (movelist := mv :: !movelist;
           movecost := cost + !movecost
          )

      fun setMove(dst, src, mv, cost) = 
          (move(dst, mv, cost); move(src, mv, cost))

      fun init [] = ()
        | init((mv as MV{dst, src, cost, ...})::mvs) = 
          let val dst as NODE{color=dstCol, ...} = chase dst
              val src as NODE{color=srcCol, ...} = chase src
          in  case (!dstCol, !srcCol) of
                (SPILLED x, SPILLED y) => setMove(dst, src, mv, cost)
              | (SPILLED _, PSEUDO)    => setMove(dst, src, mv, cost)
              | (PSEUDO,    SPILLED _) => setMove(dst, src, mv, cost)
              | (SPILLED _, COLORED _) => () (* skip *)
              | (COLORED _, SPILLED _) => () (* skip *)
              | _ => error "initMemMoves" ;
              init mvs
          end
      val moves = !memMoves 
  in  memMoves := [];
      init moves
  end


  (*
   * Compute savings due to memory<->register moves
   *)
  fun moveSavings(GRAPH{memMoves=ref [], ...}) = (fn node => 0)
    | moveSavings(GRAPH{memMoves, bitMatrix, ...}) = 
  let exception Savings
      val savingsMap = Intmap.new(32, Savings)
               : {pinned:int,cost:int} Intmap.intmap
      val savings = Intmap.mapWithDefault(savingsMap, {pinned= ~1, cost=0})
      val addSavings = Intmap.add savingsMap
      val member     = BM.member(!bitMatrix)
      fun incSavings(u, v, c) =
      let val {pinned, cost} = savings u
      in  if pinned <> ~1 andalso v <> pinned orelse member(u, v)
          then ()
          else addSavings(u, {pinned=v, cost=cost + c + c})
      end
      fun computeSavings [] = ()
        | computeSavings(MV{dst, src, cost, ...}::mvs) =
          let val src as NODE{number=u, color=cu, ...} = chase src
              val dst as NODE{number=v, color=cv, ...} = chase dst
          in  case (!cu, !cv) of
                (SPILLED _, PSEUDO) => incSavings(v, u, cost)
              | (PSEUDO, SPILLED _) => incSavings(u, v, cost)
              | _ => ();
              computeSavings mvs
          end
  in  computeSavings (!memMoves);
      fn node => #cost(savings node)
  end

  (*
   * Update the regmap, after finishing register allocation.
   * All nodes must have been colored.
   *)
  fun finishRA(GRAPH{regmap, nodes, deadCopies, ...}) = 
  let val enter = Intmap.add regmap
      fun set(r, NODE{color=ref(COLORED c),...}) = enter(r, c)
        | set(r, NODE{color=ref(ALIASED n),...}) = set(r, n)
        | set(r, NODE{color=ref(SPILLED s),...}) = 
             enter(r,if s >= 0 then s else ~1) (* XXX *)
        | set(r, _) = error("finishRA "^Int.toString r)
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
  fun clearGraph(GRAPH{bitMatrix, maxRegs, trail, spillFlag, 
                       deadCopies, memMoves, copyTmps, ...}) =
  let val edges = BM.edges(!bitMatrix)
  in  trail      := END;
      spillFlag  := false;
      deadCopies := [];
      memMoves   := [];
      copyTmps   := [];
      bitMatrix  := BM.empty;
      bitMatrix  := G.newBitMatrix{edges=edges, maxRegs=maxRegs()}
  end 

  fun clearNodes(GRAPH{nodes,...}) =
  let fun init(_, NODE{pri, degree, adj, movecnt, movelist,
                       movecost, defs, uses, ...}) =
            (pri := 0; degree := 0; adj := []; movecnt := 0; movelist := [];
             defs := []; uses := []; movecost := 0)
  in  Intmap.app init nodes
  end

  end (* local *)

end 
