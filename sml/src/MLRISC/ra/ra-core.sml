(*
 * Core of the register allocator
 *
 * -- Allen
 *)

structure RACore : RA_CORE =
struct

  structure G = RAGraph

  open G

  val spillRegSentinel = ~1

  (*  
   * Utility functions
   *)
  fun error msg = MLRiscErrorMsg.error("RACore",msg)

  fun nodeNumber(NODE{number, ...}) = number

  fun isMoveRelated(NODE{movecnt=ref 0, ...}) = false
    | isMoveRelated _ = true

  fun chase(NODE{color=ref(ALIASED r), ...}) = chase r
    | chase x = x

    (* debugging *)
  fun dumpGraph(GRAPH{nodes,...}) = 
  let fun prList (l:int list,msg:string) = let
          fun pr [] = print "\n"
            | pr (x::xs) = (print (Int.toString x ^ " "); pr xs)
      in
            print msg; pr l
      end
      fun prAdj(nodes, n)= prList(map (nodeNumber o chase) nodes, n)
  in  Intmap.app
        (fn (n, NODE{adj, ...}) =>
            prAdj (!adj, Int.toString(n) ^ " <--> "))
        nodes
  end

    (* 
     * Update the regmap to be consistent with the nodes set,
     * After register allocation.  This means that all nodes have been colored
     *)
  fun finishRA(GRAPH{regmap,nodes,...}) =
  let val enter = Intmap.add regmap
  in  Intmap.app
         (fn (i, node) =>
            case chase node
              of NODE{color=ref(COLORED col), ...} => enter(i,col)
               | _ => error "finishRA"
             (*esac*))
          nodes
  end

    (* 
     * Update the regmap to be consistent with the nodes set, 
     * after copy propagation.
     *)
  fun finishCP(GRAPH{regmap,nodes,...}) = 
  let val enter = Intmap.add regmap
  in  Intmap.app
       (fn (i, node as NODE{color as ref (ALIASED _), ...}) =>
           (case (chase node) of 
             NODE{color=ref(COLORED col), ...} => enter(i, col)
           | NODE{color=ref PSEUDO, number, ...} => enter(i, number)
           | NODE{color=ref REMOVED, number, ...} => enter(i,number)
           | _ => error "finishP"
           (*esac*))
        | _ => ())
        nodes
  end


    (* add an edge to the interference graph.
     * note --- adjacency lists for machine registers are not maintained.
     *)
  fun addEdge (GRAPH{bitMatrix,...}) =
  let val addBitMatrix = BM.add bitMatrix
      fun add(r as NODE{color=ref PSEUDO, adj, degree,...}, s) =
            (adj := s :: !adj; degree := 1 + !degree)
        | add(NODE{color=ref(ALIASED _), ...}, _) = 
            error "addEdge.add: ALIASED"
        | add(NODE{color=ref(REMOVED), ...}, _) = 
            error "addEdge.add: REMOVED"
        | add _ = ()
  in  fn (x as NODE{number=xn, ...}, y as NODE{number=yn, ...}) => 
          if xn = yn then ()
          else if addBitMatrix(if xn < yn then (xn, yn) else (yn, xn)) then
               (add(x, y); add(y, x))
          else () 
  end

  (*
   * Activate moves associated with a node and its neighbors 
   *)
  fun enableMoves(node as NODE{adj, ...}, moveWkl) = 
  let fun addMvWkl([], wkl) = wkl
        | addMvWkl((mv as MV{status, ...})::rest, wkl) =
           (case !status
             of MOVE => 
                 (status := WORKLIST; addMvWkl(rest, mv::wkl))
              | _ => addMvWkl(rest, wkl)
           (*esac*))
     fun add([], wkl) = wkl
       | add((node as NODE{movelist, color=ref PSEUDO,...})::ns, wkl) = 
         if isMoveRelated node then
            add(ns, addMvWkl(!movelist, wkl))
         else
            add(ns, wkl)
       | add(_::ns, wkl) = add(ns,wkl)
  in
     add(node:: (!adj), moveWkl)
  end
  

  (*
   * Decrement the degree associated with a node returning a potentially
   * new set of worklists --- simplifyWkl, freezeWkl, and moveWkl.
   *)
  fun decrementDegree(K,node as (NODE{degree as ref d, ...}),
                        simpWkl,fzWkl,mvWkl) = 
     (degree := d - 1;
      if d = K then 
      let val moveWkl = enableMoves(node, mvWkl)
      in  if isMoveRelated(node) then
            (simpWkl, node::fzWkl, moveWkl)
          else
            (node::simpWkl, fzWkl, moveWkl)
      end
      else
        (simpWkl, fzWkl, mvWkl)
     )

    

  (* ------------------------------------------------------------------------
   * Core phases
   * ------------------------------------------------------------------------*)

                    (*--------build worklists----------*)

   (* make initial worklists. Note: register aliasing may have
    * occurred due to previous rounds of graph-coloring; therefore
    * nodes may already be colored or aliased.
    *)
  fun makeWorkLists (GRAPH{nodes,K,...}) initialMoves = 
  let fun iter([], simpWkl, fzWkl, spillWkl) =
           {simplifyWkl = simpWkl,
            freezeWkl   = fzWkl,
            spillWkl    = spillWkl,
            moveWkl     = initialMoves,
            stack       = []}
         | iter(node::rest, simpWkl, fzWkl, spillWkl) =
          (case node
           of NODE{color=ref PSEUDO, degree, ...} =>
               if !degree >= K then
                 iter(rest, simpWkl, fzWkl, node::spillWkl)
               else if isMoveRelated(node) then
                  iter(rest, simpWkl, node::fzWkl, spillWkl)
               else
                  iter(rest, node::simpWkl, fzWkl, spillWkl)
            | _ =>
               iter(rest, simpWkl, fzWkl, spillWkl)
          (*esac*))
  in  iter(Intmap.values nodes, [], [], [])
  end

                   (*---------simplify-----------*)
  fun simplifyPhase(GRAPH{K,...}) =
  let   
      (* for every node removed from the simplify worklist, decrement the
       * degree of all of its neighbors, potentially adding the neighbor
       * to the simplify worklist.
       *)
      fun simplify{simplifyWkl,freezeWkl,spillWkl,moveWkl,stack} = 
      let fun loop([], fzWkl, mvWkl, stack) = 
                 {simplifyWkl=[], freezeWkl=fzWkl, moveWkl=mvWkl, 
                  stack=stack, spillWkl=spillWkl}
            | loop((node as NODE{color as ref PSEUDO, adj, number, ...})::wkl, 
                 fzWkl, mvWkl, stack) = 
              let fun forallAdj([], simpWkl, fzWkl, mvWkl) = 
                         loop(simpWkl, fzWkl, mvWkl, node::stack)
                    | forallAdj((n as NODE{color as ref PSEUDO, ...})::rest, 
                         wkl, fzWkl, mvWkl) = 
                    let val (wkl, fzWkl, mvWkl) = 
                        decrementDegree(K, n, wkl, fzWkl, mvWkl)
                  in  forallAdj(rest, wkl, fzWkl, mvWkl)
                  end
                | forallAdj(_::rest, simpWkl, fzWkl, mvWkl) = 
                    forallAdj(rest, simpWkl, fzWkl, mvWkl)
            in color := REMOVED;
               (* print("Simplifying "^Int.toString number^"\n"); *)
               forallAdj(!adj, wkl, fzWkl, mvWkl)
            end
          | loop(_::ns, fzWkl, mvWkl, stack) = loop(ns, fzWkl, mvWkl, stack)
      in            
        loop(simplifyWkl, freezeWkl, moveWkl, stack) 
      end 
  in  simplify
  end  (* simplify *)



                    (*-----------coalesce-------------*)
  fun coalescePhase(G as GRAPH{K,spillFlag,undoInfo,bitMatrix,...}) = 
  let val memBitMatrix = BM.member bitMatrix
      fun member(NODE{number=x,...}, NODE{number=y,...}) =
          memBitMatrix (if x<y then (x, y) else (y, x))
      val addEdge = addEdge G

      fun coalesce{moveWkl, simplifyWkl, freezeWkl, spillWkl, stack} = 
      let (* v is being replaced by u *)
         fun combine(v as NODE{color=cv, movecnt, movelist=mv, adj, ...}, 
                     u as NODE{color=cu, movelist=mu, ...}, 
                     mvWkl, simpWkl, fzWkl) = let
           (* merge moveList entries, taking the opportunity 
            * to prune the lists 
            *)
           fun mergeMoveLists([], [], mvs) = mvs
             | mergeMoveLists([], xmvs, mvs) = mergeMoveLists(xmvs, [], mvs)
             | mergeMoveLists((mv as MV{status,...})::rest, other, mvs) = 
                (case !status
                  of (MOVE | WORKLIST) =>
                        mergeMoveLists(rest, other, mv::mvs)
                   | _ => mergeMoveLists(rest, other, mvs)
                (*esac*))
   
           (* form combined node *)
           fun union([], mvWkl, simpWkl, fzWkl) = (mvWkl, simpWkl, fzWkl)
             | union((t as NODE{color, ...})::rest, mvWkl, simpWkl, fzWkl) =
               (case color
                 of ref (COLORED _) =>
                     (addEdge(t, u); union(rest, mvWkl, simpWkl, fzWkl))
                  | ref PSEUDO =>
                     ((* the order of addEdge and decrementDegree is important *)
                      addEdge (t, u);
                      let val (wkl, fzWkl, mvWkl) =
                                decrementDegree(K, t, simpWkl, fzWkl, mvWkl)
                        in
                        union(rest, mvWkl, wkl, fzWkl)
                      end)
                  | _ => union(rest, mvWkl, simpWkl, fzWkl)
                (*esac*))
        in
           cv := ALIASED u;
           movecnt := 0;
           case cu 
            of ref PSEUDO => mu := mergeMoveLists(!mu, !mv, [])
            | _ => ()
           (*esac*);
           union(!adj, mvWkl, simpWkl, fzWkl)
        end (*combine*)
   
        (* If a node is no longer move-related as a result of coalescing,
         * and can become candidate for the  next round of simplification.
         *)
        fun addWkl(node as NODE{color=ref PSEUDO, 
                                 movecnt as ref mc, 
                                 degree, ...},  c, wkl) = let
               val ncnt = mc - c
             in
               if  ncnt <> 0 then (movecnt := ncnt; wkl)
               else if !degree >= K then wkl
               else node::wkl
             end  
           | addWkl(_, _, wkl) = wkl
   
        (* heuristic used to determine if a pseudo and machine register
         * can be coalesced.
         *)
        fun safe(r, NODE{adj, ...}) = let
           fun f [] = true
             | f (NODE{color=ref(COLORED _), ...}::rest) = f rest
             | f (NODE{color=ref(ALIASED _), ...}::rest) = f rest
             | f ((x as NODE{degree, ...})::rest) = 
               (!degree < K orelse member(x, r)) andalso f rest
        in
           f(!adj)
        end
   
        (* return true if Briggs et.al. conservative heuristic applies  *)
        fun conservative(x as NODE{degree=ref dx, adj=ref xadj, ...},
                          y as NODE{degree=ref dy, adj=ref yadj, ...}) =
           dx + dy < K 
          orelse let
              (* movecnt is used as a temporary scratch to record high degree
               * or colored nodes we have already visited
               * ((movecnt = ~1) => visited)
               * K-k is the number of nodes with deg > K
               * n is the number of nodes with deg = K but not neighbors of
               *    both x and y
               *
               * Note: I've replaced the old version by a purely tail
               * recursive version.  This works faster.
               *)
              datatype trail = TR of int ref * int * trail | NIL
              fun undo(NIL,v) = v
                | undo(TR(movecnt,m,tr),v) = (movecnt := m; undo(tr,v))
              fun g(_, _, 0, n, tr) = undo(tr,false)
                | g([], [], k, n, tr) = undo(tr, k > n)
                | g([], yadj, k, n, tr) = g(yadj, [], k, n, tr)
                | g(NODE{color=ref REMOVED, ...}::vs,yadj,k,n,tr) = 
                      g(vs,yadj,k,n,tr)
                | g(NODE{color=ref(ALIASED _), ...}::vs,yadj,k,n,tr) = 
                      g(vs,yadj,k,n,tr)
                | g(NODE{movecnt=ref ~1,degree, ...}::vs, yadj,k,n,tr) = 
                      if !degree = K then g(vs,yadj,k,n-1,tr)
                      else g(vs,yadj,k,n,tr)
                | g(NODE{movecnt as ref m,color=ref(COLORED _),...}::vs,
                         yadj,k,n,tr) =
                      (movecnt := ~1; g(vs,yadj,k-1,n,TR(movecnt,m,tr)))
                | g(NODE{movecnt as ref m,
                         degree = ref deg, color=ref PSEUDO,...}::vs, yadj, 
                         k, n, tr) =
                    if deg < K then g(vs, yadj, k, n, tr)
                    else if deg = K then 
                         (movecnt := ~1;
                          g(vs, yadj, k, n+1, TR(movecnt,m,tr)))
                    else (movecnt := ~1;
                          g(vs, yadj, k-1, n, TR(movecnt,m,tr)))
            in g(xadj, yadj, K, 0, NIL)
            end
   
        (* iterate over move worklist *)
        fun doMoveWkl((mv as MV{src,dst,status,...})::rest, wkl, fzWkl) = let
               val (u as NODE{number=u', color as ref ucol, ...},
                    v as NODE{number=v', movecnt as ref vCnt, ...}) = 
                          case (chase src, chase dst)
                           of (x, y as NODE{color=ref (COLORED _),...}) => (y,x)
                            | (x,y) => (x,y)
               (* val _ = print("Coalescing "^Int.toString u'^"<->"^
                                           Int.toString v') *)
              fun coalesceIt() =
                 (status := COALESCED;
                  if !spillFlag then undoInfo := (v, status) :: (!undoInfo)
                  else ())
             in 
               if u' = v' then
                 (coalesceIt (); (* print " Trivial\n"; *)
                  doMoveWkl(rest, addWkl(u, 2, wkl), fzWkl))
               else 
                (case v 
                  of NODE{color=ref(COLORED _),  ...} =>
                      (status := CONSTRAINED;
                       (* print " Both colored\n"; *)
                       doMoveWkl(rest, wkl, fzWkl))
                   | _ =>                        (* v is a pseudo register *)
                      if member (v, u) then
                        (status := CONSTRAINED;
                         (* print " Interfere\n"; *)
                         doMoveWkl(rest, addWkl(v,1,addWkl(u,1,wkl)), fzWkl))
                      else 
                       (case ucol
                         of COLORED _ =>
                            (* coalescing a pseudo and machine register *)
                             if safe(u,v) then
                              (coalesceIt();
                               (* print " Safe\n"; *)
                               doMoveWkl(combine(v, u, rest, wkl, fzWkl)))
                            else
                              (status := MOVE;
                               (* print " Unsafe\n"; *)
                               doMoveWkl(rest, wkl, fzWkl))
                         | _ => 
                            (* coalescing pseudo and pseudo register *)
                            if conservative(u, v) then let
                                val (mvWkl, wkl, fzWkl) = 
                                      combine(v, u, rest, wkl, fzWkl)
                              in
                                coalesceIt();
                                (* print " Ok\n"; *)
                                doMoveWkl(mvWkl, addWkl(u, 2-vCnt, wkl), fzWkl)
                              end
                            else 
                              (status := MOVE;
                               (* print " Non-conservative\n"; *)
                               doMoveWkl(rest, wkl, fzWkl))
                        (*esac*))
                 (*esac*))
             end
           | doMoveWkl([], wkl, fzWkl) =
             (* Note. The wkl is not uniq, because decrementDegree may have
              * added the same node multiple times. We will let simplify take
              * care of this.
              *)
               {simplifyWkl = wkl, freezeWkl = fzWkl, 
                moveWkl = [], spillWkl = spillWkl, stack = stack}
      in
        doMoveWkl(moveWkl, simplifyWkl, freezeWkl)
      end (* coalesce *)
  in  coalesce
  end
  
    (* When a move is frozen in place, the operands of the move may
     * be simplified. One of the operands is node (below).
     *)
  fun wklFromFrozen(K, NODE{number=node, movelist, movecnt, ...}) = 
  let fun mkWkl(MV{status, src, dst, ...}) = 
      let val s = chase src and  d = chase dst
          val y = if nodeNumber s = node then d else s
      in  case !status
           of MOVE  => 
             (status := LOST;
              case y 
                of NODE{color=ref(COLORED _), ...} => NONE
                 | NODE{movecnt as ref 1, degree, ...} =>
                    (movecnt := 0;
                     if !degree < K then SOME y
                     else NONE)
                 | NODE{movecnt,...} =>
                     (movecnt := !movecnt - 1; NONE)
              (*esac*))
            | WORKLIST => error "wklFromFrozen"
            | _ => NONE
      end
  in  movecnt:=0;
      List.mapPartial mkWkl (!movelist)
  end
  
                       (*-----------freeze------------*)

  fun freezePhase(GRAPH{K,...}) = 
  let  
   
   
      (* freeze a move in place 
       * Important: A node in the freezeWkl starts out with a degree < K.
       * However, because of coalescing, it may have its degree increased 
       * to > K; BUT is guaranteed never to be a spill candidate. We do not
       * want to select such nodes for freezing. There has to be some other
       * freeze candidate that will liberate such nodes.
       *)
      fun freeze{freezeWkl, simplifyWkl, spillWkl, moveWkl, stack} = let
        fun find([], acc) = (NONE, acc)
           | find((n as NODE{color=ref PSEUDO, degree=ref d, ...})::ns, acc) =
             if d >= K then find(ns, n::acc) else (SOME n, acc@ns)
           | find(_::ns, acc) = find(ns, acc)
  
        fun mkWorkLists(NONE, fzWkl) = 
              {freezeWkl=fzWkl, simplifyWkl=simplifyWkl, 
               spillWkl=spillWkl, moveWkl=moveWkl, stack=stack}
           | mkWorkLists(SOME n, fzWkl) = 
              {freezeWkl=fzWkl, simplifyWkl=n::wklFromFrozen(K,n),
               spillWkl=spillWkl, moveWkl=moveWkl, stack=stack}
      in
        mkWorkLists(find(freezeWkl,[]))
      end
  in  freeze
  end

                    (*-----------select-------------*)
    (* spilling has occurred, and we retain coalesces upto to first
     * potential (chaitin) spill. Any move that was coalesced after 
     * the spillFlag was set, is undone.
     *)
  fun optimisticSpilling 
         (G as GRAPH{getreg,stamp,nodes,regmap,undoInfo,firstPseudoR,proh,...}) 
         ({stack, ...} : G.worklists) = 
  let fun undoCoalesced (NODE{number, color, ...}, status) =
        (status := MOVE;
         if number < firstPseudoR then () else color := PSEUDO)

      (* Briggs's optimistic spilling heuristic *)
      fun optimistic([], spills) = spills
        | optimistic((node as NODE{color, adj, ...}) ::ns, spills) = 
          let val st = !stamp 
              val _  = stamp := st + 1
              fun neighbors [] = ()
                | neighbors(r::rs) =
                  (case chase r 
                     of NODE{color=ref (COLORED col), ...} =>
                         if col = spillRegSentinel then neighbors rs
                         else (Array.update(proh,col,st);neighbors rs)
                      | _ => neighbors rs
                   (*esac*))
              val _ = neighbors(!adj)
              (* Hmmm.. should use biased coloring here ... allen *)
              val spills = 
                  let val col = getreg{pref=[],stamp=st,proh=proh}
                  in  color := COLORED col; spills 
                  end handle _ => node::spills
          in  optimistic(ns, spills)
          end
  in  case optimistic(stack, []) of 
         [] => []
       | spills =>
          (app (fn NODE{color, ...} => color := PSEUDO) stack;
           app undoCoalesced (!undoInfo);
           undoInfo := [];
           spills)
  end

  (*
   * Simplify/coalesce/freeze 
   *)
  fun simplifyCoalesceFreeze G =
  let val simplify = simplifyPhase G
      val coalesce = coalescePhase G
      val freeze   = freezePhase G
      fun iterate(wl as {simplifyWkl= _::_, ...}) = iterate(simplify wl)
        | iterate(wl as {moveWkl= _::_, ...}) = iterate(coalesce wl)
        | iterate(wl as {freezeWkl= _::_, ...}) = iterate(freeze wl)
        | iterate wl = wl
  in  iterate
  end

end

