(*
 * This module is responsible for locating loop structures (intervals).
 * All loops have only one single entry (via the header) but
 * potentially multiple exits, i.e. the header dominates all nodes
 * within the loop.   Other definitions are used for ``loops'' and ``headers''
 * in the literature.  We choose a structural definition that has nicer
 * properties.
 *
 * I haven't seen this algorithm described in the literature but I'm 
 * quite sure that it works in linear time, given that the dominator tree
 * has already been computed.
 * 
 * -- Allen
 *)

functor LoopStructure (structure GraphImpl : GRAPH_IMPLEMENTATION
                       structure Dom       : DOMINATOR_TREE)
    : LOOP_STRUCTURE =
struct
 
   structure G   = Graph
   structure GI  = GraphImpl
   structure Dom = Dom
   structure A   = Array

   datatype ('n,'e,'g) loop = 
      LOOP of { nesting    : int,
                header     : G.node_id,
                loop_nodes : G.node_id list,
                backedges  : 'e G.edge list,
                exits      : 'e G.edge list
              }

   datatype ('n,'e,'g) loop_info = 
       INFO of { dom : ('n,'e,'g) Dom.dominator_tree }

   type ('n,'e,'g) loop_structure = 
         (('n,'e,'g) loop, unit, ('n,'e,'g) loop_info) Graph.graph 

   fun dom(G.GRAPH{graph_info=INFO{dom,...},...}) = dom

   fun loop_structure DOM =
   let val info               = INFO{ dom = DOM }
       val G.GRAPH cfg        = Dom.cfg DOM
       val G.GRAPH dom        = DOM
       val N                  = #capacity dom ()
       val dominates          = Dom.dominates DOM
       val LS as G.GRAPH ls   = GI.graph ("Loop structure",info,N) 
       val ENTRY              = case #entries cfg () of
                                  [ENTRY] => ENTRY
                                | _ => raise Graph.NotSingleEntry
       val headers            = A.array(N,~1) (* header forest *)
       val visited            = A.array(N,~1) 

       fun find_loops (header,level) i =
       let val backedges = List.filter (fn (j,i,_) => dominates(i,j)) 
                                (#in_edges cfg i)
           val is_header = case backedges of [] => i = ENTRY
                                           | _  => true
       in  if is_header then (* i is now the new loop header *)
               (* find all nested loops first *)
           let val _ = app (find_loops (i,level+1)) (#succ dom i)
               (* locate all loop nodes *)
               fun find_loop_nodes([],nodes) = nodes
                 | find_loop_nodes((j,_,_)::es,nodes) = 
                   if i = j then find_loop_nodes(es,nodes)
                   else find_loop_nodes(es,do_node(j,nodes))
               and do_node(j,nodes) =  (* j is not the header i *)
                   let val v = A.sub(visited,j) 
                   in  if v = ~1 then (* j is a new loop node *)
                        (A.update(headers,j,i);
                         A.update(visited,j,i);
                         find_loop_nodes(#in_edges cfg j,j::nodes))
                       else chase_header(v,j,nodes)
                   end
               and chase_header(v,j,nodes) =
                   if v = i then nodes (* j has been visited before *)
                   else 
                      (* j is in a nested loop *)
                   let val _ = A.update(visited,j,i) (* mark j as visited *)
                       val h = A.sub(headers,j) 
                   in  if h = i then 
                          (* j is a header immediately nested under i *)
                          find_loop_nodes(#in_edges cfg j,nodes)
                       else (A.update(headers,j,i); (* path compression *)
                             chase_header(A.sub(visited,h),h,nodes))
                   end

               fun find_entry_loop_nodes() =
                  map #1 (List.filter (fn (i,n) => A.sub(headers,i) = ~1)
                            (#nodes cfg ()))

               fun find_exits(header,[],exits) = exits
                 | find_exits(header,i::is,exits) =
                   let fun f((e as (i,j,_))::es,exits) =
                         if A.sub(headers,j) = ~1 
                         then f(es,e::exits) else f(es,exits)
                         | f([], exits) = exits
                   in  find_exits(header,is,f(#out_edges cfg i,exits)) end
               val _     = A.update(headers,i,header)
               val _     = A.update(visited,i,i)
               val nodes = if i = ENTRY then
                              find_entry_loop_nodes()
                           else
                              find_loop_nodes(backedges,[])
               val exits = if i = ENTRY then [] 
                           else find_exits(i,i::nodes,[])
               val loop  = LOOP { nesting    = level,
                                  header     = i,
                                  backedges  = backedges,
                                  loop_nodes = nodes,
                                  exits      = exits
                                }
           in  #add_node ls (i,loop);
               if i = ENTRY then () else #add_edge ls (header,i,()) 
           end
           else app (find_loops (header,level)) (#succ dom i)
       end
   in  find_loops (ENTRY,0) ENTRY; 
       #set_entries ls [ENTRY];
       LS
   end

   fun nesting_level(G.GRAPH L) =
       let val INFO{dom=G.GRAPH dom,...} = #graph_info L
           val N                         = #capacity dom ()
           val levels                    = A.array(N,0)
           fun tabulate(_,LOOP{nesting,header,loop_nodes,...}) =
               (A.update(levels,header,nesting);
                app (fn i => A.update(levels,i,nesting)) loop_nodes)
       in  #forall_nodes L tabulate;
           levels
       end

   fun header(G.GRAPH L) = 
       let val INFO{dom=G.GRAPH dom,...} = #graph_info L
           val N                         = #capacity dom ()
           val headers                   = A.array(N,0)
           fun tabulate(_,LOOP{header,loop_nodes,...}) =
               (A.update(headers,header,header);
                app (fn i => A.update(headers,i,header)) loop_nodes)
       in  #forall_nodes L tabulate;
           headers
       end

   fun entryEdges(Loop as G.GRAPH L) =
       let val dom = dom Loop
           val G.GRAPH cfg = Dom.cfg dom
           val dominates = Dom.dominates dom
           fun entryEdges(header) = 
               if #has_node L header then 
                  List.filter(fn (i,j,_) => not(dominates(j,i)))
                      (#in_edges cfg header)
               else []
       in  entryEdges
       end

end    

