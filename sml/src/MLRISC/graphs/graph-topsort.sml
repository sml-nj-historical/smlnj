structure GraphTopsort : GRAPH_TOPOLOGICAL_SORT = 
struct

   structure G = Graph

   (*
    * Topological sort
    *)
   fun topsort (G.GRAPH G) roots = 
   let val visited = BitSet.create (#capacity G ())
       val succ    = #succ G
       fun dfs (n, list) =
          if BitSet.markAndTest(visited,n) then list
          else dfs'(n,succ n,list)
       and dfs'(x,[],list)    = x::list
         | dfs'(x,n::ns,list) = dfs'(x,ns,dfs(n,list))
       and dfs''([], list)    = list
         | dfs''(n::ns, list) = dfs''(ns,dfs(n,list))
   in
       dfs''(roots,[])
   end

end

(* 
 * $Log: graph-topsort.sml,v $
 * Revision 1.1.1.1  1998/11/16 21:48:35  george
 *   Version 110.10
 *
 *)
