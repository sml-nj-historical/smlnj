(* feedback.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Feedback : 
    sig val scc : (int * int list) list -> (int * int list) list list 
	  (* Strongly-connected components of a graph *)
	val feedback : (int * int list) list -> int list 
	  (* Minimum feedback vertex set of a graph *)
    end =
(* Input: A directed graph; that is, a list of vertex-numbers, 
            each node with a list of out-edges which indicate other vertices.
   Output:  A minimum feedback vertex set.

   Method: branch and bound

   NOTE:  By setting MAXDEPTH=infinity, this algorithm will produce
         the exact minimum feedback vertex set.  With MAXDEPTH<infinity,
	 the result will still be a feedback vertex set, but not
         always the minimum set.  However, on almost all real programs,
	 MAXDEPTH=3 will give perfect and efficiently computed results.
	 Increasing MAXDEPTH will not make the algorithm take longer or
	 produce better results on "real" programs.

*)

struct

(* I want to get rid of SortedList, so I fake the required routines here.
 * Eventually, this module should be cleaned up!
 *   - Matthias 11/2000 *)

val sluniq = SortedList.uniq
val slmember = SortedList.member
val sldiff = SortedList.difference   

type node = int * int list
type graph = node list

val infinity = 1000000000

fun minl l =
 let fun f(i,nil) = i | f(i,j::rest) = if i<j then f(i,rest) else f(j,rest)
  in f(infinity,l)
 end

fun all (a::rest) = a andalso all rest | all nil = true

fun forall nil f = () | forall (a::r) f = (f a; forall r f)

fun filter f nil = nil | filter f (x::rest) = if f x then x::filter f rest
                                                     else filter f rest

val normalize : graph -> graph = map (fn(n,e)=>(n,sluniq e))

fun scc nil = nil   (* quickie special case; the general case still works
		       but is slower *)
  | scc nodes =
let exception Unseen
    type info = {dfsnum: int ref, 
		 sccnum: int ref, 
		 edges: int list}
    val m : info IntHashTable.hash_table = IntHashTable.mkTable(32,Unseen)
    val lookup = IntHashTable.lookup m

    val compnums = ref 0 and id = ref 0
    val comps = ref (nil: (int * int list) list list)

    val stack : (int * info) list ref = ref nil

    fun scc' nodenum =
	(* Find strongly-connected components of a graph;
	   return a list of components; each component is a graph
	   with no edges pointing out of the component *)
        let val info as {dfsnum as ref d, sccnum, edges} = lookup nodenum
	    (* prune: gets rid of edges out of the component *)
	    fun prune c = filter (fn i => !(#sccnum(lookup i)) = c)

            fun gather(c,bag,(n' as (n,{sccnum,dfsnum,edges}))::rest) = 
			(sccnum := c; dfsnum := infinity;
			 
(*                         print n; print "  "; print c; print "\n"; *)
                         if n=nodenum then (map (fn (n,{edges,...})=>
					         (n, prune c edges))
					     (n'::bag),
					 rest)
			           else gather(c,n'::bag,rest))
	    val v = !id 
         in if d >= 0 then d 
	    else (id := v+1; 
		  stack := (nodenum, info) :: !stack;
                  dfsnum := v;
		  let val b = minl(map scc' edges)
		   in if v <= b
                      then let val c = !compnums
			       val _ = compnums := c+1
			       val (newcomp,s) = gather(c,nil,!stack)
			    in stack := s;
			       comps := newcomp :: !comps;
			       v
			   end
		      else b
		  end)
       end

 in (*print "\nInput: "; forall nodes (fn (i,_) => (print i; print " "));*)
    forall nodes
     (fn (f,edges) => IntHashTable.insert m 
              (f,{dfsnum=ref ~1, sccnum=ref ~1, edges=edges}));
    forall nodes (fn (vertex,edges) => scc' vertex);
(*    print "\nOutput:";
    forall (!comps) (fn l =>
        (forall l (fn (i:int,_) => (print i; print " "));
	 print "; "));
    print "\n";
*)    !comps
end

(* A "trivial" component is just a single node with no self loop *)
fun trivial [(_,[])] = true
  | trivial _ = false

(*
     val printlist = app( fn i:int => (print i; print " "))
     (print "f "; print lim; print " "; printlist (map #1 left); 
      print "("; print x; print ") "; printlist (map #1 right); print "\n";
    print "try "; print limit; print " "; printlist (map #1 nodes); print "\n";
*)



fun feedb(limit,graph:graph, 0) =
      if limit >= length graph
       then (print "Approximating!";
	     app (fn (n,_)=>(print " "; print (Int.toString n))) graph; print "\n";
	     SOME(map #1 graph))
       else ( (*print "Pessimistic!\n";*) NONE)


  | feedb(limit, graph, depth) = 
    (* return a minimum feedback vertex set for graph, 
       of size no bigger then limit; else return NONE *)
     if depth<=0 andalso limit >= length graph
       then ((* print "Approximating!";
	        app (fn (n,_)=>(print " "; print n)) graph; print "\n";*)
	     SOME(map #1 graph))
   else
( (*print (substring(".............................",0,!MAXDEPTH+1-depth));
  print (length graph); print " "; print limit;
 print "\n"; *)
	 
    let val comps = filter (not o trivial) (scc graph)
        fun g(lim, set, c::comps) = 
	      if lim>0 
	       then (case try(lim,c,depth)
		      of NONE => NONE
                       | SOME vl => g(lim-(length vl - 1), vl@set, comps))
	       else NONE
          | g(lim, set, nil) = SOME set
     in g(limit - length comps + 1, nil, comps)
    end
)
     
and try(limit, nodes: graph,depth) =
    (* "nodes" is a strongly-connected component; remove each node in turn 
       and find the minimum feedback vertex set of the result.
       The resulting set must be no bigger than limit, or don't bother. *)
     let fun f(best,lim,left,nil) = best
           | f(best,lim,left as _::_, (node as (_,[_]))::right) = 
	           (* A node with only one out-edge can't be part of
		    a unique minimum feedback vertex set, unless they
		    all have one out-edge. *)
	          f(best,lim,node::left,right)
           | f(best,lim,left,(node as (x,_))::right) = 
	          let fun prune (n,el) = (n, filter (fn e=>e<>x) el)
                      val reduced = map prune (left@right)
		   in case feedb(lim-1, reduced,depth-1)
		       of SOME vl => f(SOME(x::vl), length vl, 
				       node::left, right)
			| NONE => f(best,lim,node::left,right)
                  end
      in f(NONE, Int.min(limit,length nodes), nil, nodes)
     end

val scc = scc o normalize

val MAXDEPTH= 3  (* let's approximate! *)

fun feedback1 graph = case feedb(length graph, graph,MAXDEPTH) 
                       of SOME set => set
 
fun pruneMany (out,g) =
   let val out' = sluniq out
       fun pruneNode(n,e) = (n,sldiff(e,out'))
    in map pruneNode g
   end

fun selfloops ((n,e)::rest) = 
    let val (selfn,nonselfn) = selfloops rest
     in if slmember e n
         then (n::selfn,nonselfn)
         else (selfn,(n,e)::nonselfn)
    end   
  | selfloops nil = (nil,nil)

(* any node with an edge to itself MUST be in the minimum feedback
   vertex set; remove these "selfnodes" first to make the problem easier.
   Relies on the fact that out-edges are sorted (result of "normalize").
*)
val feedback2 = fn graph =>
    let val (selfnodes,rest) = selfloops graph
     in selfnodes @ feedback1 (pruneMany(selfnodes,rest))
    end

val feedback = feedback2 o normalize

end

