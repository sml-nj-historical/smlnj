(* unionFind.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** Union Find 
 **
 ** Note: root is guaranteed to return the least element 
 **)
 
signature UNIONFIND = sig
    type trees
    val init    : int   -> trees
    val clear   : trees -> unit
    val sets    : trees -> (int * int list) list
    val addEdge : trees * int * int -> unit
    val find    : trees * int * int -> bool
    val root    : trees * int -> int
    val compact : trees -> unit
end

structure UnionFind : UNIONFIND = struct
  type trees = int Array.array
  val least = ~1
  fun init n = Array.array(n,least)

  fun clear tree = let
      fun f n = (Array.update(tree,n,least); f (n+1))
    in f 0 handle _ => ()
    end

  fun ascend (trees,u) = let val v = Array.sub(trees,u)
			 in 
			     if v > least then ascend(trees,v) else u
			 end

  val root = ascend

  fun addEdge(trees,x,y) = let
      val xroot = ascend(trees,x)
      val yroot = ascend(trees,y)
    in
	if xroot = yroot then ()
	else if xroot < yroot then
	     Array.update(trees,yroot,xroot)
	else Array.update(trees,xroot,yroot)
    end

  fun find(trees,x,y) = let 
      val xroot = ascend(trees,x)
      val yroot = ascend(trees,y)
    in
	xroot = yroot
    end

  fun sets trees = let
      val len = Array.length trees
      exception UnionFind
      val m :int list Intmap.intmap = Intmap.new(16,UnionFind)
      fun getSet n = (Intmap.map m n) handle _ => []
      fun f 0 = Intmap.intMapToList m
	| f n = if Array.sub(trees,n) = least then f (n-1)
		else let val root = ascend(trees,n)
		     in 
			 Intmap.add m (root,n::getSet root);
			 f (n-1)
		     end
    in f (Array.length trees - 1)
    end

  fun compact trees = let
      fun iter 0 = ()
	| iter n = let val m = ascend(trees,n)
	  in 
	      Array.update(trees,n,m); iter(n-1)
          end
    in
	iter(Array.length trees-1)
    end
end



(*
 * $Log$
 *)
