(* sparseSet.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* sparse-set.sml
 *
 * Sparse sets of integers; based on "An efficient representation for sparse sets,"
 * by Briggs and Torczon, LOPLAS v. 2, #1-4, 1993.
 *)

signature SPARSESET = sig
  type set
  val set    : int -> set
  val clear  : set -> unit
  val insert : set -> int -> unit
  val delete : set -> int -> unit
  val member : set * int -> bool
  val forall : set -> (int -> unit) -> unit
end

structure SparseSet :> SPARSESET = struct

  datatype set = SET of {
      max : int,		
      members : int ref,
      sparse : int Array.array,
      dense : int Array.array
    }

  exception SparseSet

  fun set max = SET{
	  max = max,
	  members = ref 0,
	  sparse = Array.array(max, 0),
	  dense = Array.array(max, 0)
	}

  fun error msg = (print ("SparseSet: "^ msg); raise SparseSet)

  val subscript = Array.sub
  val update = Array.update
  val unsafeSub = Unsafe.Array.sub
  val unsafeUpdate = Unsafe.Array.update

  fun clear(SET{members, ...}) = (members := 0)

  fun member(SET{members = ref n, sparse, dense, ...}, x) = let
      val i = Array.sub(sparse, x)
    in
      (i < n) andalso (unsafeSub(dense, i) = x)
    end

  fun insert(SET{members, sparse, dense, ...}) x = let
      val n = !members
      val i = Array.sub(sparse, x)
    in
      if ((n <= i) orelse (unsafeSub(dense, i) <> x))
	then (
	  unsafeUpdate(sparse, x, n);
	  Array.update(dense, n, x);
	  members := n+1)
	else ()
    end

  fun delete(SET{members,sparse,dense,...}) x = let

      val n = !members - 1
      val i = Array.sub(sparse,x)
    in
      if (i <= n) andalso Array.sub(dense,i) = x then let
	  val e = Array.sub(dense,n)
	in 
	  members:=n;
	  unsafeUpdate(dense,i,e);
	  unsafeUpdate(sparse,e,i)
	end
      else ()
    end

  fun forall(SET{members,dense,...}) f = let
      val n = !members
      fun doit i = (f (Array.sub(dense,i)); doit(i-1))
    in
      doit(n-1) handle _ => ()
    end
end



(*
 * $Log: sparseSet.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
