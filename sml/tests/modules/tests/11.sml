(* test11.sml *)
(* keywords: functor *)

(* derived from mlyacc.sml (old version) *)

signature GRAPH =
sig
  type e
end;

signature GRAM =
sig
  type s
end;

signature LRG =
sig
  structure L : GRAPH
end;

functor Graph (X : sig type e end) =
struct
  open X
end;

functor LrGraph(G: GRAM) : LRG =
struct
  structure L = Graph (struct type e = G.s end)
end;

structure Graph = LrGraph (struct type s = int end);
