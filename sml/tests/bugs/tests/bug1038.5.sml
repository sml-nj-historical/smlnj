(* bug1038.5.sml *)

signature ORD_KEY =
sig
  type ord_key
  val compare : ord_key * ord_key -> order
end; (* ORD_KEY *)

signature ORD_SET =
sig
  structure Key : ORD_KEY
  type item = Key.ord_key
end;

functor LatticeFn (structure NodeSet : ORD_SET
		   structure PartialOrder :
		     sig
		       type element
		       sharing type element = NodeSet.item
		       val lessThanEq : element * element -> bool 
		     end) =
struct
end;
