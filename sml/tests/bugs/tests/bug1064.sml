(* bug1064.sml *)

signature ORD_KEY =
sig
  type ord_key
  val compare : ord_key * ord_key -> order
end;

signature ORD_LIST =
sig
  structure Key : ORD_KEY
  type 'a ord_list                               
  val find : ('a ord_list * Key.ord_key) -> 'a
end;
