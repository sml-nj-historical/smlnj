(* test73.sml *)

(* type abbreviation expansion in signature matching, on spec side *)

signature S =
sig
  type ('a,'b) t
  val x : 'a -> 'b -> ('a,'b) t
end;

structure A: S =
struct
  datatype 'a d = D of 'a
  type ('a,'b) t = 'a d
  val x = (fn y => fn z => D y)
end;
