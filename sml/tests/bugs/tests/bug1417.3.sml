(* bug1417.3.sml *)

signature S =
sig
  type t
  datatype u = C1
  and v = C2 of t
end;

functor F(type t) :> S where type t = t =
struct
  type t = t
  datatype u = C1
  and v = C2 of t
end;

structure M1 = F(type t = int);

structure M2 =
struct
  datatype v = datatype M1.v
end;
