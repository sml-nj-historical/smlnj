(* bug12.sml *)

functor F(type t) =
struct
  datatype r = C of t
end;

structure S = F(type t = int);

S.C 3;
