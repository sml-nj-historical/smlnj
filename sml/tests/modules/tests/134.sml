signature S =
sig
  type t = int
  type u = t list
end;

functor F(): S =
struct
  type t = int
  type u = t list
end;
