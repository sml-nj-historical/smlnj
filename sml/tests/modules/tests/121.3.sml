structure A : sig type t type s type u end = 
struct
  type t = string
  type s = bool
  type u = int
end;

signature S =
sig
  structure D : sig type t end = A
end;

functor F(X:S) = struct end;

structure B =
struct
  structure D = 
  struct
    type t = string
    type s = unit
    type v = string
  end
end;

structure C = F(B);
