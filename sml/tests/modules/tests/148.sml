functor F (X : sig type t end) =
struct
  type s = X.t list
end;

functor G (Y: sig type u end) =
struct
  structure A = F (struct type t = Y.u list end)
end;

structure B = struct type u = int end;
structure S = G (B);

