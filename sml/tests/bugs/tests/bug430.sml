(* bug430.sml *)

signature S2 =
sig
  structure A : sig type t end
  datatype u = ITEM of A.t
end;

functor F(X : sig type v end ) =
struct
  type w = X.v
end;

functor G(Y : S2) =
struct
  structure B = F(struct type v = Y.u end)
end;
