(* test13.sml *)
(* keywords: functor *)

(* from sannella -- tests nested functor applications *)

signature SIG =
sig
  type t
end;

(* ok *)
functor F(X : SIG) : SIG =
struct
  type t = X.t
end;

(* ok *)
functor F'(X : SIG) : sig type t end =
struct
  type t = X.t
end;

(* ok *)
functor G(X : SIG) : SIG =
struct
  type t = X.t
end;

functor H(X : SIG) : SIG = G(F(X));

(* Replacing output signature by its definition: fails with exception Bind *)
functor H'(X : SIG) : sig type t end = G(F(X));
