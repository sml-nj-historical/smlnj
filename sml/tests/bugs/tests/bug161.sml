(* bug161.sml *)
(* nested functor calls *)

signature SIG =
    sig type t
    end;

functor F(X : SIG) : SIG
    = struct type t = X.t
      end;

(* Replacing output signature by its definition: no problem *)
functor F'(X : SIG) : sig type t end
    = struct type t = X.t
      end;

functor G(X : SIG) : SIG
    = struct type t = X.t
      end;

functor H(X : SIG) : SIG = G(F(X));

(* Replacing output signature by its definition: fails with exception Bind *)
functor H'(X : SIG) : sig type t end = G(F(X));
signature SIG =
    sig type t
    end;

functor F(X : SIG) : SIG
    = struct type t = X.t
      end;

(* Replacing output signature by its definition: no problem *)
functor F'(X : SIG) : sig type t end
    = struct type t = X.t
      end;

functor G(X : SIG) : SIG
    = struct type t = X.t
      end;

functor H(X : SIG) : SIG = G(F(X));

(* Replacing output signature by its definition: fails with exception Bind *)
functor H'(X : SIG) : sig type t end = G(F(X));
