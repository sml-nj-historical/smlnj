(* test49.sml *)
(* keywords: functor, datatype, printing *)

(* Problem: impossible error, lookPath, when printing top-level result *)

functor F(X: sig datatype d = A | B of d end)
  : sig datatype p = P of X.d
        val bug : p
    end =
  struct
    datatype p = P of X.d
    val bug = P(X.B(X.A))
  end;
