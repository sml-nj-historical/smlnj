(* 214.2.sml *)
(* impossible error, lookPath, when printing top-level result *)

functor F(X: sig datatype d = A | B of d end) =
  struct
    datatype p = P of X.d
    val bug = P(X.B(X.A))
  end;

structure Bug = F(struct datatype d = A | B of d end);

Bug.bug;
