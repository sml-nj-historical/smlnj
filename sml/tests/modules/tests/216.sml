signature S =
sig
  structure A : sig type t end
  val x : A.t
end;

functor F(type r): sig val f : r -> r end = 
struct
  fun f z = z
end;

functor G(X : S) : 
  sig
    type s
    val y : s
  end = 
struct
  type s = X.A.t
  structure B = F(type r = s)
  val y = B.f X.x  (* B.f necessary *)
end;
