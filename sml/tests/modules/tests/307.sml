(* 307.sml *)

signature S =
sig
  eqtype A
  eqtype T
  val e : T
  val c : A * T -> T
  val d : T -> A * T
end;

functor E (functor F(type A):S where type A = A
	   functor G(type A):S where type A = A)
          (type B) =
struct
  local 
    structure X=F(type A=B)
    structure Y=G(type A=B)
  in
    fun f x = if x=X.e then Y.e else 
	      let val (a,y) = X.d x in Y.c (a,f y) end
  end
end;
