(* 226.sml *)

signature S1 =
sig
  eqtype t
  val x : t
end;

functor F(A:S1) = 
struct
  functor G(X:sig end) =
  struct
    structure B=A
  end
end

structure a:S1 = struct type t=int val x=5 end;
structure b=F(a);
structure c=b.G(struct end);

a.x = c.B.x;
