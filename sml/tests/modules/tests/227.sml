(* 227.sml *)

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
end;

funsig FS1(A:S1) = 
sig
  functor G(X:sig end):sig structure B:S1 = A end
end;


funsig FS2(A:S1) = 
sig
  functor G(X:sig end):sig structure B:S1 = A end
end;

functor H:FS1=F;
functor K:FS2=F;

structure a:S1 = struct type t=int val x=5 end;

structure b=H(a);
structure c=b.G(struct end);
a.x = c.B.x;

(* sharing added by the realization not implied by the functor signature *)
structure d=K(a);
structure e=d.G(struct end);
a.x = e.B.x;
