(* 229.sml *)

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
  functor G(X:sig end):sig structure B:S1 end
end;

functor K1(functor H:FS1) = struct
  structure a:S1 = struct type t=int val x=5 end;
  structure b=H(a);
  structure c=b.G(struct end);
end;

(* implicit sharing *)
structure d=K1(functor H=F);

d.a.x = d.c.B.x;

(* mod7 without the sharing specification: it fails *)

functor K2(functor H:FS1) = struct
  structure a:S1 = struct type t=int val x=5 end;
  structure b=H(a);
  structure c=b.G(struct end);
  val b = a.x = c.B.x;
end;

