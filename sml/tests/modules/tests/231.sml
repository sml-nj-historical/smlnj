(* 231.sml *)

signature S1 =
sig
  eqtype t
  val x : t
end;

functor F(A:S1) = 
struct
  functor G(X:sig end) =
  struct
    structure B:S1=struct datatype t=c1 val x=c1 end;
  end
end;

funsig FS1(A:S1) = 
sig
  functor G(X:sig end):sig structure B:S1 end
end;

functor H:FS1=F;
structure a:S1 = struct datatype t=c1 val x=c1 end;
structure b=H(a);
structure c=b.G(struct end);

a.x = c.B.x;
