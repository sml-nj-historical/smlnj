(* 236.sml *)

functor f() =
struct
  structure a=struct
  datatype t=c
  val w=c

  functor g(type t' = t val x:t') = struct
    val b1 = x=w;
  end;
  end
end;

structure a=f();
functor g=a.a.g;
structure b=g(type t'=a.a.t val x=a.a.w);
