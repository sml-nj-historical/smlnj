(* test16.sml *)
(* keywords: functor, datatype *)

(* derived from new mlyacc *)

signature S1 =
sig
  type s
  val x : s
end;

signature S2 =
sig
  structure K2 : S1
end;

functor F(X:sig end) = 
struct
  structure K2 =
  struct
    datatype s = VOID
    val x = VOID
  end
  structure K1 : S2 = 
  struct
    structure K2 = K2
  end
end;

structure Foo: S2 = F (struct end);
