(* bug239.sml *)
(* exception INCONSISTENT raised *)

signature A =
  sig
    type A
    datatype Foo = FOO of A
  end;

signature B = sig  type B  end;

signature C = sig  datatype C = C of int -> int  end;

functor XYZZY(structure A: A
	      structure B: B sharing type A.A = B.B
	      structure C: C sharing type C.C = B.B
	     ) =
  struct
  end;
