(* bug921.2.sml *)

signature X = sig end;

functor Apply (P: sig functor F (A : X) : X end) (B : X) : X =
	P.F(B);

functor FUN (Q: sig functor G (C : X) : X end) (D : X) : X =
	Apply (struct functor F = G end) (D);
