(* 240.sml *)

signature A = sig val x:int end;
functor f(X:A) = struct val x=X.x end;
funsig F(X:A) = A;
functor g:F=f;
structure a = g(val x=2);

