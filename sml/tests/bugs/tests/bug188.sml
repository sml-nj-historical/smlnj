(* bug188.sml *)
(* infinite loop parsing simple functor declaration *)

signature TRIVSIG = sig end;
functor A(X : TRIVSIG) : TRIVSIG = X;
functor B(X : TRIVSIG) : TRIVSIG = A(X);
