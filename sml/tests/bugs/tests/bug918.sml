(* bug918.sml *)
(* 918. incomplete sharing propagation after functor application *)

signature SIG = sig end;

functor ID (X: SIG) = X;

functor H() =
struct
  structure A = struct end
  structure B = ID(A)
end;

structure S = H();

functor F(structure X: SIG
	  structure Y: SIG sharing X = Y) = 
struct end;

structure foo = F(structure X = S.A structure Y = S.B);
