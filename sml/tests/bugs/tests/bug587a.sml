(* functor signatures built from a previously existing signature *
 * (83 bug fixed in 85 (O. Nora nb 587))                         *)

signature A = sig val x:int end

funsig F(X:A) = A

functor f(functor g:F structure s:A) = struct end;

