(* bug620.sml *)
(* 0.87: Compiler bug: abstractfct:tyconSeries1: param tycon not found *)

signature SIG =
sig
  datatype d = D of unit   (* the "of unit" is necessary for bug *)
end

functor Twice(functor F(A:SIG):SIG) =
struct
  functor TwiceF(A: SIG) = F(F(A))
end
