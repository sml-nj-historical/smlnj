(* This tests instantiate for inconsistent arities in sharing types *)
signature S0 =
sig
  type 'a t
end

signature S1 =
sig
  type ('a,'b) t
end

signature S2 =
sig
  structure A : S0
  structure B : S1
  sharing A = B
end

functor F(X:S2) = struct end
