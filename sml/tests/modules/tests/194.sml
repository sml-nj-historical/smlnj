(* where type applied to a datatype spec 
 * def and spec not compatible, so resulting signature is not matchable *)

structure A =
struct
  datatype t = K1 | K2 of t
end;

signature S =
sig
  structure B :
    sig
      datatype t = K
    end
end;

signature S1 = S where type B.t = A.t;
