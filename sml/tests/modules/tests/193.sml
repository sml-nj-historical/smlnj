(* where type applied to a datatype spec 
 * def and spec compatible, so resulting signature is matchable *)

structure A =
struct
  datatype t = K1 | K2 of t
end;

signature S =
sig
  structure B :
    sig
      datatype t = K1 | K2 of t
    end
end;

signature S1 = S where type B.t = A.t;
