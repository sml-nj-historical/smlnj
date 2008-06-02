(* 
 * This file tests translation of datatype bindings with opaque ascription.
 * It also appears to set off the negative index error for TC_VARs
 * Again, the Tycpath for type s should be a TVar. 
 *)

functor P(structure A : sig type s end) = struct end
structure B = P(structure A :> sig type s end = struct datatype s = S end)
