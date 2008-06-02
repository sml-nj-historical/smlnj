(*
 * This example tests whether the coalescing to canonical representatives
 * due to sharing has been propagated into PLambda types representing 
 * datatypes. 
 * 
 * In particular, the fact that M.I.t and M.I.R.t have been coalesced 
 * should be reflected in M.I.R.u.
 *
 * This example also tests whether deBruijn indexing works for curried
 * functors in the presence of sharing. 
 *)

signature S1 =
sig
   type t                      
   
   datatype u = K of t
end

signature S2 =
sig
   structure I : S1
   val x : unit -> I.u
end

signature S0 =
sig
   structure I     : S1
   structure R : S2
     sharing R.I = I
end

functor F () (M : S0) =
struct
   structure Q = M
end

functor G = F()