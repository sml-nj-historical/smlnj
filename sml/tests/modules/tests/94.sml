(* This tests instantiate to see if it detects a structure which
 * is equivlanced with one of its substructures
 *)
signature C = 
 sig
   structure A : sig end
 end

signature B =
 sig
    structure C : C
 end

signature A =
 sig
    structure C : C
 end

signature S =
  sig
    structure A : A
    structure B : B
    sharing A = B
    sharing B.C.A = A
 end

functor F(X:S) = struct end
