(* bug241.sml *)
(* illegal sharing constraint detected but ignored *)

signature S = sig  type t  end;

functor F
    (structure A : S
     structure B : S
	sharing type B.t = A.s) = struct end;
