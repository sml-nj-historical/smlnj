(* bug49.sml *)

signature Sig = sig type t end;

structure S1 = struct type t = int end;

functor G (X:Sig) = struct datatype s = A of X.t end;

structure S2 = G(S1);

S2.A 4 = S2.A 4;
    
