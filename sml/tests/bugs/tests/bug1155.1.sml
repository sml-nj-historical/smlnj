(* bug1155.1.sml *)

signature S1 =
sig
  type s
end;

signature S2 =
sig
  structure A : S1
  type t = A.s
end;

functor G(Y: S2) =
struct
  open Y
end;

functor H(Z: S1) = 
struct
  structure B =
    struct
      structure A = Z
      type t = A.s	
    end
  structure C : S2 = G(B)
end;
