(* bug1155.2.sml *)

signature S1 =
sig
  type s
end;

signature S2 =
sig
  type s
  type t = s
end;

functor G(Y: S2) =
struct
  open Y
end;

functor H(type u) = 
struct
  structure B =
    struct
      type s = u
      type t = s
    end
  structure C : S2 = G(B)
end;
