(* bug1155.3.sml *)

signature S =
sig
  type s
  type t = s
end;

functor F(Y: S) =
struct
  open Y
end;

structure A = 
struct
  structure B =
    struct
      type s = unit
      type t = s
    end
  structure C : S = F(B)
end;
