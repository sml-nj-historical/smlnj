(* bug796.sml *)

signature S = sig type t end;

functor F (A:S) (B:S) = struct type t = int end;

functor G () =
struct
  functor H = F (struct type t = int end)
end;
