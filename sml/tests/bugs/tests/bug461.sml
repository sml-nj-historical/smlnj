(* bug461.sml *)
(* weak polymorphism *)

local
  val x = ref nil
in
  fun define(y: string list) = x := y
end;
