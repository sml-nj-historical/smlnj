(* bug278.sml *)

local
   structure Internal = struct val x=1 val y=2 end
in
   structure First  : sig val x : int end = Internal
   structure Second : sig val y : int end = Internal
end;
