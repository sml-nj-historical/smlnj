(* bug233.sml *)
(* opening locally declared structure causes Runbind *)

local
  structure Bug =
  struct
    val message = "Try to evaluate me !"
  end
in
  open Bug
end;
