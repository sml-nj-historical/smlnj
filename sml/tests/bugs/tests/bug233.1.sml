(* bug233.1.sml *)
(* opening locally declared structure causes Runbind *)

local
  structure Bug =
  struct
    val message = "Try to evaluate me !"
    val x = 3
  end
in
  open Bug
end;
