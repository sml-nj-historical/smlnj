(* bug1096.sml *)

abstype 'a q = Q of 'a list
with
  val empty = Q []
end;
