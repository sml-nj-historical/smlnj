(* bug460.sml *)

signature A = sig val s : (unit -> 'a) -> unit end;
structure A : A = struct fun s f = f() end;
