(* bug907.sml *)

let exception A of 'a in (A: 'a -> exn) end;
