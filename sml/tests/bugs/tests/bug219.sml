(* bug219.sml *)

let val (x) as (y :: z) = [1,2] in x end;
