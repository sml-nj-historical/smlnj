(* bug1388.1.sml *)

val div31 = 
    (Int31.div(Option.valOf Int31.minInt, ~1); "WRONG (no exn)")
    handle Overflow => "OK"
         | _ => "WRONG (exn)";

val mod31 =
    (if Int31.mod(Option.valOf Int31.minInt, ~1) = 0 then "OK" else "WRONG (NOT 0)")
    handle _ => "WRONG (exn)";

val quot31 =
    (Int31.quot(Option.valOf Int31.minInt, ~1); "WRONG (no exn)")
    handle Overflow => "OK"
         | _ => "WRONG (exn)";

val rem31 =
    (if Int31.rem(Option.valOf Int31.minInt, ~1) = 0 then "OK" else "WRONG (NOT 0)")
    handle _ => "WRONG (exn)";

val div32 = 
    (Int32.div(Option.valOf Int32.minInt, ~1); "WRONG (no exn)")
    handle Overflow => "OK"
         | Div => "WRONG (DIV)"
         | _ => "WRONG (exn)";

val mod32 =
    (if Int32.mod(Option.valOf Int32.minInt, ~1) = 0 then "OK" else "WRONG (NOT 0)")
    handle _ => "WRONG";

val quot32 =
    (Int32.quot(Option.valOf Int32.minInt, ~1); "WRONG (no exn)")
    handle Overflow => "OK"
         | Div => "WRONG (DIV)"
         | _ => "WRONG (exn)";

val rem32 =
    (if Int32.rem(Option.valOf Int32.minInt, ~1) = 0 then "OK" else "WRONG (NOT 0)")
    handle Div => "WRONG (DIV)"
         | _ => "WRONG (exn)";
