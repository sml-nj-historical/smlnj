(* bug521.sml *)
(* incorrect type checking of flex records *)
(* result type of foo should not be free *)

fun foo x = let val a = #1 x
                val (a,b) = x
            in
               b ()
            end;

