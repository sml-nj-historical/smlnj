(* bug1048.sml *)

fun readAll () =
    (case TextIO.input1(TextIO.stdIn)
       of SOME c => c :: readAll()
        | NONE => nil);
