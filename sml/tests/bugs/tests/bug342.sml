(* bug 342 -- should return display name
 * Note: this also tests for #413, #424 and #432.
 *)

let val proc1 = Unix.execute ("/bin/sh", []); (* to avoid print PID *)
    val (istr,ostr) = Unix.streamsOf proc1;
 in TextIO.output(ostr,"echo '>'$HOME'<'\n");
    TextIO.closeOut ostr;
    TextIO.inputLine istr;
    TextIO.closeIn istr
end;

let val proc2 = Unix.executeInEnv ("/bin/sh", [], ["DISPLAY=foo"]);
    val (istr,ostr) = Unix.streamsOf proc2;
 in TextIO.output(ostr, "echo '>'$DISPLAY'<'\n");
    TextIO.closeOut ostr;
    TextIO.inputLine istr
end;
