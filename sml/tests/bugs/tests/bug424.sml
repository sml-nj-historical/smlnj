(* bug424.sml
 * bug342.sml also tests this.
 *)

(* discard the date to avoid time dependent printing *)
let val proc = Unix.execute ("/bin/date",[]);
    val (istr,ostr) = Unix.streamsOf proc;
 in TextIO.inputLine istr; (* get the date *)
    TextIO.closeIn istr;
    print "ok\n"
end;
