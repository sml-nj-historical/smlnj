(* test/cmdline.sml, PS 1997-03-07 *)

val _ = let
    val cmdName = 
      String.tokens (fn #"/" => true | _ => false) (CommandLine.name())
 in
    app print ["This program is invoked as `", List.last cmdName, "'\n",
		"with arguments:\n"];
     app (fn a => (print a; print "\n")) (CommandLine.arguments ())
 end

