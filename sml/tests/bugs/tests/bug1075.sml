(* bug1075.sml *)

Compiler.Profile.setProfMode true;
fun f x = 3;
Compiler.Profile.reset();
f 3;
Compiler.Profile.report(TextIO.stdOut);

