(* bug71.sml
71. Failure to restore enviroment after exception in "use"
uses auxiliary file bug71a.sml
*)

val x = (use "bug71/y.sml"; let exception X in raise X end);
x;

