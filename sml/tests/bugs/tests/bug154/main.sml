(* ------------------------  main.sml: ---------------------- *)
System.Control.Print.signatures := false;
import "checker";
(* structure Globals:GLOBALS = GlobalsFun(); *)
structure TConst:TCONST = TConstFun((*structure Globals=Globals*));
TConst.from_string "int";
