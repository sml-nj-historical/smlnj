(* bug1472.1.sml *)

datatype expression = 
   RealConst of real |
  Cast of ctype * expression

and ctype =
  Struct of declarator

and declarator = 
  Array_d of expression;

val f = (fn (x:declarator, y) => (x = y));
