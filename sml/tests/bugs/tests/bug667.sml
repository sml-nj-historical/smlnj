(* bug667.sml *)
(* 667. Compiler bug: getvars(STRdec)/fn opening a structure *)

functor A(type aa) = struct type a = aa list val a =[] end;

structure A = A(type aa = int);
 
open A;
