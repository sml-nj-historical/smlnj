(* bug256.2.sml *)
(* Compiler bug: r_o in mcopt *)

datatype 'a Xlist
  = Xnil
  | Xcons of 'a * 'a Xlist
  | Xappend of 'a Xlist * 'a Xlist;

fun incr Xappend(Xnil,Xnil) = Xnil
  | incr Xappend(Xnil,Xcons(a,b)) = Xcons(a,b);
