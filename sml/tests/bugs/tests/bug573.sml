(* bug573.sml *)
(* detection of spurious cycles in presense of type abbrevs *)

type 'a ID = 'a;

fun f (x:'a) = (x:'a ID);

fun g x = g (f x);

(* This program should type check without any circularity errors *)
