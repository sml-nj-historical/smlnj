(* bug216.sml *)

fun rlist(from:real,to:real,step:real) f =
    if (from<to) then (f from)::rlist(from+step,to,step) f else [];

rlist(0.0,10.0,1.0) (fn x => x);

rlist(0.0,10.0,1.0) (fn x => x);

rlist(0.0,10.0,1.0) (fn x => x);

rlist(0.0,10.0,1.0) (print o Real.toString);

rlist(0.0,10.0,1.0) (fn x => x);
