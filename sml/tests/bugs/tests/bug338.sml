(* bug 338 -- shouldn't type check last line *)
fun new() = 
    let datatype A = A in
	(A,fn A => ())
    end;
val (a1,f1) = new ();
val (a2,f2) = new ();
val _ = f1 a2;
    
