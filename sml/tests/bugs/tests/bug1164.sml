(* bug1164.sml *)

fun f x =   (* type metavar tv(x) has depth 1 *)
    let fun g(y: 'a) =  (* 'a is user bound tyvar scoped at g decl; depth 2 *)
	   if true then x else y  (* forces unification of 'a and tv(x)
				   * which should reset 'a depth to 1 *)
     in 0 
    end;
