(* bug 378 *)
signature AA =
    sig
	datatype s  =  a of t * s
	and t = b of s
end  (* signature AA *)

signature BB =
    sig
	structure A : AA
end  (* signature BB *)


signature CC =
    sig
	structure B : BB
	type u
end  (* signature CC *)

functor F (structure B : BB) : CC =
    struct
	structure B = B
	structure A = B.A
	open A
	type u = t * s
    end (* functor F *)

structure C : CC = F (structure B = B);
