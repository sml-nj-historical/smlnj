(* bug860.sml *)
(* 860. exception Subscript raised while compiling curried functors *)

signature NUM =
sig
  val n : int
end;

functor LeftSection (structure A : NUM
		     functor BinOp (structure A : NUM)
				   (structure B : NUM) : NUM)
		     (structure B : NUM) : NUM =
struct
  structure Result = BinOp (structure A = A)
			   (structure B = B)
  open Result
end;
