(* bug845.sml *)
(* 845. bus error from locals in signatures *)

signature S =
sig
  local
      structure A: sig type t end
   in         
      val make: int -> A.t
      val destr: A.t -> int
      val add: A.t -> A.t -> A.t
  end
end;

structure M1: S =
struct
  local structure A = struct type t=int end 
   in fun make(x:int) = x
      fun destr x = x
      fun add (x:int)  y = x+y
  end
end;

open M1;     

val x1 = make 3;

add x1 x1;

