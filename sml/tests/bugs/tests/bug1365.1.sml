(* bug1365.sml *)

signature S =
sig
  type t
  datatype foo =
      A of t | B 
  val a : unit -> foo
  val f : foo -> string
end;
    
functor F(structure T:S) =
struct
  fun f (T.A _) = "A"
    | f (T.B) = "B"
end;

structure S1 : S =
struct
  type t = unit
  datatype foo = A of t
	       | B

  fun a () = A ()
  fun f (A _) = "A"
    | f (B) = "B"
end;

structure S2 : S =
struct
  type t = int
  datatype foo = A of t | B

  fun a () = A 1
  fun f (A _) = "A"
    | f (B) = "B"
end;

structure Test =
struct
  structure T1 = F(structure T = S1)
  structure T2 = F(structure T = S2)

  val x = [(S1.f (S1.a()),T1.f (S1.a ())),
	   (S2.f (S2.a()),T2.f (S2.a ()))]
end;

val y = Test.x;
