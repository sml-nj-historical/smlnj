(* bug1602.1.sml *)

signature SSS =
sig
  structure A : Absyn
  val foo : A.foo -> A.bar
end

structure sss : SSS =
struct
  structure A = Absyn
  fun foo x = x
end
