(* bug1171.sml *)

functor WRef(type u):>sig
   type t
   type href
   val new:t->href
end where type t=u =
struct
   type t=u
   type href=u ref
   fun new x=ref x
end

structure WR=WRef(type u=int)
val h=WR.new 0
