(* ------------------------  globals.sml: ---------------------- *)
signature GLOBALS =
sig
  val member: ''a -> ''a list -> bool
end

functor GlobalsFun() : GLOBALS =
struct
  fun member x [] = false
    | member x (y::l) = (x=y) orelse (member x l)
end

