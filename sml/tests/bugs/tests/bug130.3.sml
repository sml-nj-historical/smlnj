(* bug 130 (b) *)

functor G() =
struct
  type zz = int * int
  type aa = zz * int
  fun f(x:aa) = let val (u:zz,_) = x in u end
end
