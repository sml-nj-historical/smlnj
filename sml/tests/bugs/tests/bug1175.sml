(* bug1175.sml *)

(*----------------------------------------------------------------------*)
(* A very general datatype for state monads				*)
(*----------------------------------------------------------------------*)
datatype ('s, 'r) Monad = St of 's -> 'r * 's
  
(*----------------------------------------------------------------------*)
(* Wadler's unit and bind for state monads				*)
(*----------------------------------------------------------------------*)
fun unit v = St (fn state => (v, state))
fun bind (St st) f =
  St (fn s => let val (v, s') = st s in 
              let val St st' = f v 
              in st' s' end end)

(*----------------------------------------------------------------------*)
(* Map with state							*)
(*----------------------------------------------------------------------*)
fun monadmap f [] = 
    unit []

  | monadmap f (x::xs) =   
    bind (f x) (fn y =>
    bind (monadmap f xs) (fn ys =>
    unit (y::ys)))
