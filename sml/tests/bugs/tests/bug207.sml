(* bug207.sml *)

signature SS =
sig
  datatype t = B of t | A of t list
end

functor F(X:SS) =  
struct
  fun f(X.B(v)) = (v :: nil = v :: nil) | f _ = false
end
