signature S =
sig
    type t
end

functor H(X:S) (Y:sig val f : X.t end)   =
struct
 
end

functor G() = 
struct
functor I = H(struct type t = int end) 
   (* structure M = H(struct type t = int end) (struct val f = 1 end) *)

 
end

structure M = G()
