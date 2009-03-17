signature S =
sig
    type t
end

functor H(X:S) (Y:sig val f : X.t end)   =
struct
 
end

functor G = 
   H(struct type t = int end) 

