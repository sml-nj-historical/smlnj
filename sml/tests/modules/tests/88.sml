signature S0 = sig type t end

signature S =
  sig
     structure B : S0
  end

functor F(A : S) = A.B

structure S = 
     struct
       structure B = struct type t =int end
     end

(* structure T should have the same stamp as S.B *)

structure T=F(S)
