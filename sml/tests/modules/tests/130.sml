signature S =
sig
  structure A: sig type s end

  structure B:
  sig
    type t = A.s
  end
end

signature T =
sig
  structure C: S
  type t = C.A.s
end

functor F(X: S): T = 
struct
  structure C = X
  type t = X.B.t
end
