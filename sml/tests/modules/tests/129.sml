signature S =
sig
  structure A: sig type s end

  structure I:
  sig
    type t = A.s
  end
end

signature T =
sig
  structure B: S
  type t = B.A.s
end

functor F(B: S): T = 
struct
  structure B = B
  structure I = B.I
  open I
end
