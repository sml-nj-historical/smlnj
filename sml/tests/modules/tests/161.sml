signature S =
sig
  type s
  type t = s
end

signature T =
sig
  type u
  structure A : S
  sharing type u = A.t
end

functor F(X : T) = struct end
