signature S =
sig
  type s
  type t = s
end

signature T =
sig
  structure A : S
  type u
  sharing type u = A.t
end

functor F(X : T) = struct end
