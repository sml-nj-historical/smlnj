signature S =
sig
  type t
  val x: t
end

functor F(X:S) : sig val y: X.t end = 
struct
  val y = X.x
end

functor G(Y : S) =
struct
  structure Z = F(Y)
  val _ = if true
	  then Y.x
	  else Z.y
end
