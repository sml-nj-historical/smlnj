signature S1 =
sig
  type t
end

signature S2 =
sig
  structure A : S1
  structure B : S1
  open A
  open B
  val x : t
end
