signature R =
sig
  type b
end;

signature S =
sig
  structure A : R
  val b0 : A.b
end;

structure B :> S =
struct
  structure A: R =
  struct
    type b = string
  end
  val b0 : A.b = Unsafe.cast()
end;

