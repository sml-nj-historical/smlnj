signature R =
sig
  type b
end;

signature AS =
sig
  structure A : R
  val b0 : A.b
end;

structure S : AS =
struct
  structure AA =
  struct
    type b = string
  end
  structure A : R = AA
  val b0 : A.b = Unsafe.cast()
end;

