signature S =
sig
  type t
  val x: t
end;

structure A : S =
struct
  type t = int
  val x = 3
end;
