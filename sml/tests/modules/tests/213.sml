signature S =
sig
  type t
  val x: t
end;

functor F() : S =
struct
  datatype h = H of int
  type t = h list
  val x = [H 3]
end;

functor G () =
struct
  structure A = F()
  val y = hd A.x
end;
