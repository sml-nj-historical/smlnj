signature S =
sig
  type t
  val x : int
end;

functor F(X:S)  = struct end;
