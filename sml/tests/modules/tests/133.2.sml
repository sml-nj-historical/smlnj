signature S =
sig
end;

structure A  =
struct
  structure U : S = struct end
end;

signature T =
sig
  structure X: S
end where X =A.U;

functor F() : T =
struct
  structure X = A.U
end;

