signature S1 =
sig
  type t = int
end;

signature S2 =
sig
  type t
end;

signature S =
sig
  structure A : S1
  structure B : S2

  sharing type A.t = B.t
end;

functor F(X:S) = struct end;
    
