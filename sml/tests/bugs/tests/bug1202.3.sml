(* bug1202.3.sml *)

signature SA =
sig
  type t
end;

signature SX =
sig
  structure A : SA
end;

signature SY =
sig
  structure A : SX
end;

signature SZ =
sig
  structure X : SX
  structure Y : SY  where  A = X
end;

functor F(Z: SZ) =
struct
end;
