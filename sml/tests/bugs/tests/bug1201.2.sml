(* bug1201.2.sml *)

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
  structure A : SA
end;

functor F
  (structure X : SX)
   (structure Y : SY  where A = X.A) =
struct
  fun f(x: X.A.t, y: Y.A.t list) = x::y
end;

functor G(structure A : SA) : 
  sig
    functor G1(structure Y: SY where A = A) : sig end
  end =
struct
  functor G1 = F(structure X = struct structure A = A end)
end;
