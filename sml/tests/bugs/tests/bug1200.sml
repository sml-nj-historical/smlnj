(* bug1200.sml *)

signature S = sig
  type t
end;

functor F1(structure X : S
	   structure Y : sig type t = X.t
			 end) =
struct
  val x = 1
end;

functor F2(structure X : S
	   structure Y : S where type t = X.t) =
struct
  val x = 1
end;
