(* bug172.sml *)

signature SIG1 =
sig
  type s
end;

signature SIG2 =
sig
  type t
  val x : t

  structure Sub : 
  sig
    val f : t -> t
  end;
end;

functor F (structure S1 : SIG1
	   structure S2 : SIG2) =
struct
  val fx = S2.Sub.f S2.x;
end;
