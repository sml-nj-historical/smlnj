(* bug174.sml *)

signature ASig =
sig
  datatype POP = a | b
end;

signature BSig =
sig
  structure DT : ASig
  val f : DT.POP -> unit
end;

functor AFun () : ASig =
struct
  datatype POP = a | b
end;

functor BFun (structure DT : ASig) : BSig =
struct
  structure DT = DT
  open DT
  val f = fn _ => outputc std_out "Is Running\n"
end;

