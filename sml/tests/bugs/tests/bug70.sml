(* bug70.sml
70. constructor shouldn't appear in printed structure signature
*)

signature SIG =
sig
  type t
end;

structure S:SIG =
struct
  datatype t = foo of int
  val x = 3
end;
