(* bug964.1.sml *)

signature D =
sig
  type 'a snark = 'a list
  datatype x = Y of int snark
end;

functor Test (structure Decl: D) =
struct
  fun test (Decl.Y _) = ()
end;
