(* bug869.sml *)
(* 869. uncaught exception: Subscript -- compiling signature with type def *)

signature S1 =
sig
  structure A: sig type s end 
  structure B: sig type t end
  (* A necessary, s necessary, & A must come before B *)
end;

signature S2 =
sig
  structure C: S1
  type v = C.B.t
end;
