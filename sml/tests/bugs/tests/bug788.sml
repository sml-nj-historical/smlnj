(* bug788.sml *)
(* 788. "open" reports an error (it should always work) *)

signature S = sig type a; val nil:a; val :: :int*a->a end;

structure A: S = struct datatype a = nil | :: of int*a end;

open A;
