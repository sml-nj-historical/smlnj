(* bug932a.sml *)
(* 932. Compiler bug: applyTyfun: not enough arguments *)

signature S = sig type 'a t = 'a list end;

structure T : S = struct type 'a t = 'a list end;
