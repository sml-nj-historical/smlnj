(* test71.sml *)

(* tests type abbreviation during signature matching *)

signature S =
sig
  type 'a t
  val x : int t
end;

structure A: S =
struct
  type 'a t = bool
  val x : string t = true
end;
