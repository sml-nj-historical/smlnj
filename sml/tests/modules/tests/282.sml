(* 282.sml *)
(* where types and sigmatch *)

signature S =
sig
  type t
end;

signature S1 =
sig
  type s
  structure A: S where type t = s
end;

structure A : S1 =
struct
  type s = bool
  structure A =
    struct
      type t = int
    end
end;


