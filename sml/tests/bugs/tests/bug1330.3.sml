(* bug1330.3.sml *)

signature S =
sig
  type s
  structure U : sig
    type 'a t
    type u = (int * real) t
  end where type 'a t = s
end where type U.u = int;
