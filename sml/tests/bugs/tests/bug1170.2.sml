(* bug1170.2.sml *)

signature S =
sig
  type t1
  type t2
end
where type t1 = int
where type t2 = bool;
