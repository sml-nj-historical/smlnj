(* bug1170.1.sml *)

signature S =
sig
  type t1
  type t2
end
where type t1 = int and type t2 = bool;
