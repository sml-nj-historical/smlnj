(* bug1499.1.sml *)

signature Bug =
sig
    type t
    type t
    datatype u = C of t
    and v = D
end
