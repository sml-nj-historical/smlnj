(* bug82.sml *)
(* compiler bug caused by type in datatype declaration *)

datatype a = A of int;
datatype b = B of A;                    (* typo for B of a *)
