(* bug1020.sml *)
(* printing of signature should include type defn *)

signature S =  sig type t=int->int val x:t end;
