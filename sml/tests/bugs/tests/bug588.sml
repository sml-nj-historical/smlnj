(* bug588.sml *)
(* printing of flex record patterns *)

(fn {...} => ()) 3;

(* the {...} in the above should not print out as {,...} *)
