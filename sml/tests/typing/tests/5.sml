(* 5.sml *)
(* type checking test *)

(* this example illustrates the need to reset the lambda depth of
   a metavariable when its generalization is blocked because of
   the value restriction *)

let val x = ref []
    fun g () = !x  (* better not generalize
in x := [1];
   g()();    (* try to apply contents of x as function *)
end;
