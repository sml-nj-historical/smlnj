(* bug1446.1.sml *)

Compiler.Control.lazysml := true;

infixr 5 >>;

datatype lazy series = >> of int * series;

fun lazy recip () =  (* ok if "lazy" removed on this line *)
    let val rec lazy rs = 1 >> rs
     in rs
    end;
