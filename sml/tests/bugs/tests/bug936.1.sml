(* bug936.1.sml *)

fun foo x = 
    let abstype t1 = T | F
        with
          val goo = foo
        end
     in goo x
    end;
