(* bug1446.2.sml *)

Compiler.Control.lazysml := true;

datatype lazy series = >> of int * series;

fun lazy tail (x>>xs) = xs;

fun lazy plus (f>>fs,g>>gs) = f+g >> plus(fs,gs);

let val rec lazy rs =
        let val lazy rs1 = tail rs
         in 0 >> 1 >> plus(rs1,rs)
        end
 in rs
end;
