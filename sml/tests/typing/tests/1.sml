(* 1.sml *)
(* keywords: ref, weak *)

let val rid = (fn x => !(ref x))
 in let val f = (fn y => rid (fn a => a) y)
     in f 0; f true
    end
end;
