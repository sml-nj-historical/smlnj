(* bug1336.1.sml *)

fun f x =
      let fun g y =
                let fun h _ =
                            if x = y orelse x = y orelse x = y orelse x = y
                              then x = y
                            else if x = y
                              then f(x + 1)
                            else if x = y
                              then g(x + 1) 
                            else if x = y
                              then h(x + 1)
                            else true
                in h 0 end 
      in g 0 end;
