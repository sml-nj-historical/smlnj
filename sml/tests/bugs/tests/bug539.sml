(* bug539.sml *)
(* Greiner, weak typing *)
(* type of r given as '1a list ref *)

let val r = (let val x = ref nil in fn y => x end) ()
in r:=[1]; hd(!r)^"hi" 
end;    
