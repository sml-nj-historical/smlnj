(* bug1246.sml *)

(* this works *)
fun f() = (let val 'a id :'a -> 'a = fn z => z in id id end, 
           fn z => z:'b);

(* this doesn't *)
fun f() = (let val 'a id :'a -> 'a = fn z => z in id id end, 
           fn z => z:'a);
