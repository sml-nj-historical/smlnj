(* bug930.sml *)

signature S_SIG = sig end;

structure S : S_SIG  =
struct
  fun f x = x 
  fun g ([],[]) = f (g ([],[]))
    | g (x,[]) = f (g ([],[]))
end;
