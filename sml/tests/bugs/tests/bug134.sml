(* bug134.sml *)

type 'a ft = (int * 'a) list;

fun f ([]:'a ft) x = []:'a ft
  | f (((y,fy)::l):'a ft) x = 
      if x = y
      then l:'a ft
      else (y,fy)::(f l x):'a ft

and g (l:'a ft) (x,fx) = (x,fx) :: (f l x):'a ft;
