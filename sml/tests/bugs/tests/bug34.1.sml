(* bug34.1.sml *)

structure foo =
struct

local
  exception Sort
in
fun sort (op > : ('x * 'x -> bool))
   = let fun select(min, best, hd::tl) = select(min,
					  if best > min
					   then if best > hd andalso hd > min
						 then hd else best
					   else hd,
					  tl)
	   | select(min, best, nil) = best;
	 fun lowest(best, hd::tl) = lowest( (if hd>best then best else hd), tl)
	   | lowest(best, nil) = best;
	 fun s (l as (hd::tl), min) = min
	   | s _ = raise Sort
      in fn (l as (hd::tl)) => let val v = lowest(hd,tl) in v :: s(l, v) end
	  | nil => nil
     end
end (* local *)

end;
