(* bug 340 -- Nick Rothwell thinks the first is legal but the second isn't *)
signature S =
    sig
	val = : 'a * 'a -> bool
    end

signature S' =
    sig
	val = : int
    end
