(* bug92.4.sml *)

functor OrdSet(B : sig
			type elem
			val gt : elem * elem -> bool
		   end) =
struct
end

structure Bad =
    struct
	type 'a elem = int * 'a
	val gt = fn ((a:int,_),(b,_)) => a > b
    end

structure  X = OrdSet(Bad)
