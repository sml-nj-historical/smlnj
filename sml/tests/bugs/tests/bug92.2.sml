(* bug92.2.sml *)

functor OrdSet(B : sig
			type elem
			val gt : elem * elem -> bool
			val eq : elem * elem -> bool
		   end) =
struct
end

structure Bad =
    struct
	type 'a elem = int * 'a
	val gt = fn ((a:int,_),(b,_)) => a > b
	val eq = fn ((a:int,_),(b,_)) => a = b
    end

structure  X = OrdSet(Bad)
