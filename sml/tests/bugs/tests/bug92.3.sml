(* bug92.3.sml *)

functor OrdSet(B : sig
			type elem
		   end) =
struct
end

structure Bad =
    struct
	type 'a elem = int * 'a
    end

structure  X = OrdSet(Bad)
