(* bug1259.2.sml *)

structure S : sig
		type b'
		datatype a = A1 | A2 of b'
		datatype b = B1 | B2 of a
		sharing type b = b'
	      end =
struct
  datatype a = A1 | A2 of b
  and b = B1 | B2 of a
  type b' = b
end
