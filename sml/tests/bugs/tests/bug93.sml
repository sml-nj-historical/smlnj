(* bug: 93 (Tarditi) *)

signature T =
sig
  type 'pos token
end

signature L =
sig
  structure T : T
end

functor P(structure L : L) =
struct
  open L
end

structure L =
  struct
    structure T =
      struct
	type 'a token = int * 'a * 'a
      end
  end

structure B = P(structure L = L)

val x = (5,"","")
val _ = x : string L.T.token  (* this works *)
val _ = x : string B.T.token  (* this causes a type error - why ? *)
