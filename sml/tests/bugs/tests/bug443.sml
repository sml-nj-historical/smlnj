(* bug443.sml *)

signature S =
  sig
    type 'a s

    datatype t = A of int -> int

    datatype
      v = D of w s
    and
      w = E of t
  end
