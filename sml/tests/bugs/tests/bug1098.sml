(* bug1098.sml *)

structure T : sig end =
struct
  datatype B = U

  fun FVu'  depth (_,  acc) = acc
  and FVus' depth (xs, acc) = foldl (FVu' depth) acc xs

  fun FVb depth U = FVu' depth ((), [])

  fun wrap_FV FV' (d : int) = FV' d

  fun FVus d = wrap_FV FVus' d
end
