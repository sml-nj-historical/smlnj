(* causes memory fault *)

structure S =
struct
  datatype d = D of t
  and s = A | B of string
  and t = C of s list
end;

val p = S.C[S.B "b"];
