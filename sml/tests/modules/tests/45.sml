(* test45.sml *)
(* keywords: functor, datatype, typedef *)

functor F4() =
struct
  local
    datatype a = A of b
    and      b = B of a | C
  in
    type t = a
    val x : t = A(B(A(C)))
  end
end;
