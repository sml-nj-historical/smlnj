(* bug 94, Tarditi -- Bind exception *)

functor F() =
struct
  datatype d = C
end

functor G() =
struct
  structure A = F()
  val x = A.C
end
