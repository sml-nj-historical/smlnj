(* bug: 94, Tarditi -- Bind exception *)

functor G() =
struct
  structure A =
    struct
      datatype d = C
    end
  val x = A.C
end
