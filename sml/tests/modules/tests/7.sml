(* test7.sml *)
(* keywords: functor *)

(* from Tarditi -- tests null parameter and null argument *)

functor F() =
struct
  datatype d = D
end;

functor G() =
struct
  structure A = F()
  val x = A.D
end;
