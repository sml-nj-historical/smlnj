(* bug1573.1.sml *)

functor F(A: ARRAY) =
struct
  val _ = A.fromList [] = A.fromList []
end;
