(* bug1274.sml *)

functor F
  (structure A : S  (* undefined signature S *)
   structure B : S
   sharing type B.t = A.s) =
struct end;
