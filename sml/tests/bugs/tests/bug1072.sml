(* bug1072.sml *)

functor F
 (structure X : sig type t end
  structure T : sig type t end
  sharing type T.t = Y.t) =
struct end;
