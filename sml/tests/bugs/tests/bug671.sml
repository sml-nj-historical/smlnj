(* bug671.sml *)
(* 671. visibility of parameter in functor signature *)

(* case 1 *)
 funsig FSIG(B : sig type t end) = sig val f : B.t end;

(* case 2 *)
 funsig FSIG(structure B : sig type t end) = sig val f : B.t end;

(* case 3 *)
 funsig FSIG(B : sig type t end) = sig val f : t end;

(* case 4 *)
 funsig FSIG(structure B : sig type t end) = sig val f : t end;
