(* test15.sml *)
(* keywords: functor *)

(* derived from new mlyacc *)

signature TAB =
sig
  datatype term = T
end;

signature PAR =
sig
  structure PT : TAB
  val x : PT.term
  structure P1 : sig val x1 : PT.term end
end;

signature SIGX =
sig
  structure P: PAR
end;

functor Join(structure P: PAR) =
struct
  val a = (P.x = P.PT.T)
  val b = (P.x = P.P1.x1)
  val c = (P.P1.x1 = P.PT.T)
end;
