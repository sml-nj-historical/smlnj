(* bug416.sml *)
(* lack of equality type checking on functor application *)

signature PSIG =
sig
  eqtype 'a symTab ;
  datatype guide = G1 | G2 of guide symTab  (* guide an eqtype *) 
end;

functor PFUN (structure S : sig type 'a symTab end) =
struct
  open S;
  datatype guide = G1 | G2 of guide symTab;  (* this guide is not an eqtype *)
end;

structure S = struct datatype 'a symTab = Empty end;
structure P = PFUN(structure S = S);  (* P.guide is not an eqtype *)

(*
P.G1 = P.G1;  (* this is illegal *)
*)

functor MFUN(structure X : PSIG) =
struct
  val z = X.G1 = X.G1;
end;
structure M = MFUN(structure X = P);

(* This functor application should be illegal because P.guide is not
   an equality type as required by signature PSIG *)
