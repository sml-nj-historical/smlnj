(* bug1038.4.sml *)

signature SIGA =
sig
  type t
end;

signature SIGB =
sig
  datatype tt = TT
  type t = tt		(*XXX*)
end;

functor F(structure A : SIGA
	  structure B : SIGB
	  sharing type A.t = B.t) = struct end;	(* fails *)
