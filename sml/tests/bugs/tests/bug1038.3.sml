(* bug1038.3.sml *)

signature SIGA =
sig
  type t
end;

signature SIGB =
sig
  datatype tt = TT
  datatype t = T of tt	(*XXX*)
end;

functor F(structure A : SIGA
	  structure B : SIGB
	  sharing type A.t = B.t) = struct end;	(* works *)
