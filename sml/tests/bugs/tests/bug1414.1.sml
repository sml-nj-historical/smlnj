(* bug1414.1.sml *)

signature SIG1 = 
sig
  type t
  type u

  datatype c = C of u 
  and d = D of t * c   (* needed! *)
end;

signature SIG2 =
sig
  type s = int         (* <------ that is the culprit I think *)
end;

signature SIG3 = 
sig
  structure A : SIG1 
  structure B : SIG2
    sharing type B.s = A.c  (* not valid *)
end;
