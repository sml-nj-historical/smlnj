(* bug1414.1.sml *)

signature ORD_KEY =
sig
  type ord_key
end;

signature SIG1 = 
sig
  type t
  type u

  datatype c = C of u 
  and d = D of t * c   (* needed! *)
end;

signature SIG2 =
sig
  structure K : ORD_KEY
  type s = K.ord_key         (* <------ that is the culprit I think *)
end;

signature SIG3 = 
sig
  structure A : SIG1 
  structure B : SIG2
    sharing type B.s = A.c  (* not valid *)
end;
