(* bug1332.sml *)

signature SIG =
sig
   structure Z : sig end
end;

signature BUG =
sig
   structure A : sig end
   structure B : SIG where Z = A
   structure C : SIG where Z = A
   sharing B = C
end;
