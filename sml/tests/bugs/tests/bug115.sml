(* bug115.sml *)

(* shouldn't be allowed, since object (signature) is not cycle-free *)
signature bad =
sig
  structure A :
  sig
    structure B : sig end;
  end;
  sharing A = A.B;
end;
