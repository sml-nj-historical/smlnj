(* 54.sml *)
(* keywords: sharing *)

signature S0 =
sig
  structure D : sig end
end;

signature S2 =
sig
  structure C : sig end
  structure D : sig end
end;

signature S1 = 
sig
  structure A: S0
  structure B:
    sig
      structure C:
        sig
	  structure C1: sig end
	  sharing C1 = A.D  (* A.D not local to C or B, but local to S1
			       A.D normalizes to A.D,
			       C1 normallizes to B.C.C1 *)
        end
      structure A: S2
      structure E:
        sig
	  structure F: sig end
	  sharing F = A.D  (* A.D normalizes to B.A.D,
			      F normalizes to B.E.F *)
        end
    end
end;
