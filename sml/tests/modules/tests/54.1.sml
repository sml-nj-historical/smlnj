(* 54.1.sml *)
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
	  structure C1: sig end = A.D
        end
      structure A: S2
      structure E:
        sig
	  structure F: sig end = A.D
        end
    end
end;
