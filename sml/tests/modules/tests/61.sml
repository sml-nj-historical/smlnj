(* test61.sml *)
(* keywords: sharing *)

(* Doligez *)
(* sharing with a substructure of itself *)
(* status : S1 : should issue a warning, does not in 66, does not in dd67 *)
(* status : S2 : should issue a warning, does not in 66, does in dd67 *)

signature S1 =
  sig structure A :
        sig structure A : sig end
        end
      sharing A = A.A
  end;

signature S2 =
  sig structure B : sig end
      structure C : S1
      sharing B = C.A
  end;
