(* bug816.sml *)
(* 816. repeated component names in signatures *)

signature S =
sig
  structure A :
    sig
      structure A : sig type t end
      val x : A.t
    end
end;
