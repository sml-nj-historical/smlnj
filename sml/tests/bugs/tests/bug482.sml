(* bug482.sml *)
(* check signature/structure matching in presence of type abbreviations *)

    signature A =
    sig
      eqtype 'a t
      datatype d = C | D of int t
    end;

    structure Z =
    struct
      type 'a t = bool
      datatype d = C | D of bool t
    end;

    structure X : A = Z;

  (* The above line should work without errors *)
