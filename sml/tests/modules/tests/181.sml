(* 181.sml *)
(* order of instanatiation *)

signature S1 =
sig
  structure A :
    sig
      type t
    end

  structure B :
    sig
      datatype s = C of A.t   (* circularity! *)
    end

  sharing type A.t = B.s
end;
