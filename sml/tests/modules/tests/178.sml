(* 178.sml *)
(* order of instanatiation *)

signature S1 =
sig
  structure A :
    sig
      type t               (* 2 *)
    end

  structure B :
    sig
      type v               (* 1 *)
      datatype s = C of v  (* 2 *)
      type u = A.t         (* 3 *)
    end

  sharing type A.t = B.s

end;
