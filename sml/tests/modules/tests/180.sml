(* 180.sml *)
(* order of instanatiation *)

signature S1 =
sig
  structure A :
    sig
      type t               (* 2 *)
      datatype s = S       (* 1 *)
    end

  structure B :
    sig
      type v               (* 1 *)
      datatype s = C of v  (* 2 *)
    end

  sharing type A.t = B.s
      and type A.s = B.v
end;
