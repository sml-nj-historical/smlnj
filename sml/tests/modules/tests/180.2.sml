(* 180.2.sml *)
(* syntax of multiple sharing equaltions *)

signature S1 =
sig
  structure A :
    sig
      type t
      type s
    end

  structure B :
    sig
      type v
      type s
    end

  sharing type A.t = B.s
      and type A.s = B.v
end;
