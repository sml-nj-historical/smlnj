(* bug1385.3.sml *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  structure A : S1
  structure B : S1 = A
end;

structure X : S2 =
struct
  structure A =
    struct
      type t = int list
    end
  structure B =
    struct
      type t = bool
    end
end;
