(* 67.sml *)
(* keywords: sharing, structure *)

(* Doligez *)
(* complex sharing within a structure *)
(* should not work *)

structure A = struct type t = int end;

structure B = struct type t = bool end;

signature S =
sig structure C : sig type t end
    structure D : sig type t end
    sharing type C.t = D.t
end
where C = A and D = B;
