(* test68.sml *)
(* keywords: sharing, structure *)

(* Doligez *)
(* obtaining incompatible structures by multiple functor application *)
(* status : should not work, does not work in 66, does not work in dd67 *)

functor F (X : sig end) =
  struct structure A = struct end
  end;

structure B = struct end;

structure C = F (B);

structure D = F (B);

signature S =
  sig sharing C.A = D.A
  end;
