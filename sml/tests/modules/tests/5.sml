(* test5.sml *)
(* keywords: functor *)

(* tests sibling type reference *)

signature VAL =
sig
  structure A : sig type s end
  structure B : sig val a : A.s end
end;

structure S =
struct
  structure A = struct type s = int end
  structure B = struct val a = 3 end
end;

functor F(X: VAL) = struct end;

structure M = F(S);
