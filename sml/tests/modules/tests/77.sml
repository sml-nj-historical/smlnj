(* this shows that *all* deftycs in a functor body which is simple need to be abstracted. *)

functor F(A : sig type t val x : t end) =
struct
   type s = A.t
   val y : s = A.x
   type s = int
end

structure S = F(struct type t=bool val x=true end);
structure S' = F(struct type t=int val x=5 end);

structure A = struct
val a = S.y : bool;   (* this doesn't work in version 0.70 ! *)
end

structure B = struct
val a = S'.y : int
end

(*
(* now to show that the current implementation is unsound! *)

val join = fn (a:'a,b:'a) => a
val bad = join(S.y,S'.y);  (* S.y has type bool and S'.y has type int *)
*)
