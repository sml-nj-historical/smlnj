(* 302.sml *)
(* related to bug 1445 *)

signature S =
sig
  type t
  val x : t
end;

functor F
  (type s
   structure C : S
   datatype u = K of s
   sharing type u = C.t
   structure D : sig structure T : S end where T = C) =
struct end;

