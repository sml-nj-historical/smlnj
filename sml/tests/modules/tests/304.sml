(* 304.sml *)
(* related to bug 1445 *)

signature S =
sig
  type t
  val x : t
end;

functor F
  (type s
   datatype u = K of s  (* moved datatype u *)
   structure C : S
   sharing type u = C.t
   structure D : sig structure T : S end where T = C) =
struct end;


