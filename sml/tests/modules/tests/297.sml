(* 297.sml *)
(* same as bug1445.1.sml *)

signature S1 =
sig
  type t
  val x : t  (* needed *)
end;

signature S2 =
sig
  structure T : S1
end;

signature S3 =
sig
  type s
  datatype t = Kt of s  (* s needed *)
end;

(* ok *)
functor F1
  (structure C : S2
   structure D : S2
   structure E : S3
   sharing D.T = C.T
   sharing type E.t = C.T.t) =
struct end;

(* ok *)
functor F2
  (structure C : S2
   structure D : S2 where T = C.T
   structure E : S3 where type t = C.T.t) =
struct end;

(* uncaught exception Unbound at FLINT/trans/transtypes.sml:277.33-277.43 *)
functor F
  (structure C : S2
   structure D : S2 where T = C.T  (* needed *)
   structure E : S3
   sharing type E.t = C.T.t) =  (* needed *)
struct end;

(* in 110.0.3 error message is:
  uncaught exception Unbound
     raised at: translate/transmodules.sml:80.33-80.43
                translate/translate.sml:129.39
*)
