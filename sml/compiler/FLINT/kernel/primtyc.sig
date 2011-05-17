(* Copyright 1996 by AT&T Bell Laboratories *)
(* primtyc.sig *)

signature PRIM_TYC = 
sig

eqtype primtyc

(** the primitive type constructors *)
val ptc_int31  : primtyc 
val ptc_int32  : primtyc 
val ptc_real   : primtyc 
val ptc_string : primtyc 
val ptc_exn    : primtyc 
val ptc_void   : primtyc 
                 
val ptc_array  : primtyc 
val ptc_vector : primtyc 
val ptc_ref    : primtyc 
val ptc_list   : primtyc   (* currently not used *)
val ptc_etag   : primtyc   (* exception tag *)
                 
val ptc_cont   : primtyc 
val ptc_ccont  : primtyc 
val ptc_arrow  : primtyc 
val ptc_option : primtyc 

val ptc_obj    : primtyc 
val ptc_cfun   : primtyc 
val ptc_barray : primtyc 
val ptc_rarray : primtyc 
val ptc_slock  : primtyc 

(*                 
 * val ptc_boxed  : primtyc 
 * val ptc_tgd    : primtyc 
 * val ptc_utgd   : primtyc 
 * val ptc_tnsp   : primtyc 
 * val ptc_dyn    : primtyc  
 *)

(** misc utility functions on primtyc *)
val pt_arity   : primtyc -> int
val pt_print   : primtyc -> string

(** hash-consing each prim tyc *)
val pt_toint   : primtyc -> int
val pt_fromint : int -> primtyc

(** equality of primtycs *)
val pt_eq : primtyc * primtyc -> bool

(** check the boxity of values of each prim tyc *)
val unboxed : primtyc -> bool

val bxupd : primtyc -> bool
val ubxupd : primtyc -> bool

val isvoid : primtyc -> bool

end (* signature PRIM_TYC *)

