(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltykernel.sig *)

signature LTYKERNEL = sig 

(* definitions of kind and kind-environment *)
type tkind

datatype tkindI
  = TK_MONO                                   (* ground mono tycon *)
  | TK_BOX				      (* boxed/tagged tycon *)
  | TK_SEQ of tkind list                      (* sequence of tycons *)
  | TK_FUN of tkind * tkind                   (* tycon function *)

(* definitions of named tyc variables *)
type tvar = LambdaVar.lvar                    (* temporary, not used *)
val mkTvar : unit -> tvar                     (* temporary, not used *)

(* definitions of tyc and tyc-environment *)
type tyc
type tycEnv 

datatype tycI
  = TC_VAR of DebIndex.index * int            (* tyc variable *)
  | TC_NVAR of tvar * DebIndex.depth * int    (* named tyc variable *)
  | TC_PRIM of PrimTyc.primtyc                (* primitive tyc *)
  | TC_FN of tkind list * tyc                 (* tyc abstraction *)
  | TC_APP of tyc * tyc list                  (* tyc application *)
  | TC_SEQ of tyc list                        (* tyc sequence *)
  | TC_PROJ of tyc * int                      (* tyc projection *)

  | TC_SUM of tyc list                        (* sum tyc *)
  | TC_FIX of (int * tyc * tyc list) * int    (* recursive tyc *) 

  | TC_TUPLE of tyc list                      (* std record tyc *)
  | TC_ARROW of rawflag * tyc list * tyc list (* std function tyc *)
  | TC_PARROW of tyc * tyc                    (* special fun tyc, not used *)

  | TC_BOX of tyc                             (* boxed tyc *)
  | TC_ABS of tyc                             (* abstract tyc *)
  | TC_CONT of tyc list                       (* std continuation tyc *)
  | TC_IND of tyc * tycI                      (* indirect tyc thunk *)
  | TC_ENV of tyc * int * int * tycEnv        (* tyc closure *)

withtype rawflag = bool * bool    (* single or multiple arguments/results ? *)

(* definition of lty *)
type lty
datatype ltyI          
  = LT_TYC of tyc                             (* monomorphic type *)  
  | LT_STR of lty list                        (* structure type *)
  | LT_FCT of lty list * lty list             (* functor type *)
  | LT_POLY of tkind list * lty list          (* polymorphic type *)
    
  | LT_PST of (int * lty) list                (* partial-str type *)
  | LT_CONT of lty list                       (* internal cont type *)
  | LT_IND of lty * ltyI                      (* indirect type thunk *)
  | LT_ENV of lty * int * int * tycEnv        (* type closure *)

(** injections and projections on tkind, tyc, and lty *)
val tk_inj   : tkindI -> tkind 
val tc_inj   : tycI -> tyc
val lt_inj   : ltyI -> lty

val tk_out   : tkind -> tkindI
val tc_out   : tyc -> tycI
val lt_out   : lty -> ltyI

(** key comparison for tkind, tyc, and lty; used in pickling *)
val tk_cmp   : tkind * tkind -> order
val tc_cmp   : tyc * tyc -> order
val lt_cmp   : lty * lty -> order

(** get the hash key of each lty, used by reps/coerce.sml; a hack! *)
val lt_key   : lty -> int

(** testing the equivalence for regular tkinds, tycs and ltys *)
val tk_eqv   : tkind * tkind -> bool
val tc_eqv   : tyc * tyc -> bool
val lt_eqv   : lty * lty -> bool

(** testing the equivalence for tycs and ltys with relaxed constraints *)
val tc_eqv_bx: tyc * tyc -> bool
val lt_eqv_bx: lty * lty -> bool

(** utility functions on tycEnv *)
exception tcUnbound
val initTycEnv : tycEnv
val tcInsert : tycEnv * (tyc list option * int) -> tycEnv

(** testing if a tyc (or lty) is in the normal form *)
val tcp_norm : tyc -> bool
val ltp_norm : lty -> bool

(** finding out the depth for a tyc's innermost-bound free variables *)
val tc_depth : tyc * DebIndex.depth -> DebIndex.depth
val tcs_depth: tyc list * DebIndex.depth -> DebIndex.depth

(** utility functions for TC_ENV and LT_ENV types *)
val tcc_env  : tyc * int * int * tycEnv -> tyc
val ltc_env  : lty * int * int * tycEnv -> lty

(** reducing a tyc or lty into the weak-head normal form *)
val tc_whnm : tyc -> tyc
val lt_whnm : lty -> lty

(** reducing a tyc or lty into the true normal form *)
val tc_norm : tyc -> tyc
val lt_norm : lty -> lty

(** automatically flattening the argument or the result type *)
val lt_autoflat : lty -> bool * lty list * bool

(** tcc_arw does automatic argument and result flattening *)
val tcc_arw : rawflag * tyc list * tyc list -> tyc

end (* signature LTYKERNEL *)


