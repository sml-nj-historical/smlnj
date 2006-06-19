(* typecheck.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * SML/NJ-specific instantiation of the Typecheck functor.
 *)

(* [dbm, 6/16/06] Obsolete -- this file will be removed.
   Typecheck now defined directly in Elaborator/types/typecheck.sml,
   no longer functorized. *)

structure Typecheck =
          TypecheckFn (val ii_ispure = InlInfo.pureInfo
		       val ii2ty = InstantiateParam.ii2ty)
