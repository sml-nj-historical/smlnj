(* typecheck.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * SML/NJ-specific instantiation of the Typecheck functor.
 *)
structure Typecheck =
          TypecheckFn (val ii_ispure = InlInfo.pureInfo
		       val ii2ty = InstantiateParam.ii2ty)
