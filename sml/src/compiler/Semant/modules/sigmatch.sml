(* sigmatch.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * SML/NJ-specific instantiation of the SigMatch functor.
 *)
structure SigMatch = SigMatchFn (structure EV = EvalEntity)
