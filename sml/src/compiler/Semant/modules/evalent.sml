(* evalent.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * SML/NJ-specific instantiation of the EvalEntity functor.
 *)
structure EvalEntity = EvalEntityFn (structure I = Instantiate)
