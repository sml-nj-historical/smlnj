(* elabmod.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * SML/NJ-specific instantiation of the ElabMod functor.
 *)
structure ElabMod = ElabModFn (structure SM = SigMatch
			       structure Typecheck = Typecheck)
