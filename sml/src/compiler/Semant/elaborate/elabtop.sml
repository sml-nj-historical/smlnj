(* elabtop.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * SML/NJ-specific instantiation of the ElabTop functor.
 *)
structure ElabTop = ElabTopFn (structure ElabMod = ElabMod)
