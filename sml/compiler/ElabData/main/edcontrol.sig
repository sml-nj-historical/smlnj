(* edcontrol.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

signature ELABDATA_CONTROL =
sig

  val saveLvarNames : bool ref
  val eedebugging : bool ref
  val mudebugging : bool ref

end
