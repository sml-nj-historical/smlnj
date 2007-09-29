(* ltytostring.sig *)

signature LTYTOSTRING =
sig

  (** pretty printing of tkinds, tycs, and ltys *)
  val tk_print   : Lty.tkind -> string
  val tc_print   : Lty.tyc -> string
  val lt_print   : Lty.lty -> string

end (* signature LTYTOSTRING *)
