(* frag.sig --- code and data fragments that need to be compiled.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature FRAG = sig
  structure T : MLTREE
  datatype generated =
      UNGEN of CPS.lvar * CPS.lvar list * CPS.cty list * CPS.cexp
    | GEN of T.mlrisc list

  datatype frag =
      STANDARD of {func: CPS.function option ref, 
		   fmlTyps: CPS.cty list}
    | KNOWNFUN of generated ref
    | KNOWNCHK of generated ref 

  val makeFrag : CPS.function * Label.label -> frag
  val next : unit -> (Label.label * frag) option
  val add : (Label.label * frag) -> unit
end (* FRAG *)





