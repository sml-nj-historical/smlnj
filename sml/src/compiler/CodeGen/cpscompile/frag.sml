(* frag.sml --- code and data fragments that need to be compiled.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)
functor Frag(MLTree:MLTREE) : FRAG = struct
  structure T = MLTree
  datatype generated =
      UNGEN of CPS.lvar * CPS.lvar list * CPS.cty list * CPS.cexp
    | GEN of T.mlrisc list

  datatype frag =
      STANDARD of {func: CPS.function option ref, 
		   fmlTyps: CPS.cty list}
    | KNOWNFUN of generated ref 
    | KNOWNCHK of generated ref
    | STRINGfrag of string
    | REALfrag of string

  fun error msg = ErrorMsg.impossible ("Frag." ^ msg)

  val frags = ref ([]: (Label.label * frag) list)

  fun next () = 
    case !frags
     of frag::rest => SOME frag before (frags := rest)
      | [] => NONE

  fun add lf = frags := lf :: !frags

  (* make compilation fragments for this cluster *)
  fun makeFrag (arg as (fk, f, vl, cl, e), lab) = let
    val frag = (case fk
      of (CPS.ESCAPE | CPS.CONT) => STANDARD{func=ref(SOME arg), fmlTyps=cl}
       | CPS.KNOWN => KNOWNFUN (ref(UNGEN(f,vl,cl,e)))
       | CPS.KNOWN_CHECK => KNOWNCHK (ref(UNGEN(f,vl,cl,e)))
       | _  => error "makeFrag"
      (*esac*))
  in
    frags:=  (lab, frag) :: !frags;
    frag
  end

end (* Frag *)




(*
 * $Log: frag.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:34  george
 *   Version 109.24
 *
 *)
