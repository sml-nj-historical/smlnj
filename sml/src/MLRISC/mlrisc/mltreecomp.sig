(* mltreeComp.sig --- translate mltrees to a flowgraph of target machine code.
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)
signature MLTREECOMP = sig
    structure T : MLTREE
    val mltreeComp : T.mltree -> unit
    val mlriscComp : T.stm -> unit
end

(*
 * $Log: mltreecomp.sig,v $
 * Revision 1.1.1.1  1997/04/19 18:14:21  george
 *   Version 109.27
 *
 *)
