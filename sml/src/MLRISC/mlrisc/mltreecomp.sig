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
 * $Log$
 *)
