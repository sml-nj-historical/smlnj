(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ppflint.sig -- Pretty printer for Flint IL. *)

signature PPFLINT =
sig
    val printFKind : FLINT.fkind -> unit
    val printRKind : FLINT.rkind -> unit
    val printCon   : FLINT.con -> unit
    val printSval  : FLINT.value -> unit
    val printLexp  : FLINT.lexp -> unit
    val printFundec: FLINT.fundec -> unit
    val printProg  : FLINT.prog -> unit

    (* defaults to LV.lvarName *)
    val LVarString  : (FLINT.lvar -> string) ref

end (* signature PPFLINT *)


(*
 * $Log: ppflint.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:38  george
 * Version 110.5
 *
 *)
