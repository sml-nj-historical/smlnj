(* mk-record.sig --- translate a CPS.RECORD to MLRISC
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature MK_RECORD = sig
  structure T : MLTREE

  val record : 
    {desc: T.rexp, 
     fields: (T.rexp * CPS.accesspath) list,
     ans: int,
     mem: CPSRegions.region,
     hp : int
    } -> unit

  val frecord : 
    {desc: T.rexp, 
     fields: (T.mlrisc * CPS.accesspath) list,
     ans: int,
     mem: CPSRegions.region,
     hp : int
    } -> unit
end

(*
 * $Log: mkRecord.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
