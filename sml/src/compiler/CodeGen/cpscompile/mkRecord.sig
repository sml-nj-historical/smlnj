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
     hp : int,
     emit : T.stm -> unit
    } -> unit

  val frecord : 
    {desc: T.rexp, 
     fields: (T.mlrisc * CPS.accesspath) list,
     ans: int,
     mem: CPSRegions.region,
     hp : int,
     emit : T.stm -> unit
    } -> unit
end

(*
 * $Log$
 *)
