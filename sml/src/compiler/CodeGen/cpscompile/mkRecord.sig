(* mk-record.sig --- translate a CPS.RECORD to MLRISC
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

signature MK_RECORD = sig
  structure T : MLTREE

  type rexp   = (unit, unit, unit, unit) T.rexp
  type fexp   = (unit, unit, unit, unit) T.fexp
  type stm    = (unit, unit, unit, unit) T.stm
  type mlrisc = (unit, unit, unit, unit) T.mlrisc

  val record : 
    {desc     : rexp, 
     fields   : (rexp * CPS.accesspath) list,
     ans      : int,
     mem      : CPSRegions.region,
     hp       : int,
     emit     : stm -> unit,
     markPTR  : rexp -> rexp, (* mark this as an ml object ptr (for gc) *) 
     markComp : rexp -> rexp  (* mark the component type (for gc) *)
    } -> unit

  val frecord : 
    {desc     : rexp, 
     fields   : (mlrisc * CPS.accesspath) list,
     ans      : int,
     mem      : CPSRegions.region,
     hp       : int,
     emit     : stm -> unit,
     markPTR  : rexp -> rexp,
     markComp : fexp -> fexp
    } -> unit
end

