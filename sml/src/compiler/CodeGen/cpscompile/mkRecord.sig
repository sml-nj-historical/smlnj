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
 * Revision 1.5  1997/08/11 18:38:03  george
 *   Implemented correct but very conservative alias information for
 *   reference cells.
 *
 * Revision 1.4  1997/08/07  02:10:50  george
 *   Refined region information to the granularity of words  in the allocation space
 *
 * Revision 1.3  1997/08/03  14:16:00  george
 *    Allocation pointer increments are performed at function exit
 *    if possible.
 *
 * Revision 1.2  1997/07/28  20:04:51  george
 *   Added support for regions
 *
 * Revision 1.1.1.1  1997/01/14  01:38:34  george
 *   Version 109.24
 *
 *)
