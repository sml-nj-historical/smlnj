signature CPS_REGION = sig
  datatype region =
      RW_MEM				(* read-write memory *)
    | RO_MEM				(* read-only memory *)
    | STACK				(* stack *)
    | REAL				(* real *)
    | RVAR of int			(* region variable *)
    | STORELIST				(* storelist *)
    | RECORD of (region * region * CPS.accesspath) list	
					(* record (def/usr pairs) *)
    | OFFSET of int * (region * region * CPS.accesspath) list
					(* offset def/use pairs *)
    | MUTABLE of region * region	(* array, ref, ... def/use  *)
    | REGIONS of region * region	(* collection *)

  val stack : region
  val memory : region
  val trace : region * CPS.accesspath -> region
  val toString : region -> string
end

(*
 * $Log: cpsRegions.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
