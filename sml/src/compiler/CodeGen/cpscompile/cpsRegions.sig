signature CPS_REGION = sig
  datatype region =
      RW_MEM				(* read-only memory *)
    | RO_MEM				(* read-write memory *)
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
 * $Log$
 *)
