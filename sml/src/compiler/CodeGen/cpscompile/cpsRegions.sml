structure CPSRegions = struct

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

  val stack = STACK
  val memory = RW_MEM
  fun error msg = ErrorMsg.impossible ("CPSRegions." ^ msg)

  fun toString region =
   (case region
    of RW_MEM => "rw_mem"
     | RO_MEM => "ro_mem"
     | STACK  => "stack"
     | REAL   => "real"
     | RVAR r => Int.toString r
     | STORELIST => "storelist"
     | MUTABLE(def,use) => "def=" ^ toString def ^ " use=" ^ toString use
     | RECORD _ => error "toString: RECORD"
     | OFFSET _ => error "toString: OFFSET"
     | REGIONS(r1, r2) => toString r1 ^ " " ^ toString r2
    (*esac*))

  fun trace(RECORD vl, CPS.SELp(j,rest)) = let
	val (_, region, ap) = List.nth(vl, j+1)
      in trace(trace(region, ap), rest)
      end
    | trace(OFFSET(i, vl), CPS.SELp(j,rest)) = let
	val (_, region, ap) = List.nth(vl, i+j+1)
      in trace(trace(region, ap), rest)
      end
    | trace(r, CPS.OFFp 0) = r
    | trace(RECORD vl, CPS.OFFp j) = OFFSET(j, vl)
    | trace(OFFSET(i,vl), CPS.OFFp j) = OFFSET(i+j, vl)
    | trace(MUTABLE _, _) = error "trace"
    | trace(_, _) = RO_MEM
end



(*
 * $Log: cpsRegions.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
