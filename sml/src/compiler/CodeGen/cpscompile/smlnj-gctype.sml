structure SMLGCType : SMLGCTYPE =
struct

   structure CPS = CPS

   type objtype = CPS.cty 
  
   datatype gctype =
     CONST of int                  (* integer constant *)
   | NONREF of objtype ref         (* non-reference value *)
   | REF of objtype ref            (* a reference, pointer to a gc object *)
   | ADD of int * gctype * gctype  (* address arithmetic + *)
   | SUB of int * gctype * gctype  (* address arithmetic - *)
   | BOT
   | TOP

   fun int i = if i >= 0 then Int.toString i else "-"^Int.toString(~i)

   fun toString BOT = "bot"
     | toString TOP = "top"
     | toString (CONST i) = int i
     | toString (NONREF(ref obj)) = CPS.ctyToString obj
     | toString (REF(ref obj)) =  CPS.ctyToString obj
     | toString (ADD(ty,a,b)) = "("^toString a^"+"^toString b^")"
     | toString (SUB(ty,a,b)) = "("^toString a^"-"^toString b^")"

   fun ==(x:gctype, y:gctype) = x = y

   fun join(BOT, x) = x
     | join(x, BOT) = x
     | join(TOP, x) = TOP
     | join(x, TOP) = TOP
     | join(x, y)   = x  (* XXX *)

   fun meet(BOT, x) = BOT 
     | meet(x, BOT) = BOT 
     | meet(TOP, x) = x 
     | meet(x, TOP) = x 
     | meet(x, y)   = x  (* XXX *)

  val I31    = NONREF(ref CPS.INTt)    (* tagged integers *)
  val I32    = NONREF(ref CPS.INT32t)  (* untagged integers *)
  val REAL64 = NONREF(ref CPS.FLTt)    (* untagged floats *)
  val PTR    = REF(ref(CPS.PTRt(CPS.VPT))) (* boxed objects (pointers) *)

end

structure SMLGCMap = GCMap(SMLGCType)
