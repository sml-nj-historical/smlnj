structure SMLGCType : SMLGCTYPE =
struct

   structure CPS = CPS

   type ty = int
  
   datatype gctype =
     CONST of IntInf.int           (* integer constant *)
   | NONREF of CPS.cty ref         (* non-reference value *)
   | REF of CPS.cty ref            (* a reference, pointer to a gc object *)
   | PLUS of ty * gctype * gctype   (* address arithmetic + *)
   | MINUS of ty * gctype * gctype   (* address arithmetic - *)
   | ALLOCPTR 
   | LIMITPTR 
   | BOT
   | TOP

   fun toString BOT = "bot"
     | toString TOP = "top"
     | toString (CONST i) = IntInf.toString i
     | toString (NONREF(ref obj)) = CPS.ctyToString obj
     | toString (REF(ref obj)) =  CPS.ctyToString obj
     | toString (PLUS(ty,a,b)) = "("^toString a^"+"^toString b^")"
     | toString (MINUS(ty,a,b)) = "("^toString a^"-"^toString b^")"
     | toString ALLOCPTR = "allocptr"
     | toString LIMITPTR = "limitptr"

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
  val INT    = I32 (* untagged integer *)
  val REAL32 = TOP (* unused in SML/NJ *)

  fun ADD(_,TOP,x) = TOP
    | ADD(_,x,TOP) = TOP
    | ADD(ty,CONST i,CONST j) = (CONST(IntInf.+(i,j)) handle Overflow => INT)
    (*| ADD(ty,CONST 0,b) = b
    | ADD(ty,b,CONST 0) = b*)
    | ADD(ty,CONST _,NONREF _) = INT
    | ADD(ty,NONREF _,CONST _) = INT
    | ADD(ty,x as NONREF a,y as NONREF b) = if a = b then x else INT
    | ADD(ty,x,y)  = PLUS(ty,x,y)
  fun SUB(_,TOP,x) = TOP
    | SUB(_,x,TOP) = TOP
    | SUB(ty,CONST i,CONST j) = (CONST(IntInf.+(i,j)) handle Overflow => INT)
    (*| SUB(ty,a,CONST 0) = a*)
    | SUB(ty,CONST _,NONREF _) = INT
    | SUB(ty,NONREF _,CONST _) = INT
    | SUB(ty,x as NONREF a,y as NONREF b) = if a = b then x else INT
    | SUB(ty,x,y)  = MINUS(ty,x,y)

  fun isRecoverable TOP = false
    | isRecoverable BOT = false (* XXX *)
    | isRecoverable _   = true

  exception GCTYPE of gctype
  val GC_TYPE = Annotations.new'{create=GCTYPE,
                                 get=fn GCTYPE x => x | e => raise e,
                                 toString=toString}
end
