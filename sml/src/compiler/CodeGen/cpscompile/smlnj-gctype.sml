structure SMLGCType : SMLGCTYPE =
struct

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

end

structure SMLGCMap = GCMap(SMLGCType)
