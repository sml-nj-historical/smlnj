signature SMLGCTYPE =
sig

   structure CPS : CPS
   type ty = int

   datatype gctype =
     CONST of int                  (* integer constant *)
   | NONREF of CPS.cty ref         (* non-reference value *)
   | REF of CPS.cty ref            (* a reference, pointer to a gc object *)
   | PLUS of ty * gctype * gctype  (* address arithmetic + *)
   | MINUS of ty * gctype * gctype (* address arithmetic - *)  
   | ALLOCPTR                      (* SML/NJ allocation pointer *)
   | BOT
   | TOP

   val ==       : gctype * gctype -> bool
   val join     : gctype * gctype -> gctype
   val meet     : gctype * gctype -> gctype

   val toString : gctype -> string

   (*
    * Primitive types 
    *)
   val I31      : gctype  (* tagged integers *)
   val I32      : gctype  (* untagged integers *)

   val REAL64   : gctype  (* unboxed real *)
   val REAL32   : gctype  (* unused *)
   val PTR      : gctype  (* tagged ML objects *)
   val INT      : gctype  (* machine integers aka I32 *)
   val ADD      : ty * gctype * gctype -> gctype
   val SUB      : ty * gctype * gctype -> gctype

   val isRecoverable : gctype -> bool

end

