signature SMLGCTYPE =
sig

   structure CPS : CPS
   type objtype = CPS.cty 
  
   datatype gctype =
     CONST of int                  (* integer constant *)
   | NONREF of objtype ref         (* non-reference value *)
   | REF of objtype ref            (* a reference, pointer to a gc object *)
   | ADD of int * gctype * gctype  (* address arithmetic + *)
   | SUB of int * gctype * gctype  (* address arithmetic - *)
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
   val PTR      : gctype  (* tagged ML objects *)

end

