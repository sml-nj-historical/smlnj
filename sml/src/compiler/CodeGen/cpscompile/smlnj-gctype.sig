signature SMLGCTYPE =
sig

   type objtype = CPS.cty 
  
   datatype gctype =
     CONST of int                  (* integer constant *)
   | NONREF of objtype ref         (* non-reference value *)
   | REF of objtype ref            (* a reference, pointer to a gc object *)
   | ADD of int * gctype * gctype  (* address arithmetic + *)
   | SUB of int * gctype * gctype  (* address arithmetic - *)
   | BOT
   | TOP

   val toString : gctype -> string

end

