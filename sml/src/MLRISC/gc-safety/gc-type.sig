(*
 * Abstract interface for GC types.
 *)
signature GC_TYPE =
sig

   type objtype 
   type gctype 

   val CONST  : int -> gctype                   (* integer constant *)
   val NONREF : objtype ref -> gctype           (* non-reference value *)  
   val REF    : objtype ref -> gctype           (* a reference to a gc object *)
   val ADD    : int * gctype * gctype -> gctype (* address arithmetic *)
   val SUB    : int * gctype * gctype -> gctype (* address arithmetic *)
   val BOT    : gctype
   val TOP    : gctype

   val ==     : gctype * gctype -> bool
   val join   : gctype * gctype -> gctype
   val meet   : gctype * gctype -> gctype

   val toString : gctype -> string

end
