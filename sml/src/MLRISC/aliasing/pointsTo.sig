(*
 *  This module can be used to perform points-to analysis for typed languages
 *
 * -- Allen
 *)
signature POINTS_TO =
sig

   eqtype edgekind 

   datatype cell = 
     LINK  of region                             
   | SREF  of int * edges ref
   | WREF  of int * edges ref
   | SCELL of int * edges ref
   | WCELL of int * edges ref
   | TOP   of {mutable:bool, id:int, name:string}
      (* a collapsed node *)
   withtype region = cell ref
   and      edges  = (edgekind * int * region) list

   val reset    : (unit -> int) -> unit

   (* generate a new reference/immutable cell *)
   val newSRef  : unit -> region  
   val newWRef  : unit -> region  
   val newSCell : unit -> region  
   val newWCell : unit -> region  

   (* generate a new collapsed node *)
   val newTop   : {mutable:bool,name:string} -> region  

   (*  
    * The following are methods for constructing the storage shape graph.
    *)
   val pi     : region * int -> region (* the ith projection *)
   val sub    : region * int -> region (* the ith subscript *)
   val dom    : region * int -> region (* the ith domain *)
   val ran    : region * int -> region (* the ith range *)
   val offset : region * int -> region (* the ith offset *)

   val unify     : region * region -> unit 
   val interfere : region * region -> bool (* do they interfere? *) 

   (*   
    * More complex methods
    *)
   val mkRecord : region option * region list -> region    
   val mkRef    : region option * region -> region        
   val mkArray  : region option * region list -> region
   val mkVector : region option * region list -> region
   val mkLambda : region list -> region (* define a function *)

   val app      : region * region list -> unit (* apply a function *)
   val ret      : region * region list -> unit (* binds the return values *)

   val strongUpdate    : region * int * region -> unit
   val strongSubscript : region * int -> region 
   val weakUpdate      : region * region -> unit
   val weakSubscript   : region -> region

   val toString  : region -> string

end
