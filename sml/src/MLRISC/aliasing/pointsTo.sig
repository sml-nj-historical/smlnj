(*
 *  This module can be used to perform points-to analysis for
 *  a typed language.   It is inspired by Nevin's stuff, but for compile
 *  time-efficiency reasons we are using a unification based scheme.
 *  Closure based schemes seem to be too expensive to be practical.
 *
 * -- Allen
 *)
signature POINTS_TO =
sig

   datatype kind = PI     (* projection *)
                 | DOM    (* domain *)
                 | RAN    (* co-domain (note: doesn't apply in CPS) *)

   datatype cell = 
     LINK of loc                               (* union/find up link *)
   | REF of int * (kind * int * loc) list ref  (* reference node *)
   | TOP of int                                (* a collapsed node *)
   | NAMED of string * loc

   withtype loc = cell ref

   val reset  : (unit -> int) -> unit

   val newRef : 'a -> loc  (* generate a new reference *)
   val newTop : 'a -> loc  (* generate a new collapsed node *)

   (*  
    * The following are methods for constructing the storage shape graph.
    *)
   val pi     : loc * int -> loc  (* the ith projection *)
   val dom    : loc * int -> loc  (* the ith domain *)
   val ran    : loc * int -> loc  (* the ith range *)
   val offset : loc * int -> loc  (* the ith offset *)

   val unify  : loc * loc -> unit (* unify two locations *)

   (*   
    * More complex methods
    *)
   val record : loc list -> loc        (* allocate a record *)
   val mkref  : loc -> loc             (* allocate a reference *)
   val app    : loc * loc list -> unit (* apply a function *)
   val ret    : loc * loc list -> unit (* binds the return values *)

   (*
    * Pretty printing
    *)
   val toString : loc -> string

end
