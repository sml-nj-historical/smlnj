(*
 *  User definable annotations.
 *
 *  Note: annotations will now be used extensively in all part of
 *  the optimizer.
 *
 *  Idea is stolen from Stephen Weeks
 * 
 *  -- Allen
 *)

signature ANNOTATIONS =
sig
   
   type annotation  = exn
   type annotations = annotation list

   (*
    * Generate a new annotation
    *)
   val new : unit -> { get : annotations -> 'a option,
                       put : 'a * annotations -> annotations,
                       rmv : annotations -> annotations
                     }

   (*
    * Extract an annotation value from an annotation list 
    *)
   val get : (annotation -> 'a option) -> annotations -> 'a option
   val rmv : (annotation -> bool) -> annotations -> annotations
   val put : annotation * annotations -> annotations

   (*
    * Pretty print an annotation
    *) 
   val toString : annotation -> string

   (*
    * Register new a pretty printer.
    * The pretty printer should raises an exception (any) if it cannot
    * handle the annotation given.
    *)
   val attachPrettyPrinter : (annotation -> string) -> unit

end
