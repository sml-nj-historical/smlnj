(*
 *  User definable annotations.
 *
 *  Note: annotations will now be used extensively in all part of
 *  the optimizer.
 *
 *  Idea is stolen from Stephen Weeks
 *)

signature ANNOTATIONS =
sig
   
   type annotation  = exn
   type annotations = annotation list

   (*
    * The only predefined annotation is comment, which has no semantics.
    * The user has to generate other annotations using exception
    * declarations, or use the 'new' function below.
    *)
   exception COMMENT of string  

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

end

(*
 * $Log: annotations.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:55  george
 *   Version 110.10
 *
 *) 
