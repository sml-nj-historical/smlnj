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
   
   type annotation 
   type annotations = annotation list
   type propList = annotations
 
   exception NoProperty

   type 'a property = 
        { get      : annotations -> 'a option,
          peek     : annotation  -> 'a option,
          lookup   : annotations -> 'a,
          contains : annotations -> bool,
          set      : 'a * annotations -> annotations,
          rmv      : annotations -> annotations,
          create   : 'a -> annotation
        }
   type flag = unit property

   (*
    * Generate a new annotation.
    * Client should provide a pretty printing function.
    *)
   val new : ('a -> string) option -> 'a property
   val newFlag : string -> flag

   (*
    * Pretty print an annotation
    *) 
   val toString : annotation -> string

end
