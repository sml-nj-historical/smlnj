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

structure Annotations :> ANNOTATIONS =
struct

   type annotation = exn
   type annotations = annotation list
   type propList = annotations
   exception NoProperty
   type 'a property = 
         { get      : annotations -> 'a option,
           peek     : annotation -> 'a option,
           lookup   : annotations -> 'a,
           contains : annotations -> bool,
           set      : 'a * annotations -> annotations,
           rmv      : annotations -> annotations,
           create   : 'a -> annotation
         }
   type flag = unit property

   val prettyPrinters = ref [] : (annotation -> string) list ref 

   fun attachPrettyPrinter p = prettyPrinters := p :: !prettyPrinters

   fun toString a =
   let fun pr([]) = ""
         | pr(p::ps) = (p a handle _ => pr ps)
   in  pr(!prettyPrinters) end

   (*
    * Look ma, a real use of generative exceptions!
    *)
   fun 'a new(prettyPrinter) =
   let exception Annotation of 'a
       fun get [] = NONE
         | get (Annotation x::_) = SOME x
         | get (_::l) = get l
       fun peek(Annotation x) = SOME x
         | peek _ = NONE
       fun lookup [] = raise NoProperty
         | lookup (Annotation x::_) = x
         | lookup (_::l) = lookup l
       fun contains [] = false
         | contains (Annotation _::_) = true
         | contains (_::l) = contains l
       fun set(x,[]) = [Annotation x]
         | set(x,Annotation _::l) = Annotation x::l
         | set(x,y::l) = y::set(x,l)
       fun rmv [] = []
         | rmv (Annotation _::l) = rmv l
         | rmv (x::l) = x::rmv l
   in  case prettyPrinter of
         SOME f => attachPrettyPrinter(fn Annotation x => f x | e => raise e)
       | NONE => ();
       { get=get, peek=peek, lookup=lookup, contains=contains,
         set=set, rmv=rmv, create=Annotation
       }
   end
 
   fun newFlag ""   = new NONE
     | newFlag name = new(SOME(fn _ => name))

end

