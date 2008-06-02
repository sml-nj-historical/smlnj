(* 
 * This test case addresses polymorphic functions instantiated to arrow types.
 * It may fail during FLINT normalization due to an extra nesting of the 
 * val binding type in a structure.  
 * Translate appears to produce a malformed tyc for F's parameter, 
 * particularly A.sub.
 * Translate (actually transtypes) produces TCFN instead of TVs 
 * for FORMAL GENtycs. The FORMAL was previously pre-translated 
 * into TVs in the Elaborator and the resulting TV was encoded 
 * in a FLEXTYC. 
 *)


functor RealConst((* R : sig val sub : 'a array -> 'a end *)) =
struct 
 
structure M = F(
  struct
    open Array
    (* val sub = R.sub*)
    type elem = unit->unit
    type t = elem array
  end)

  fun access(arr) = (M.sub(arr)) ()

end (* functor RealConst *)

