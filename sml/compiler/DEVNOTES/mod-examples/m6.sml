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

functor F (A : sig
    type t
    type elem
    val sub      : t -> elem
  end) 
 =
struct
  (* val sub = A.sub *)
end (* DynamicArrayFn *)

(* functor RealConst(R : sig val sub : 'a list -> 'a end) =
struct 
 
structure M = F(
  struct
    val sub = R.sub
    type elem = unit->unit
    type t = elem list
  end)

  fun access(arr) = (M.sub(arr)) ()

end (* functor RealConst *)*)

