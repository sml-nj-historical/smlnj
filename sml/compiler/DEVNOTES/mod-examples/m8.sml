(* 
 * This program exercises the deBruijn indexing of formal type parameters.
 * Because the functor application is nested inside of a functor (i.e., 
 * in the functor context), the deBruijn index must be bumped up. 
 * If the depth is not properly kept, then transtypes may incorrectly
 * assign an unbound deBruijn index.
 *)

functor RealConst(R : sig val sub : 'a array -> 'a end) = 
struct

structure DFA = DynamicArrayFnQ(
  struct 
    val sub = R.sub
    type elem = unit->unit
    type array = elem array
  end)

    fun access(arr) = (DFA.sub(arr)) ()

end (* functor RealConst *)

