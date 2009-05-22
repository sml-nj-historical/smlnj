(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* debindex.sml *)

(* 
 * This implements the abstraction of de Bruijn indices used
 * by the FLINT type and term language. The notion of depth
 * refers to the type-binding depth relative to the top level
 * of the current compilation unit. I can't make type depth 
 * and index abstract because certain clients want to use 
 * the values of these types as table indices.
 *)

(* I moved this into the elaborator library.  It may be moved
 * back to FLINT if the elaborator gets "cleaned up", i.e., if
 * it is made to be unaware of such middle-end specifics.
 * (08/2001 Blume) *)
(* [DBM 20/05/09] No longer used in elaborator, so moved into FLINT/trans.
 * Consider eliminating and just using ints and arithmetic. *)

(* Basic PLambda type variables are pairs of indices: (index, count),
 * where index (the deBruijn index) is the normal lambda binding distance
 * from the current type variable to its binder, starting with 1 to reference
 * the innermost binder.  Each binder binds a tuple of variables, and
 * the count, which is zero-based, is used to select from this tuple.
 * Thus if the innermost abstraction binds three type variables (t1,t2,t3),
 * then a reference to t2 is translated to (1,1).
 *
 * depth is used to represent absolute type abstraction depth, with the top-level
 * being 0. The deBruijn index is calculated as the current abstraction depth
 * minus the abstraction depth of the binder of the type variable in question.
 * [But what is the "abstraction depth" of a binder?  Is it the nesting depth of
 * that binder?  Starting with 0 or 1?]
 *
 * The abstraction depth of a binder depth is the then current depth
 * just outside of that abstraction, i.e. the number of enclosing
 * abstractions. For example, the outermost binder will have depth=0.
 * and the current depth is incremented when entering the the scope of
 * an abstraction. So the deBruijn index of any bound type variable occurrence
 * is >= 1 because it is inside at least one abstraction.
 *
 * Abstractions incrementing the depth can be either functor abstractions, where
 * the abstracted (FLINT) type variables represent the primaries of the functor
 * parameter signature, or polymorphic abstractions, where the abstracted FLINT type
 * variables correspond to the implicitl polymorphically bound type variables.
*)

structure DebIndex : DEB_INDEX = 
struct

local structure EM = ErrorMsg
in

fun bug s = EM.impossible ("DebIndex: " ^ s)

type depth = int  (* depth of type abstractions INVARIANT: 0 <= depth *)
     (* at top-level, outside any functors or polymorphic abstractions,
      * depth = 0. See defn of top below. *)

type index = int  (* position within the sequence of type variables bound by
                     an abstraction INVARIANT: 1 <= index *)

val top = 0  (* depth at top-level *)

fun next (i: depth): depth = i + 1

fun prev (i: depth): depth = if (i > 0) then i-1 else bug "negative depth in prev"

fun eq (i: depth, j: depth) = (i=j)

fun dp_key (i : depth) = i

fun dp_print i = Int.toString i

fun dp_toint (i : depth) = i
fun dp_fromint (i : int) = i

(* this returns an int n, n >= 0 *)
fun relativeDepth (useDepth: int, bindDepth: int) = 
  if bindDepth > useDepth then bug "the definition is deeper than the use"
  else (useDepth - bindDepth)

val cmp = Int.compare

fun di_key i = i

fun di_print i = Int.toString i

fun di_toint (i : index) = i
fun di_fromint (i : int) = i
fun di_inner i = i+1

val innermost = 1   (* ??? *)
val innersnd = 2    (* ??? *)

end (* local *)
end (* structure DebIndex *)
