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

structure DebIndex : DEB_INDEX = 
struct

local structure EM = ErrorMsg
in

fun bug s = EM.impossible ("DebIndex: " ^ s)

type depth = int  (* depth of type abstractions *)
type index = int  (* position within the sequence of type variables bound by
                     an abstraction *)

val top = 0

fun next i = i + 1

fun prev i = if (i > 0) then i-1 else bug "negative depth in prev"

fun eq (i:int, j) = (i=j)

fun dp_key (i : depth) = i

fun dp_print i = Int.toString i

fun dp_toint (i : depth) = i
fun dp_fromint (i : int) = i

fun calc (cur:int, def) = 
  if def > cur then bug "the definition is deeper than the use"
  else (cur - def)

val cmp = Int.compare

fun di_key i = i

fun di_print i = Int.toString i

fun di_toint (i : index) = i
fun di_fromint (i : int) = i

val innermost = 1
val innersnd = 2
fun di_inner i = i+1

end (* local *)
end (* structure DebIndex *)


