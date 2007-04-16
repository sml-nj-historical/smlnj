(* statenv.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure StaticEnv : STATICENV =
struct

local structure B  = Bindings
      structure E = Env
      structure M = Modules
in 

type binding = B.binding
type real_binding = binding * M.modtree option

(* [GK 4/16/07] 
   A symbol list is attached to each environment to keep a record 
   of the original order bindings are added to the static environment.
   This symbol list will be used to determine the order of printing 
   these bindings in an inferred signature (signature extracted from
   a struct...end with no ascribed signature) *)
type staticEnv = real_binding E.env * Symbol.symbol list

exception Unbound = E.Unbound

fun aug x = (x, NONE)
fun strip (rb: real_binding) = #1 rb

val empty = (E.empty, [])
fun look ((e,_), s) = strip (E.look (e, s))
fun bind0 (s, b, (e,names)) = (E.bind (s, b, e), names)
fun bind (s, b, (e,names)) = (E.bind (s, aug b, e), s::names)
fun special (mkb, mks) = (E.special (aug o mkb, mks), [])
fun atop((e,names),(e',names')) = (E.atop(e,e'), names @ names')
fun consolidate (e,names) = (E.consolidate e, names)
fun consolidateLazy (e,names) = (E.consolidateLazy e, names)
fun app f (e,names) = E.app (fn (s, b) => f (s, strip b)) e
fun map f (e,names) = (E.map (aug o f o strip) e, names)
fun fold f x0 (e,_) = E.fold (fn ((s, b), x) => f ((s, strip b), x)) x0 e

fun realfold f x0 (e,_) = E.fold f x0 e
fun symbols (e,_) = E.symbols e 

(* fold but only over the elements in the environment with the keys
   given in the key list (last parameter). This functions allows 
   us to compute folds in arbitrary order over a consolidated list.
   In particular, this function is currently used in extractSig in
   elabmod to keep the inferred signature specs in the same order as
   the original structure decls. 
 *)
fun foldOrigOrder(f, x0, (env, symOrder)) =
    let fun loop(f, x0, (env, [])) = x0
	  | loop(f, x0, senv as (env, elem::rest)) = 
	    loop(f, f((elem, look(senv,elem)), x0), (env, rest))
    in loop(f, x0, (env, rev symOrder))
    end  
(* 
 * sort: sort the bindings in an environment.
 *  
 * This is used for the assignment of dynamic access slots in structure
 * elaborate, for printing, and for other purposes.
 * The bindings are sorted in the following order:
 *
 *   values
 *   constructors
 *   types
 *   signatures
 *   structures
 *   funsigs
 *   functors
 *   fixity declarations
 *
 * It is only correct to sort environments which have no duplicate bindings.
 * All routines which build structure environments maintain this
 * invariant, so it is ok to sort any structure environment using
 * this function.
 *)

fun sort env = ListMergeSort.sort B.binderGt (fold (op ::) nil env)

fun filter (e, l) =
    let fun add (sy, e') = bind (sy, look (e, sy), e') handle Unbound => e'
    in foldl add empty l
    end

end (* local *)
end (* structure StaticEnv *)
