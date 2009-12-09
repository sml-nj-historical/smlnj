(* Copyright 1996 by AT&T Bell Laboratories *)
(* epcontext.sml *)

signature ENT_PATH_CONTEXT =
sig

  type context

  val initContext : context
  val isEmpty : context -> bool
  val enterOpen : context * EntPath.entVar -> context
  val enterClosed : context -> context
  val lookTycEntPath : context * ModuleId.tycId -> EntPath.entPath option
  val lookStrEntPath : context * ModuleId.strId -> EntPath.entPath option
  val lookFctEntPath : context * ModuleId.fctId -> EntPath.entPath option
  val bindTycEntVar : context * ModuleId.tycId * EntPath.entVar -> unit
  val bindStrEntVar : context * ModuleId.strId * EntPath.entVar -> unit
  val bindFctEntVar : context * ModuleId.fctId * EntPath.entVar -> unit
  val bindTycEntPath : context * ModuleId.tycId * EntPath.entPath -> unit

end  (* signature ENT_PATH_CONTEXT *)


structure EntPathContext :> ENT_PATH_CONTEXT =
struct
 
local structure ST = Stamps
      structure EP = EntPath
      structure MI = ModuleId
in

(* pathmap: a record of five functional finite maps from static identifiers (for
 * tycons, signatures, structures, functors, and static envs) *)
type pathmap = EP.entPath MI.umap
val emptyPathMap: pathmap = MI.emptyUmap

(* 
 * A structure body (struct decls end) is "closed" if it is a functor
 * body structure.
 * The idea is that the elements of a closed structure are not
 * directly referenced from outside the structure, so the pathmap
 * local to the closed structure can be discarded after the structure
 * body is elaborated.
 *)

(* pathmap maps modIds to full entPaths relative to current functor context *)
(* each "closed" structure body pushes a new layer, with empty pathmap and 
 * context entPath. *)
datatype context
  = EMPTY     (* outside of any functor *)
  | LAYER of  (* inside of at least one functor *)
     {locals: pathmap ref,   (* maps local entities to their entPaths *)
      context: EP.entPath,   (* entPath to current (named) structure *)
      outer: context}


val initContext : context = EMPTY

fun isEmpty(EMPTY : context) = true
  | isEmpty _ = false

(*
 * enterClosed : context -> context 
 * called on entering a closed structure scope, whose elements will not
 * be accessed from outside (hence the null context).
 * called once in elabmod.sml (elabFct(BaseFct)) when entering a functor
 * body.
 * Also used once in elabsig.sml when elaborating a datatype/withtype spec. 
 *)
fun enterClosed epc = 
    LAYER {locals=ref(emptyPathMap), context=EP.epnil, outer=epc}

(*
 * enterOpen : context * entVar -> context
 * called on entering an open structure scope (claim: this is always an
 * unconstrained structure decl body), where ev is the entVar of the
 * structure being elaborated.
 *)
fun enterOpen (EMPTY, _) = EMPTY  (* not in a functor, don't need entPaths *)
  | enterOpen (LAYER{locals,context,outer}, ev) =
      (* entering a "named" structure somewhere within a functor.
       * we add the entVar for the structure to the end of the context *)
      LAYER{locals=locals, context=context@[ev], outer=outer}

(* relative: entPath * entPath -> entPath *)
(* relative(path,ctx) - subtract common prefix of a path and a context
 * path from the path *)
fun relative([],_) = []    (* can this, should this, ever happen? *)
  | relative(ep,[]) = ep
  | relative(p as (x::rest),y::rest') = 
      if EP.eqEntVar(x,y) then relative(rest,rest') else p

(* lookPath: (pathmap * 'a -> rEntPath option) -> (context * 'a) -> entPath option *)
(* generic lookup function for pathmaps *)
fun lookPath look (EMPTY, _) = NONE
  | lookPath look (LAYER { locals, context, outer }, id) =
    (case look (!locals, id)
       of NONE => lookPath look (outer, id)
	| SOME ep => SOME (relative (ep, context)))

(* used in moduleutil.sml, elabmod.sml, elabsig.sml *)
val lookTycEntPath : context * MI.tycId -> EP.entPath option = 
    lookPath MI.uLookTyc

(* used in elabmod.sml and elabsig.sml *)
val lookStrEntPath : context * MI.strId -> EP.entPath option = 
    lookPath MI.uLookStr

(* used only in elabmod.sml *)
val lookFctEntPath : context * MI.fctId -> EP.entPath option = 
    lookPath MI.uLookFct

(* probe: (pathmap * 'a -> rEntPath option) -> bool *)
(* probe(ctx,s) checks whether a statId is bound in the context.
 * slightly more efficient than using lookPath because it doesn't call relative *)
fun probe look (EMPTY, id) = false
  | probe look (LAYER{locals, outer, ...}, id) = 
      (case look(!locals, id)
	of NONE => probe look (outer, id)
         | _ => true)

(* bindEntVar: (pathmap * 'a -> entPath option) * (pathmap * 'a * entPath -> bool)
               -> (context * 'a * entVar)
	       -> unit *)
(* generic binder function for adding items to a pathmap *)
fun bindEntVar (look, insert) (EMPTY, _, _) = ()  (* should this be an exception? *)
  | bindEntVar (look, insert) (xx as LAYER { locals, context, ...}, id, ev) =
    if probe look (xx, id) then ()
    else locals := insert (!locals, id, context@[ev])

(* used in elabmod.sml, elabsig.sml, and evalent.sml *)
val bindTycEntVar = bindEntVar (MI.uLookTyc, MI.uInsertTyc)

(* following used only in elabmod.sml *)
val bindStrEntVar = bindEntVar (MI.uLookStr, MI.uInsertStr)
val bindFctEntVar = bindEntVar (MI.uLookFct, MI.uInsertFct)

(* bindTycEntPath: (context * tycId * entPath) -> unit *)
(* used only in evalent.sml *)
fun bindTycEntPath (EMPTY, _, _) = ()
  | bindTycEntPath (epc as LAYER { locals, context, ... }, tycId, ep) =
    if probe MI.uLookTyc (epc, tycId) then ()
    else locals := MI.uInsertTyc (!locals, tycId, context@ep)

end (* local *)
end (* structure EntPathContext *)
