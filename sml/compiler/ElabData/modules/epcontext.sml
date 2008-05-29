(* Copyright 1996 by AT&T Bell Laboratories *)
(* epcontext.sml *)

signature ENT_PATH_CONTEXT =
sig

  type context

  val initContext : context
  val isEmpty : context -> bool
  val enterOpen : context * EntPath.entVar option -> context
  val enterClosed : context -> context
  val lookTycEntPath : context * ModuleId.tycId -> EntPath.entPath option
  val lookStrEntPath : context * ModuleId.strId -> EntPath.entPath option
  val lookFctEntPath : context * ModuleId.fctId -> EntPath.entPath option
  val bindTycEntVar : context * ModuleId.tycId * EntPath.entVar -> unit
  val bindStrEntVar : context * ModuleId.strId * EntPath.entVar -> unit
  val bindFctEntVar : context * ModuleId.fctId * EntPath.entVar -> unit
  val bindTycEntPath : context * ModuleId.tycId * EntPath.entPath -> unit
(*
  val bindStrEntPath : context * ModuleId.strId * EntPath.entPath -> unit
  val bindFctEntPath : context * ModuleId.fctId * EntPath.entPath -> unit
*)
(* bindStrEntPath and bindFctEntPath not used -- should remove *)

end  (* signature ENT_PATH_CONTEXT *)


structure EntPathContext :> ENT_PATH_CONTEXT =
struct
 
local structure ST = Stamps
      structure EP = EntPath
      structure MI = ModuleId
in

type pathmap = EP.rEntPath MI.umap
val emptyPathMap = MI.emptyUmap

(* 
 * A structure body (struct decls end) is "closed" if 
 *    it is a functor body structure
 * The idea is that the elements of a closed structure are not
 * directly referenced from outside the structure, so the pathmap
 * local to the closed structure can be discarded after the structure
 * body is elaborated.
 *)

(* pathmap maps stamps to full entPaths relative to current functor context *)
(* each "closed" structure body pushes a new layer *)
(* INVARIANT: bindContext is the reverse of lookContext *)
datatype context
  = EMPTY
  | LAYER of {locals: pathmap ref, 
              lookContext: EP.entPath,
              bindContext: EP.rEntPath,
              outer: context}

val initContext : context = EMPTY

fun isEmpty(EMPTY : context) = true
  | isEmpty _ = false

(*
 * enterClosed : context -> context 
 * called on entering a closed structure scope, whose elements will not
 * be accessed from outside (hence the null bindContext) 
 *)
fun enterClosed epc = 
  LAYER {locals=ref(emptyPathMap), lookContext=EP.epnil,
         bindContext=EP.repnil, outer=epc}

(*
 * enterOpen : context * entVar option -> context
 * called on entering an open structure scope (claim: this is always an
 * unconstrained structure decl body), where ev is the entVar of the
 * structure being elaborated (if second arg is SOME(ev)). But under
 * what circumstances is the second argument NONE?
 *)
fun enterOpen (EMPTY, _) = EMPTY
  | enterOpen (epc, NONE) = epc
  | enterOpen (LAYER{locals,lookContext,bindContext,outer}, SOME ev) = 
      LAYER{locals=locals, lookContext=lookContext@[ev],
            bindContext=EP.repcons (ev, bindContext), outer=outer}

(* relative: entPath * entPath -> entPath *)
(* relative(path,ctx) - subtract common prefix of a path and a lookContext
 * path from the path *)
fun relative([],_) = []
  | relative(ep,[]) = ep
  | relative(p as (x::rest),y::rest') = 
      if EP.eqEntVar(x,y) then relative(rest,rest') else p

(* lookPath: (pathmap * 'a -> rEntPath option) -> (context * 'a) *)
fun lookPath look (EMPTY, _) = NONE
  | lookPath look (LAYER { locals, lookContext, bindContext, outer }, id) =
    (case look (!locals, id) of
	 NONE => lookPath look (outer, id)
       | SOME rp => SOME (relative (EP.rep2ep rp, lookContext)))

val lookTycEntPath : pathmap * MI.tycId -> EP.rEntPath = lookPath MI.uLookTyc
val lookStrEntPath : pathmap * MI.strId -> EP.rEntPath = lookPath MI.uLookStr
val lookFctEntPath : pathmap * MI.fctId -> EP.rEntPath = lookPath MI.uLookFct

(* probe: (pathmap * 'a -> rEntPath option) -> bool *)
(* probe(ctx,s) checks whether a statId is bound in the context *)
fun probe look (EMPTY, s) = false
  | probe look (LAYER{locals, outer, ...}, s) = 
      (case look(!locals, s) of
	   NONE => probe look (outer, s)
         | _ => true)

(* bindPath: (pathmap * 'a -> rEntPath option) * (pathmap * 'a * rEntPath -> bool)
             -> (context * 'a * entVar) -> unit *)
fun bindEntVar (look, insert) (EMPTY, _, _) = ()  (* should this be an exception? *)
  | bindPath (look, insert) (xx as LAYER { locals, bindContext, ... }, s, ev) =
    if probe look (xx, s) then ()
    else (locals := insert (!locals, s, EP.repcons (ev, bindContext)))

val bindTycEntVar = bindEntVar (MI.uLookTyc, MI.uInsertTyc)
val bindStrEntVar = bindEntVar (MI.uLookStr, MI.uInsertStr)
val bindFctEntVar = bindEntVar (MI.uLookFct, MI.uInsertFct)

(* bindLongPath: (pathmap * 'a -> rEntPath option) * (pathmap * 'a * rEntPath -> bool)
                 -> (context * 'a * entPath) -> unit *)
fun bindEntPath (look, insert) (EMPTY, _, _) = ()
  | bindLongPath (look, insert)
		 (xx as LAYER { locals, bindContext, ... }, s, ep) =
    if probe look (xx, s) then ()
    else (locals := insert (!locals, s, EP.ep2rep (ep, bindContext)))

val bindTycEntPath = bindEntPath (MI.uLookTyc, MI.uInsertTyc)
(* not used:
val bindStrEntPath = bindEntPath (MI.uLookStr, MI.uInsertStr)
val bindFctEntPath = bindEntPath (MI.uLookFct, MI.uInsertFct)
*)

end (* local *)
end (* structure EntPathContext *)
