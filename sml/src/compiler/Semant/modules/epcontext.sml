(* Copyright 1996 by AT&T Bell Laboratories *)
(* epcontext.sml *)

signature ENT_PATH_CONTEXT =
sig

  type context

  val initContext : context
  val isEmpty : context -> bool
  val enterOpen : context * EntPath.entVar option -> context
  val enterClosed : context -> context
  val lookPath : context * ModuleId.modId -> EntPath.entPath option
  val bindPath : context * ModuleId.modId * EntPath.entVar -> unit
  val bindLongPath : context * ModuleId.modId * EntPath.entPath -> unit

end  (* signature ENT_PATH_CONTEXT *)


structure EntPathContext :> ENT_PATH_CONTEXT =
struct

local structure ST = Stamps
      structure EP = EntPath
      structure MI = ModuleId
in

structure Key = 
  struct 
    type ord_key = MI.modId
    val cmpKey = MI.cmp
  end
   
structure D = BinaryDict(Key)

type entPathR = EP.entVar list  
type pathmap = entPathR D.dict 

(* 
 * A structure body (struct decls end) is "closed" if 
 *    it is a functor body structure
 * The idea is that the elements of a closed structure are not
 * directly referenced from outside the structure, so the pathEnv
 * local to the closed structure can be discarded after the structure
 * body is elaborated.
 *)

(* pathmap maps stamps to full entPaths relative to current functor context *)
(* each "closed" structure body pushes a new layer *)
datatype context
  = EMPTY
  | LAYER of {locals: pathmap ref, 
              lookContext: EP.entPath,
              bindContext: entPathR,
              outer: context}

val initContext : context = EMPTY

fun isEmpty(EMPTY : context) = true
  | isEmpty _ = false

(* 
 * called on entering a closed structure scope, whose elements will not
 * be accessed from outside (hence the null bindContext) 
 *)
fun enterClosed epc = 
  LAYER {locals=ref(D.mkDict()), lookContext=[],
         bindContext=[], outer=epc}

(*
 * called on entering an open structure scope (claim: this is always an
 * unconstrained structure decl body), where ev is the entVar of the
 * structure being elaborated.
 *)
fun enterOpen (EMPTY, _) = EMPTY
  | enterOpen (epc, NONE) = epc
  | enterOpen (LAYER{locals,lookContext,bindContext,outer}, SOME ev) = 
      LAYER{locals=locals, lookContext=lookContext@[ev],
            bindContext=ev::bindContext, outer=outer}

(* relative(path,ctx) - subtract common prefix of path and ctx from path *)
fun relative([],_) = []
  | relative(ep,[]) = ep
  | relative(p as (x::rest),y::rest') = 
      if EP.eqEntVar(x,y) then relative(rest,rest') else p

fun lookPath (EMPTY, _) = NONE
  | lookPath (LAYER{locals,lookContext,bindContext,outer}, id: MI.modId) 
          : entPathR option =
      (case D.peek(!locals,id) 
        of NONE => lookPath(outer,id)
         | SOME rp => SOME(relative(rev rp, lookContext)))

(* probe(ctx,s) checks whether a stamp has already be bound before *)
fun probe (EMPTY, s) = false
  | probe (LAYER{locals, outer, ...}, s) = 
      (case D.peek(!locals, s)
        of NONE => probe(outer, s)
         | _ => true)

fun bindPath (EMPTY, _, _) = ()
  | bindPath (xx as LAYER {locals, bindContext, ...}, s, ev) =
      if probe(xx, s) then () 
      else (locals := D.insert(!locals, s, ev::bindContext))

fun bindLongPath(EMPTY, _, _) = ()
  | bindLongPath(xx as LAYER {locals, bindContext, ...}, s, ep) = 
      let fun h(a::r, p) = h(r, a::p)
            | h([], p) = p
       in if probe(xx, s) then ()
          else (locals := D.insert(!locals, s, h(ep,bindContext)))
      end

(*
(*** top leve wrappers: used for profiling the compilation time *)
val lookPath =
  Stats.doPhase (Stats.makePhase "Compiler 034 x-lookPath") lookPath
val bindPath =
  Stats.doPhase (Stats.makePhase "Compiler 034 x-bindPath") bindPath
val bindLongPath =
  Stats.doPhase (Stats.makePhase "Compiler 034 y-bindPath") bindLongPath
*)

end (* local *)
end (* structure EntPathContext *)

(*
 * $Log: epcontext.sml,v $
 * Revision 1.6  1997/09/23  03:53:36  dbm
 *   Rewrite for EntityEnv.Unbound fix (bugs 1270, 1271, etc.)
 *
 * Revision 1.5  1997/08/28  12:37:08  jhr
 *   Replaced insertNovwt witn insertSp [zsh]
 *
 * Revision 1.4  1997/08/22  18:37:41  george
 *   Add several new functions and simplifications to fix bug 1234, 1206, etc.
 *                                                              --- zsh
 *
 * Revision 1.2  1997/01/31  20:40:05  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:42  george
 *   Version 109.24
 *
 *)
