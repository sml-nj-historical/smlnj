(* Copyright 1989 by AT&T Bell Laboratories *)
(* environ.sml *)

structure Environment: ENVIRONMENT =
struct

local structure A = Access
      structure S  = Symbol
      structure M  = Modules
      structure V = VarCon
      structure T = Types
      structure MU = ModuleUtil
      structure B  = Bindings
      structure SE = StaticEnv
      structure DE = DynamicEnv
      structure SY = SymbolicEnv
in

type symbol = S.symbol
type staticEnv = SE.staticEnv
type dynenv  = DE.dynenv
type symenv = SY.symenv

type environment = { static: staticEnv, dynamic: dynenv, symbolic: symenv }

fun bug msg = ErrorMsg.impossible("Environment: "^msg)

fun staticPart (e: environment) = #static e
fun dynamicPart (e: environment) = #dynamic e
fun symbolicPart (e: environment) = #symbolic e
      
fun mkenv (e as { static, dynamic, symbolic }) = e

val emptyEnv = {static   = SE.empty,
		dynamic  = DE.empty,
		symbolic = SY.empty}

fun layerEnv({static, dynamic, symbolic},
	       {static=sta, dynamic=dy, symbolic=sy}) =
      {static =  SE.atop (static, sta),
       dynamic = DE.atop (dynamic, dy),
       symbolic = SY.atop (symbolic, sy)}
  
val layerStatic = SE.atop
val layerSymbolic = SY.atop
  
fun consolidateEnv ({ static, dynamic, symbolic }) =
      {static = SE.consolidate static,
       dynamic = DE.consolidate dynamic,
       symbolic = SY.consolidate symbolic}

val consolidateStatic = SE.consolidate
val consolidateSymbolic = SY.consolidate

fun root(A.EXTERN pid) = SOME pid 
  | root(A.PATH(p,i)) = root p
  | root _ = NONE

(* getting the stamp from a binding *)
fun stampOf(B.VALbind(V.VALvar {access=a, ...})) = root a
  | stampOf(B.CONbind(T.DATACON {rep=A.EXN a, ...})) = root a
  | stampOf(B.STRbind(M.STR {access=a, ...})) = root a
  | stampOf(B.FCTbind(M.FCT {access=a, ...})) = root a
  | stampOf _ = NONE


(* functions to collect stale dynamic pids for unbinding in concatEnv *)

(* 
 * stalePids: takes a new environment and a base environment to which
 * it is to be added and returns a list of pids that are unreachable 
 * when the new environment is added to the base environment
 *
 * what we do instead:
 *  - count the number of occurences for each pid in baseEnv bindings
 *    that is going to be shadowed by deltaEnv
 *  - count the total number of total occurences for each such
 *    pids in baseEnv
 *  - the ones where the counts coincide are stale
 *
 * This code is ok, because deltaEnv is the output of `export'.  `export'
 * calls consolidateStatic, therefore we don't have duplicate bindings
 * of the same symbol.
 *)
fun stalePids (deltaEnv, baseEnv) = 
  let 

      (* any rebindings? *)
      val anyrebound = ref false

      (* counting map *)
      val countM = ref (PersMap.empty: int ref PersMap.map)
      fun look s = 
            SOME (PersMap.lookup (!countM) s) handle PersMap.MapF => NONE

      (* initialize the counter map: for each new binding with stamp
       * check if the same symbol was bound in the old env and enter
       * the old stamp into the map *)
      fun initOne s =
        case look s 
         of NONE => countM := PersMap.add (!countM, s, ref (~1))
          | SOME r => r := (!r) - 1

      fun initC (sy, _) =
	  (case stampOf (SE.look (baseEnv, sy))
	     of NONE => ()
	      | SOME s => (initOne s; anyrebound := true))
	  handle SE.Unbound => ()
      (* increment counter for a given stamp *)
      fun incr NONE = ()
	| incr (SOME s) = 
 	   case look s 
             of NONE => ()
 	      | SOME r => r := (!r) + 1

      fun incC (_, b) = incr (stampOf b)
      (* select the 0s *)
      fun selZero ((s, ref 0), zeros) = s :: zeros
	| selZero (_, zeros) = zeros
   in
      SE.app initC deltaEnv;		(* init counter map *)
      if !anyrebound then let		(* shortcut if no rebindings *)
	  (* count the pids *)
	  val _ = SE.app incC baseEnv
	  (* pick out the stale ones *)
	  val stalepids = foldl selZero [] (PersMap.members (!countM))
      in
	  stalepids
      end
      else []
  end

fun concatEnv ({ static = newstat, dynamic = newdyn, symbolic = newsym },
		 { static = oldstat, dynamic = olddyn, symbolic = oldsym }) =
  let val hidden_pids = stalePids (newstat, oldstat)
      val slimdyn = DE.remove (hidden_pids, olddyn)
      val slimsym = SY.remove (hidden_pids, oldsym)
   in {static=SE.consolidateLazy(SE.atop(newstat, oldstat)),
       dynamic=DE.atop(newdyn, slimdyn),
       symbolic=SY.atop(newsym, slimsym)}
  end

fun getbindings(static: staticEnv, symbols: S.symbol list) :
        (S.symbol * B.binding) list =
  let fun loop([], bindings) = bindings
        | loop(s::rest, bindings) =
            let val bindings' = (s,SE.look(static,s)) :: bindings
				  handle SE.Unbound => bindings
	     in loop (rest, bindings') 
            end
   in loop(symbols,[])
  end

fun copystat([], senv) = senv
  | copystat((s,b)::l, senv) = copystat(l,SE.bind(s, b, senv))

fun filterStaticEnv(static: staticEnv, symbols: S.symbol list) : staticEnv =
      copystat(getbindings(static, symbols), SE.empty)

fun filterEnv({static, dynamic, symbolic}: environment, symbols) =
  let val sbindings = getbindings (static, symbols)
      fun copydynsym ([], denv, syenv) = (denv, syenv)
	| copydynsym ((_, b) :: l, denv, syenv) =
	      (case stampOf b
		of NONE => copydynsym (l, denv, syenv)
		 | SOME pid =>
		     let val dy = DE.look dynamic pid
		         val denv = DE.bind (pid, dy, denv)
		         val sy = SY.look symbolic pid
		         val syenv = case sy
		                      of NONE => syenv
				       | SOME sy => SY.bind (pid, sy, syenv)
		      in copydynsym (l, denv, syenv)
		     end)
      val senv = copystat(sbindings, SE.empty) 
      val (denv, syenv) = copydynsym(sbindings, DE.empty, SY.empty)
   in {static =senv, dynamic = denv, symbolic = syenv}
  end

fun catalogEnv static : S.symbol list = map #1 (SE.sort static)

(* CM-style environment lookup *)
datatype cmEnv
  = CM_NONE
  | CM_ENV of {look : S.symbol -> cmEnv, 
               symbols : unit -> S.symbol list}

exception CmEnvOfModule

fun lookElems elements sym =
      (case MU.getSpec(elements,sym)
         of M.STRspec{sign,...} => sigenv sign
          | M.FCTspec{sign,...} => fsgenv sign
          | _ => CM_NONE)
      handle MU.Unbound _ => CM_NONE

and sigenv (s as M.SIG{elements, ...}) = 
      CM_ENV {look = lookElems(elements), 
              symbols = (fn () => MU.getSigSymbols s)}
  | sigenv _ = CM_NONE

(*
 * The following is a hack to make the cmEnvOfModule function consistent
 * with the changes made on ast during the elaboration of ast into absyn.
 * Syntactic changes made on ast by the elaborator should be propagated
 * to this function so that CM can do the correct job. I personally think 
 * that syntactic changes on curried functors and insertions of <resultStr>s 
 * should be done on Ast directly, before the elaboration --- this way, we 
 * don't have to write the following ugly sigenvSp function. 
 * 
 *)
and sigenvSp (M.SIG{elements=[(sym,M.STRspec{sign,...})],...}) = 
      if S.name sym = "<resultStr>" then sigenv sign
      else bug "unexpected case <resultStr> in sigenvSp"
  | sigenvSp (M.SIG{elements=[(sym,M.FCTspec{sign,...})],...}) = 
      if S.name sym = "<functor>" then fsgenv sign
      else bug "unexpected case <functtor> in sigenvSp"
  | sigenvSp _ = bug "unexpected case in signenvSp"

and fsgenv (M.FSIG{bodysig,...}) = sigenvSp bodysig
  | fsgenv _ = CM_NONE

fun strenv(M.STR{sign,...}) = sigenv sign
  | strenv M.ERRORstr = CM_NONE

fun fctenv(M.FCT{sign,...}) = fsgenv sign
  | fctenv _ = CM_NONE

fun cmEnvOfModule env sym =
    (case SE.look(env,sym)
       of B.SIGbind b => sigenv b
        | B.STRbind b => strenv b
        | B.FSGbind b => fsgenv b
        | B.FCTbind b => fctenv b
        | _ => CM_NONE)
    handle SE.Unbound => CM_NONE

fun describe static (s: symbol) : unit =
      let open PrettyPrint
       in with_pp (ErrorMsg.defaultConsumer())
	   (fn ppstrm =>
	    (begin_block ppstrm CONSISTENT 0;
	     PPModules.ppBinding ppstrm
	       (s, SE.look(static,s), static, !Control.Print.printDepth);
	     add_newline ppstrm;
	       end_block ppstrm))
      end handle SE.Unbound => print (S.name s ^ " not found\n")

val primEnv = PrimEnv.primEnv

end (* local *)
end (* structure Environment *)


(*
 * $Log: environ.sml,v $
 * Revision 1.3  1997/12/03 05:10:22  dbm
 *   Fix for spurious error messages (e.g. testing/modules/tests/228.sml)
 *   caused by CM autoloading.  cmEnvOfModule changed to bypass <resultStr>
 *   and <functor> special bindings.
 *
 * Revision 1.2  1997/08/15  20:39:53  dbm
 *   Use new consolidateLazy in place of consolidate to reduce top-level
 *   loop overhead.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)
