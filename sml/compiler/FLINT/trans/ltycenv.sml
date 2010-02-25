(* ltycenv.sml *)

(* environments mapping entity paths to PLambdaType.tyc,
 * used in modtypes.sml *)

structure LTtycEnv : ENTITY_ENV =
struct

local
  structure EP = EntPath
  structure ED = EntPath.EvDict
  structure ST = Stamps
  structure M = Modules
  structure T = Types
in

val say = Control_Print.say
val debugging = ElabDataControl.eedebugging (* ref false *)
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()
fun bug msg = ErrorMsg.impossible("EntityEnv: "^msg)

type entVar = EP.entVar
type entPath = EP.entPath

datatype ltycEnv 
  = BIND of LT.tyc EP.EvDict.map * ltycEnv
  | NIL

exception LtycEnvUnbound

val empty = NIL

fun bind(v, e, BIND(d, env)) = BIND(ED.insert(d, v, e), env)
  | bind(v, e, NIL) = BIND(ED.insert(ED.empty, v, e), env)

fun atop(e1, NIL) = e1
  | atop(BIND(d,e1),e2) = BIND(d,atop(e1,e2))
  | atop(NIL, e2) = e2

fun atopSp(e1, NIL) = e1
  | atopSp(NIL, e2) = e2
  | atopSp(BIND(d,e1),e2) = atopMerge(d,atop(e1,e2))

and atopMerge(d, NIL) = BIND(d, NIL)
  | atopMerge(d, BIND(d', e)) = BIND (ED.unionWith #1 (d,d'),e)

fun toList (BIND(d, ee)) = (*ED.fold((op ::), toList ee, d)*)
     ED.foldri (fn (key, value, base) => (key,value)::base) (toList ee) d
  | toList NIL = nil

fun look(env,v) =
    let fun scan(BIND(d, rest)) = 
              (case ED.find(d, v)
                of SOME e => e
                 | NONE => scan rest)
	  | scan NIL = 
	      (debugmsg ("$EE.look: didn't find "^EP.entVarToString v);
	       raise LtycEnvUnbound)
     in scan env
    end

fun lookStrEnt(entEnv,entVar) = 
    case look(entEnv,entVar)
     of M.STRent ent => ent
      | M.ERRORent => M.bogusStrEntity
      | _ => bug "lookStrEnt"

fun lookTycEnt(entEnv,entVar) = 
    case look(entEnv,entVar)
     of M.TYCent ent => ent
      | M.ERRORent => Types.ERRORtyc
      | _ => bug "lookTycEnt"

fun lookFctEnt(entEnv,entVar) = 
    case look(entEnv,entVar)
     of M.FCTent ent => ent
      | M.ERRORent => M.bogusFctEntity
      | _ => bug "lookFctEnt"

fun lookEP(entEnv,[]) = bug "lookEP.1"
  | lookEP(entEnv,[v]) = look(entEnv,v)
  | lookEP(entEnv,ep as (v::rest)) =
     (case look(entEnv,v)
	of M.STRent { entities, ... } => lookEP (entities,rest)
	 | M.ERRORent => M.ERRORent
	 | ent =>
	     (say "lookEnt.1: expected STRent\n";
	      say "found entity: ";
	      case ent
		of M.TYCent _ => say "TYCent\n"
		 | M.FCTent _ => say "FCTent\n"
		 | _ => say "ERRORent\n";
	      say "entpath: "; say (EP.entPathToString(ep)); say "\n";
	      bug "lookEnt.2"))

fun lookTycEP(entEnv,entPath) = 
    case lookEP(entEnv,entPath)
     of M.TYCent tycon => tycon
      | M.ERRORent => T.ERRORtyc
      | _ => bug "lookTycEP: wrong entity"

fun lookStrEP(entEnv,entPath) = 
    case lookEP(entEnv,entPath)
     of M.STRent rlzn => rlzn
      | M.ERRORent => M.bogusStrEntity
      | _ => bug "lookStrEP: wrong entity"

fun lookFctEP(entEnv,entPath) = 
    case lookEP(entEnv,entPath)
     of M.FCTent rlzn => rlzn
      | M.ERRORent => M.bogusFctEntity
      | _ => bug "lookFctEP: wrong entity"

end (* local *)
end (* structure LtycEnv *)

