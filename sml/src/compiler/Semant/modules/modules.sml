(* Copyright 1996 by AT&T Bell Laboratories *)
(* modules.sml *)

structure Modules : MODULES =
struct

local structure S  = Symbol
      structure SP = SymPath
      structure IP = InvPath
      structure DI = DebIndex
      structure EP = EntPath
      structure ST = Stamps
      structure LT = PLambdaType
      structure T = Types
      structure A = Access
      structure II = InlInfo
      structure E = Env
      structure V = VarCon
in

(* -------------------- signature-related definitions -------------------- *)

type sharespec = SP.path list  (* only internal sharing *)

datatype Signature
  = SIG of {name : S.symbol option,
	    closed : bool,
            fctflag : bool,
            stamp : ST.stamp,  
            symbols : S.symbol list,
            elements : (S.symbol * spec) list,
            boundeps : (EP.entPath * LT.tkind) list option ref,
            lambdaty : (LT.lty * DI.depth) option ref,
            typsharing: sharespec list,
            strsharing: sharespec list}
  | ERRORsig

(*
 * 1. tycspec should only be GENtyc, with FORMAL or DATATYPE tyckinds, or DEFtyc.
 * 2. the stamp and the path for the GENtyc or DEFtyc should be meaningless
 *    (but the stamps are in fact used for relativization of withtype bodies and
 *     the datacon domains of datatype repl specs)
 * 3. if VALspec and CONspec are using typspec instead of T.ty, then
 *    the whole thing can be further cleaned up.
 *)
and spec
  = TYCspec of {entVar : EP.entVar, spec : T.tycon, scope: int}
  | STRspec of {entVar : EP.entVar, sign : Signature,
		def : (strDef * int) option, slot : int}
  | FCTspec of {entVar : EP.entVar, sign : fctSig, slot : int}
  | VALspec of {spec : T.ty, slot : int}
  | CONspec of {spec : V.datacon, slot : int option}

(*
 * and specEnv
 *  = NILsenv 
 *  | BINDsenv of spec E.env * specEnv
 *  | INCLsenv of int * spec E.env * specEnv
 *)

and fctSig 
  = FSIG of {kind     : S.symbol option,
             paramsig : Signature,
             paramvar : EP.entVar,
             paramsym : S.symbol option,
             bodysig  : Signature}  
  | ERRORfsig

and extDef
  = TYCdef of SP.path * T.tycon * bool (* relative *)
  | STRdef of SP.path * strDef

and strDef
  = CONSTstrDef of Structure  (* constant *)
  | VARstrDef of Signature * EP.entPath (* relative *)

(* ------------------------- structures and functors ---------------------- *)

and Structure
  = STR of {sign : Signature, rlzn : strEntity, 
            access: A.access, info : II.inl_info}
  | STRSIG of {sign: Signature, entPath : EP.entPath}
  | ERRORstr

and Functor
  = FCT of {sign : fctSig, rlzn : fctEntity, 
            access: A.access, info : II.inl_info}
  | ERRORfct

(* ----------------------- entity-related definitions -------------------- *)

and entity (* elements of a entityEnv *)
  = TYCent of tycEntity
  | STRent of strEntity
  | FCTent of fctEntity
  | ERRORent
       (* no entities for val, con, exn, but this may change *)

and fctClosure (* realization for functors *)
  = CLOSURE of {param : EP.entVar, body : strExp, env : entityEnv}

and stampExp
  = CONST of ST.stamp  (* an existing stamp *)
  | GETSTAMP of strExp
  | NEW                (* generate a new stamp *)

and tycExp (* expression evaluating to a TYCentity *)
  = VARtyc of EP.entPath                          (* selection from cur-EE *)
  | CONSTtyc of Types.tycon                       (* actual tycon *)
  | FORMtyc of Types.tycon                        (* formal tycon *)

and strExp 
  = VARstr of EP.entPath       (* selection from current entityEnv *)
  | CONSTstr of strEntity
  | STRUCTURE of {stamp : stampExp, entDec : entityDec}
  | APPLY of fctExp * strExp  
      (* the arg strExp contains coercions to match the fct param sig *)
  | LETstr of entityDec * strExp
  | ABSstr of Signature * strExp    (* shortcut for abstraction matching *)
  | FORMstr of fctSig               (* formal functor body structure *)
  | CONSTRAINstr of {boundvar : EP.entVar, raw : strExp, coercion: strExp}
      (* similar to LETstr(M.STRdec(boundvar, strExp), coercion),
       * but with special treatment of rpath propagation to support
       * accurate type names in functor results where the functor has
       * a result signature constraint. *)

and fctExp
  = VARfct of EP.entPath (* selection from current entityEnv *)
  | CONSTfct of fctEntity
  | LAMBDA of {param : EP.entVar, body : strExp}
  | LAMBDA_TP of {param : EP.entVar, body : strExp, sign : fctSig}
  | LETfct of entityDec * fctExp

and entityExp 
  = TYCexp of tycExp
  | STRexp of strExp
  | FCTexp of fctExp
  | DUMMYexp
  | ERRORexp

and entityDec 
  = TYCdec of EP.entVar * tycExp
  | STRdec of EP.entVar * strExp * S.symbol
  | FCTdec of EP.entVar * fctExp
  | SEQdec of entityDec list
  | LOCALdec of entityDec * entityDec
  | ERRORdec
  | EMPTYdec

and entityEnv 
  = MARKeenv of ST.stamp * entityEnv
  | BINDeenv of entity EP.EvDict.dict * entityEnv
  | NILeenv
  | ERReenv

withtype strEntity = {stamp : ST.stamp,
                      entities : entityEnv,
                      lambdaty : (LT.lty * DI.depth) option ref,
                      rpath : IP.path}

and fctEntity = {stamp : ST.stamp,
                 closure : fctClosure,
                 lambdaty : (LT.lty * DI.depth) option ref,
                 tycpath : T.tycpath option,
                 rpath : IP.path}

(* the stamp and arith inside T.tycon are critical *)  
and tycEntity = T.tycon

and elements = (S.symbol * spec) list

(*
and constraint  
  = {my_path : SP.path, its_ancestor : instrep, its_path : SP.path}
*)

val bogusStrStamp = ST.special "bogusStr"
val bogusFctStamp = ST.special "bogusFct"
val bogusSigStamp = ST.special "bogusSig"
val bogusRpath = IP.IPATH[S.strSymbol "Bogus"]

val bogusStrEntity : strEntity =
      {stamp = bogusStrStamp, entities = ERReenv, 
       lambdaty = ref NONE, rpath = bogusRpath}

val bogusSig : Signature = 
       SIG {name=NONE, closed=true, fctflag=false,
            stamp=bogusSigStamp, symbols=[], 
            elements=[], boundeps=ref NONE, lambdaty=ref NONE,
            typsharing=[], strsharing=[]}

val bogusFctEntity : fctEntity =
      {stamp = bogusFctStamp,
       closure = CLOSURE{param=EP.bogusEntVar,
                         body= CONSTstr bogusStrEntity,
                         env=NILeenv},
       tycpath=NONE, lambdaty = ref NONE, rpath = bogusRpath}

end (* local *)
end (* structure Modules *)

(*
 * $Log: modules.sml,v $
 * Revision 1.9  1997/09/30  02:31:20  dbm
 *   New constructor ERReenv for entityEnv.  Used for error recovery.
 *
 * Revision 1.8  1997/09/17  21:31:59  dbm
 *   New symbol parameter for STRdec (modules/tests/12.sml).
 *
 * Revision 1.7  1997/08/22  18:35:02  george
 *    Add the fctflag field to the signature datatype -- zsh
 *
 * Revision 1.6  1997/07/15  16:11:09  dbm
 *   Representation changes associated with the rewrite of instantiate.sml.
 *
 * Revision 1.5  1997/05/20  12:23:33  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.4  1997/04/02  04:09:07  dbm
 *   Added CONSTRAINstr constructor to type strExp.  Fix for bug 12.
 *
 * Revision 1.3  1997/03/17  18:51:17  dbm
 * Changes in datatype representation to support datatype replication.
 *
 * Revision 1.2  1997/01/21  13:25:31  george
 *    Modify the entityExp definition to correctly implement the
 *    datatype generativity in functor body. -- from zsh
 *
 *)
