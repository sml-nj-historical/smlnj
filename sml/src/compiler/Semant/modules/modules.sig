(* Copyright 1996 by AT&T Bell Laboratories *)
(* modules.sig *)

signature MODULES =
sig

type sharespec = SymPath.path list

datatype Signature
  = SIG of {name : Symbol.symbol option,
	    closed : bool,
            fctflag : bool,
            stamp : Stamps.stamp,  
            symbols : Symbol.symbol list, 
            elements : (Symbol.symbol * spec) list,
            boundeps : (EntPath.entPath * PLambdaType.tkind) list option ref,
            lambdaty : (PLambdaType.lty * DebIndex.depth) option ref,
            typsharing: sharespec list,
            strsharing: sharespec list}
  | ERRORsig

and spec
  = TYCspec of {entVar : EntPath.entVar, spec : Types.tycon, scope : int}
  | STRspec of {entVar : EntPath.entVar, sign : Signature,
		def : (strDef * int) option, slot : int}
  | FCTspec of {entVar : EntPath.entVar, sign : fctSig, slot : int}
  | VALspec of {spec : Types.ty, slot : int}
  | CONspec of {spec : VarCon.datacon, slot : int option}

(*
 * and specEnv
 *  = NILsenv 
 *  | BINDsenv of spec Env.env * specEnv
 *  | INCLsenv of int * spec Env.env * specEnv
 *)

and fctSig 
  = FSIG of {kind     : Symbol.symbol option,
             paramsig : Signature,
             paramvar : EntPath.entVar,
             paramsym : Symbol.symbol option,
             bodysig  : Signature}  
  | ERRORfsig

and extDef
  = TYCdef of SymPath.path * Types.tycon * bool (* relative *)
  | STRdef of SymPath.path * strDef

and strDef
  = CONSTstrDef of Structure  (* constant *)
  | VARstrDef of Signature * EntPath.entPath  (* relative *)


(* ------------------------- structures and functors ---------------------- *)

and Structure
  = STR of {sign : Signature, rlzn : strEntity, access : Access.access,
            info : InlInfo.inl_info}
  | STRSIG of {sign: Signature, entPath : EntPath.entPath}
  | ERRORstr

and Functor
  = FCT of {sign : fctSig, rlzn : fctEntity, access: Access.access, 
            info : InlInfo.inl_info}
  | ERRORfct

(* ----------------------- entity-related definitions -------------------- *)

and entity (* elements of a entityEnv *)
  = TYCent of tycEntity
  | STRent of strEntity
  | FCTent of fctEntity
  | ERRORent
       (* no entities for val, con, exn, but this may change *)

and fctClosure (* realization for functors *)
  = CLOSURE of {param : EntPath.entVar, body : strExp, env : entityEnv}

and stampExp
  = CONST of Stamps.stamp  (* an existing stamp *)
  | GETSTAMP of strExp
  | NEW                (* generate a new stamp *)

and tycExp (* expression evaluating to a TYCentity *)
  = VARtyc of EntPath.entPath (* selection from current entityEnv *)
  | CONSTtyc of Types.tycon    (* actual tycon *)
  | FORMtyc of Types.tycon                        (* formal tycon *)

and strExp 
  = VARstr of EntPath.entPath 
  | CONSTstr of strEntity
  | STRUCTURE of {stamp : stampExp, entDec : entityDec}
  | APPLY of fctExp * strExp  
  | LETstr of entityDec * strExp
  | ABSstr of Signature * strExp
  | FORMstr of fctSig
  | CONSTRAINstr of {boundvar : EntPath.entVar, raw : strExp, coercion: strExp}
      (* similar to LETstr(M.STRdec(boundvar, raw), coercion),
       * but with special treatment of rpath propagation to support
       * accurate type names in functor results where the functor has
       * a result signature constraint. *)

and fctExp
  = VARfct of EntPath.entPath 
  | CONSTfct of fctEntity
  | LAMBDA of {param : EntPath.entVar, body : strExp}
  | LAMBDA_TP of {param : EntPath.entVar, body : strExp, sign : fctSig}
  | LETfct of entityDec * fctExp

and entityExp 
  = TYCexp of tycExp
  | STRexp of strExp
  | FCTexp of fctExp
  | DUMMYexp
  | ERRORexp

and entityDec 
  = TYCdec of EntPath.entVar * tycExp
  | STRdec of EntPath.entVar * strExp * Symbol.symbol
  | FCTdec of EntPath.entVar * fctExp
  | SEQdec of entityDec list
  | LOCALdec of entityDec * entityDec
  | ERRORdec
  | EMPTYdec

and entityEnv 
  = MARKeenv of Stamps.stamp * entityEnv
  | BINDeenv of entity EntPath.EvDict.dict * entityEnv
  | NILeenv
  | ERReenv

withtype strEntity = {stamp : Stamps.stamp,
                      entities : entityEnv,
                      lambdaty : (PLambdaType.lty * DebIndex.depth) option ref,
                      rpath : InvPath.path}

and fctEntity = {stamp : Stamps.stamp,
                 closure : fctClosure,
                 lambdaty : (PLambdaType.lty * DebIndex.depth) option ref,
                 tycpath : Types.tycpath option,
                 rpath : InvPath.path}

(* the stamp and arith inside Types.tycon are critical *)  
and tycEntity = Types.tycon

and elements = (Symbol.symbol * spec) list

(*
and constraint  
  = {my_path : SymPath.path, its_ancestor : instrep, its_path : SymPath.path}
*)

val bogusStrEntity : strEntity
val bogusFctEntity : fctEntity

end (* signature MODULES *)


(*
 * $Log: modules.sig,v $
 * Revision 1.8  1997/09/30  02:30:42  dbm
 *   New constructor ERReenv for entityEnv.  Used for error recovery.
 *
 * Revision 1.7  1997/09/17  21:31:30  dbm
 *   New symbol parameter for STRdec (modules/tests/12.sml).
 *
 * Revision 1.6  1997/08/22  18:35:01  george
 *    Add the fctflag field to the signature datatype -- zsh
 *
 * Revision 1.5  1997/07/15  16:10:33  dbm
 *   Representation changes associated with the rewriting of instantiate.sml.
 *
 * Revision 1.4  1997/05/20  12:23:13  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.3  1997/04/02  04:08:22  dbm
 *   Added CONSTRAINstr constructor to strExp type.  Fix for bug 12, propagating
 *   paths to type constructors defined in functors.
 *
 * Revision 1.2  1997/01/21  13:25:30  george
 *    Modify the entityExp definition to correctly implement the
 *    datatype generativity in functor body. -- from zsh
 *
 *)
