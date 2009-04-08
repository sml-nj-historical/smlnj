(* modules.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure Modules : MODULES =
struct

local structure S  = Symbol
      structure SP = SymPath
      structure IP = InvPath
      structure EP = EntPath
      structure ST = Stamps
      structure T = Types
      structure A = Access
      structure E = Env
in

(* -------------------- signature-related definitions -------------------- *)

type sharespec = SP.path list  (* only internal sharing *)

datatype Signature
  = SIG of sigrec
  | ERRORsig

(*
 * 1. tyc spec tycon should only be GENtyc, with FORMAL or DATATYPE tyckinds, or DEFtyc.
 * 2. the stamp and the path for the GENtyc or DEFtyc should be meaningless
 *    (but the stamps are in fact used for relativization of withtype bodies and
 *     the datacon domains of datatype repl specs)
 * 3. if VALspec and CONspec are using typspec instead of T.ty, then
 *    the whole thing can be further cleaned up.
 *)
and spec
  = TYCspec of {entVar : EP.entVar, info: tycSpecInfo}
  | STRspec of {entVar : EP.entVar, sign : Signature,
		def : (strDef * int) option, slot : int}
  | FCTspec of {entVar : EP.entVar, sign : fctSig, slot : int}
  | VALspec of {spec : T.ty, slot : int}
  | CONspec of {spec : T.datacon, slot : int option}

(* there are two forms of TYCspec. One for regular, explicitly defined signatures,
 * and the other for inferred signatures, where all the type info is found in the
 * realization. But we need some info for printing in the one case where a
 * realization is not available with the signature, namely the inferred result
 * signature for a functor. *)
and tycSpecInfo
  = RegTycSpec of {spec : T.tycon, repl: bool, scope: int} (* normal signature *)
     (* repl = true for a datatype replication spec
      * scope deals with insertion of type spec via where clause, where rhs type
      *   expression is to be interpreted in an outer scope *)
  | InfTycSpec of {name: S.symbol, arity: int} (* inferred signature *)

and fctSig 
  = FSIG of {kind     : S.symbol option,
	     paramsig : Signature,
	     paramvar : EP.entVar,
	     paramsym : S.symbol option,
	     bodysig  : Signature}
  | ERRORfsig

(* rhs of where clauses in signatures *)
and extDef
  = TYCdef of
      {path : SymPath.path,  (* lhs of the where defn *)
       tyc : T.tycon,        (* tycon representing rhs of where defn *)
       relative : bool}      (* true when tyc contains entity paths *)
  | STRdef of SP.path * strDef

and strDef
  = CONSTstrDef of Structure  (* constant *)
  | VARstrDef of Signature * EP.entPath (* relative *)

(* ------------------------- structures and functors ---------------------- *)

(* Structure and Functor are the types providing the static (i.e. type) information 
 * associated with a structure or functor, respectively. *)

(* There should be a STRvar analagous to a VALvar, where the access property would
 * be associated with the STRvar, and the Structure type would be concerned only
 * with the static specification of the structure, i.e. the signature and
 * realization.  (the prim property would presumably belong to the STRvar).
 * Similarly, there should be a FCTvar type that would contain the access and
 * prim attribues of a functor.
*)

and Structure
  = STR of strrec   (* the normal "static" representation of a structure *)
  | STRSIG of {sign: Signature, entPath : EP.entPath}
  | ERRORstr  (* for ERROR tolerance *)

and Functor
  = FCT of fctrec
  | ERRORfct

(* ----------------------- entity-related definitions -------------------- *)

(* entity: the essential static info defining with types, structures, functors
 * interpreted (in the case of structures and functors) relative to signatures *)
and entity (* elements of a entityEnv *)
  = TYCent of tycEntity
  | STRent of strEntity
  | FCTent of fctEntity
  | ERRORent
       (* no entities for val, con, exn, but this may change *)

(* entity expressions
 * These are used to compute new resulting entities during functor applications.
 * They describe how types are propagated and generated by a functor. 
 * Evaluation of entity expressions is defined in EvalEntity. *)

and stampExp
  = GETSTAMP of strExp (* evaluate strExp and extract its structure stamp *)
  | NEW                (* generate a new stamp *)

and tycExp (* expression evaluating to a TYCentity *)
  = VARtyc of EP.entPath    (* selection from current entityEnv *)
  | CONSTtyc of T.tycon     (* actual tycon *)
  | FORMtyc of T.tycon      (* formal tycon *)

and strExp
  = VARstr of EP.entPath       (* selection from current entityEnv *)
  | CONSTstr of strEntity      (* a constant referece to an existing str entity *)
  | STRUCTURE of {stamp : stampExp, entDec : entityDec}
  | APPLY of fctExp * strExp  
      (* the arg strExp includes coercions to match the fct param sig *)
  | LETstr of entityDec * strExp
  | ABSstr of Signature * strExp    (* shortcut for abstraction matching *)
  | FORMstr of fctSig               (* formal functor body structure *)
  | CONSTRAINstr of {boundvar : EP.entVar, raw : strExp, coercion: strExp}
      (* similar to LETstr(M.STRdec(boundvar, strExp), coercion),
       * but with special treatment of rpath propagation to support
       * accurate type names in functor results where the functor has
       * a result signature constraint. *)

and fctExp
  = VARfct of EP.entPath   (* selection from current entityEnv *)
  | CONSTfct of fctEntity  (* a constant reference to an existing fct entity *)
  | LAMBDA of {param : EP.entVar, body : strExp, paramRlzn : strEntity}
       (* paramRlzn is a memoization of the instantiated functor param signature
        * used later in the translation phase.  It plays no direct role in
        * entity evaluation. *)
(*  | LAMBDA_TP of {param : EP.entVar, body : strExp, sign : fctSig} *)
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
  | EMPTYdec  (* could use SEQdec [] instead? *)
  | ERRORdec

and fctClosure (* core realization for functors *)
  = CLOSURE of {param : EP.entVar, body : strExp, env : entityEnv}

(* entity environments
 * map from entity variables to entities *)
and entityEnv 
  = MARKeenv of envrec
  | BINDeenv of entity EP.EvDict.map * entityEnv
  | NILeenv
  | ERReenv

(* modtree: an auxiliary structure used in pickle isolation, appears in stubinfo *)
and modtree
  = TYCNODE of Types.gtrec
  | SIGNODE of sigrec
  | STRNODE of strrec
  | FCTNODE of fctrec
  | ENVNODE of envrec
  | BRANCH of modtree list

withtype stubinfo =
    {owner : PersStamps.persstamp,
     lib   : bool,
     tree  : modtree}

and elements = (S.symbol * spec) list

and sigrec =
    {stamp      : ST.stamp,
     name       : S.symbol option,
     closed     : bool,
     fctflag    : bool,
     elements   : elements,
     typsharing : sharespec list,
     strsharing : sharespec list,
     stub       : stubinfo option,
     properties : PropList.holder} (* FLINT: (entpath * tkind) list option *)

(* contents of a STR.  named because also used in modtree/STRNODE *)
and strrec =
    {sign   : Signature,
     rlzn   : strEntity,
     access : A.access,
     prim   : PrimOpId.strPrimInfo}
  (* access and prim belong in a separate STRvar type, since they describe
   * the "dynamic" attributes of a structure *)

(* contents of a FCT.  named because also used in modtree/FCTNODE *)
and fctrec =
    {sign   : fctSig,
     rlzn   : fctEntity, 
     access : A.access,
     prim   : PrimOpId.strPrimInfo}
  (* access and prim belong in a separate FCTvar type, since they describe
   * the "dynamic" attributes of a functor *)

(* envrec: chunks of entity evns are marked with a stamp and stubbed for
 * pickling isolation *)
and envrec =
    {stamp : ST.stamp,
     env   : entityEnv,
     stub  : stubinfo option}

(* strEntity and fctEntity
 * realizations: the essential static descriptions for structures and functors
 * relative to corresponding signatures. This is the "statically volatile
 * information that varies between different instantiations of the signatures *)

and strEntity =
    {stamp    : ST.stamp,         (* structure stamp *)
     entities : entityEnv,        (* the realizations of the static elements
				   * specified in the structure signature *)
     rpath    : IP.path,          (* reverse symbolic path of structure *)
     stub     : stubinfo option,  (* for pickling isolation *)
     properties: PropList.holder} (* FLINT: lambdaty memoization *)

and fctEntity =
    {stamp    : ST.stamp,
     paramRlzn: strEntity,      (* an instantiation of the param signature *)
     bodyRlzn : strEntity,      (* body realization -- THIS FIELD WILL BE DELETED *)
     closure  : fctClosure,     (* used to compute result rlzn in functor application *)
     rpath    : IP.path,        (* reverse symbolic path name of the functor *)
     stub     : stubinfo option, (* for pickling isolation *)
     properties: PropList.holder} (* FLINT: lambdaty memoization *)

(* a tycEntity is just a tycon. The stamp and arity of the tycon are critical. *)  
and tycEntity = T.tycon


(* some default/error values *)

val bogusStrStamp = ST.special "bogusStr"
val bogusFctStamp = ST.special "bogusFct"
val bogusSigStamp = ST.special "bogusSig"
val bogusRpath = IP.IPATH[S.strSymbol "Bogus"]

val bogusStrEntity : strEntity =
    { stamp = bogusStrStamp, 
      entities = ERReenv,
      properties = PropList.newHolder (), (* lambdaty = ref NONE *)
      rpath = bogusRpath,
      stub = NONE}

val bogusSig : Signature = 
    SIG {stamp = bogusSigStamp,
	 name=NONE, closed=true, fctflag=false,
	 elements=[],
	 properties = PropList.newHolder (),
	 (* boundeps=ref NONE, lambdaty=ref NONE *)
	 typsharing=[], strsharing=[],
	 stub = NONE}

val bogusFctEntity : fctEntity =
    {stamp = bogusFctStamp,
     paramRlzn = bogusStrEntity,
     bodyRlzn = bogusStrEntity,
     closure = CLOSURE{param=EP.bogusEntVar,
		       body= CONSTstr bogusStrEntity,
		       env=NILeenv},
     properties = PropList.newHolder (), (* lambdaty = ref NONE *)
     rpath = bogusRpath,
     stub = NONE}

end (* local *)
end (* structure Modules *)
