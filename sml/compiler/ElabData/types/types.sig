(* types.sig
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature TYPES =
sig

(* not quite abstract types... *)
type label (* = Symbol.symbol *)
type polysign (* = bool list *)

datatype eqprop = YES | NO | IND | OBJ | DATA | ABS | UNDEF

datatype litKind = INT | WORD | REAL | CHAR | STRING

datatype pkind (* Functor parameter kinds *)
  = PK_MONO                                    (* ground mono tycon *)
  | PK_SEQ of pkind list                       (* sequence of tycons *)
  | PK_FUN of pkind list * pkind               (* n-ary tycon function *)

datatype openTvKind
  = META
  | FLEX of (label * ty) list

(* tvKind: the contents of the ref cell representing a type variable
 * introduced by type checking. *)
and tvKind
  = INSTANTIATED of ty
    (* a substitution has been applied to the type variable, replacing
     * it with ty *)
  | OPEN of {depth: int, eq: bool, kind: openTvKind}
    (* a unification variable awaiting elimination by (1) substitution, when
     * it will become INSTANTIATED, or by (2) generalization, when the type
     * variable itself will be replaced by an IBOUND type, or (3) in the
     * case of a top-level value restriction preventing generalization,
     * by instantiation to a fresh atomic type *)
  | UBOUND of {depth: int, eq: bool, name: Symbol.symbol}
    (* a type variable corresponding to an explicit type variable in the
     * code, which can only be generalized, never instantiated *)
  | LITERAL of {kind: litKind, region: SourceMap.region}
    (* representing the unresolved type of a literal expression (constant) *)
  | SCHEME of bool
    (* representing a type variable in the type scheme of an overloaded
     * operator. Must be eliminated by overloading resolution, cannot be
     * generalized *)
  | LBOUND of {depth: int, index: int}
    (* FLINT-style de Bruijn index for notional "lambda"-bound type variables
     * associated with polymorphic bindings (including val bindings and
     * functor parameter bindings). The depth is depth of type lambda bindings,
     * (1-based), and the index is the index within a sequence of type variables
     * bound at a given binding site. Replaces older TV_MARK constructor.
     * LBOUND are assigned to residual type variables in types embedded in
     * the abstract syntax of a definiens, after they have been generalized
     * away in the type computed by the type checker for the defined variable. *)

and tycpath
  = TP_VAR of { tdepth: DebIndex.depth,
		num: int, kind: pkind }
  | TP_TYC of tycon
  | TP_FCT of tycpath list * tycpath list
  | TP_APP of tycpath * tycpath list
  | TP_SEL of tycpath * int

and tyckind
  = PRIMITIVE of int		(* primitive kinds are abstractly numbered *)
  | ABSTRACT of tycon
  | DATATYPE of
     {index: int,
      stamps: Stamps.stamp vector,
      root : EntPath.entVar option,
      freetycs: tycon list,
      family : dtypeFamily}
  | FLEXTYC of tycpath
  | FORMAL
  | TEMP

and tycon
  = GENtyc of gtrec
  | DEFtyc of
      {stamp : Stamps.stamp,
       tyfun : tyfun,
       strict: bool list,
       path  : InvPath.path}
  | PATHtyc of
      {arity : int,
       entPath : EntPath.entPath,
       path : InvPath.path}
  | RECORDtyc of label list
  | RECtyc of int                (* used only in domain type of dconDesc *)
  | FREEtyc of int               (* used only in domain type of dconDesc *)
  | ERRORtyc

and ty
  = VARty of tyvar
  | IBOUND of int
  | CONty of tycon * ty list
  | POLYty of {sign: polysign, tyfun: tyfun}
  | WILDCARDty
  | UNDEFty

and tyfun
  = TYFUN of {arity : int, body : ty}

(* datacon description used in dtmember *)
withtype dconDesc =
    {name: Symbol.symbol,
     rep: Access.conrep,
     domain: ty option}

(* member of a family of (potentially) mutually recursive datatypes *)
and dtmember =
    {tycname: Symbol.symbol,
     arity: int,
     eq: eqprop ref,
     lazyp : bool,
     dcons: dconDesc list,
     sign: Access.consig}

and dtypeFamily =
  {mkey: Stamps.stamp,
   members: dtmember vector,
   properties: PropList.holder}
	       

and stubinfo =
    {owner : PersStamps.persstamp,
     lib   : bool}

(* The "stub" field will be set for any GENtyc that comes out of the
 * unpickler.  The stub owner pid is the pid of the compilation unit on whose
 * behalf this GENtyc was pickled.  Normally, this is expected to be the
 * same as the pid in the (global) "stamp", but there are odd cases related
 * to recursive types where this assumption breaks.  (Is there a way of
 * fixing this? -M.) *)
and gtrec =
    {stamp : Stamps.stamp,
     arity : int,
     eq    : eqprop ref,
     kind  : tyckind,
     path  : InvPath.path,
     stub  : stubinfo option}

and tyvar = tvKind ref

val infinity : int
val mkTyvar  : tvKind -> tyvar
val copyTyvar : tyvar -> tyvar

datatype datacon                    (* data constructors *)
  = DATACON of
      {name   : Symbol.symbol,
       typ    : ty,
       rep    : Access.conrep,
       lazyp  : bool,            (* LAZY *)
       const  : bool,
       sign   : Access.consig}

end (* signature TYPES *)
