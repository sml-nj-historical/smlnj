(* Copyright 1997 by Bell Laboratories *)
(* types.sml *)

structure Types : TYPES =
struct

local structure A  = Access
      structure DI = DebIndex
      structure EP = EntPath
      structure IP = InvPath
      structure LT = PLambdaType
      structure PT = PrimTyc
      structure S  = Symbol
      structure ST = Stamps
in

type label = S.symbol
type polysign = bool list         (* equality property indicator *)

datatype eqprop = YES | NO | IND | OBJ | DATA | ABS | UNDEF

datatype litKind = INT | WORD | REAL | CHAR | STRING 

datatype openTvKind 	
  = META                          (* metavariables: 
                                     depth = infinity for meta-args
                                     depth < infinity for lambda bound *)
  | FLEX of (label * ty) list     (* flex record variables *)

and tvKind		  
  = INSTANTIATED of ty (* instantiation of an OPEN *)
  | OPEN of
     {depth: int, eq: bool, kind: openTvKind} 
  | UBOUND of (* explicit type variables *)
     {depth: int, eq: bool, name: Symbol.symbol}
  | LITERAL of (* type of a literal *)
     {kind: litKind, region: SourceMap.region}
  | SCHEME of bool (* overloaded operator type scheme variable
		   * arg is true if must be instantiated to equality type *)
  | LBOUND of {depth: DI.depth, num: int}

and tycpath                        
  = TP_VAR of {depth: DI.depth, num: int, kind: LT.tkind}
  | TP_TYC of tycon
  | TP_FCT of tycpath list * tycpath list
  | TP_APP of tycpath * tycpath list
  | TP_SEL of tycpath * int

and tyckind
  = PRIMITIVE of PT.primtyc
  | DATATYPE of
     {index: int,
      stamps: ST.stamp vector,
      root : EP.entVar option,    (* the root field used by type spec only *)
      freetycs: tycon list,
      family : dtypeFamily}
  | ABSTRACT of tycon
  | FLEXTYC of tycpath            (* instantiated formal type constructor *)
  | FORMAL                        (* used only inside signatures *)
  | TEMP                          (* used only during datatype elaborations *)

and tycon
  = GENtyc of
      {stamp : ST.stamp, 
       arity : int, 
       eq    : eqprop ref,
       kind  : tyckind, 
       path  : IP.path}
  | DEFtyc of
      {stamp : ST.stamp, 
       tyfun : tyfun, 
       strict: bool list, 
       path  : IP.path}
  | PATHtyc of                    (* used only inside signatures *)
      {arity : int,
       entPath : EP.entPath,
       path : IP.path}
  | RECORDtyc of label list
  | RECtyc of int                 (* used only in domain type of dconDesc *)
  | FREEtyc of int                (* used only in domain type of dconDesc *)
  | ERRORtyc

and ty 
  = VARty of tyvar
  | IBOUND of int
  | CONty of tycon * ty list
  | POLYty of {sign: polysign, tyfun: tyfun}
  | WILDCARDty
  | UNDEFty

and tyfun 
  = TYFUN of {arity: int, body: ty}

withtype tyvar = tvKind ref

(* datacon description used in dtmember *)
and dconDesc =
    {name: S.symbol,
     rep: A.conrep,
     domain: ty option}

(* member of a family of (potentially) mutually recursive datatypes *)
and dtmember =
    {tycname: S.symbol,
     arity: int,
     eq: eqprop ref,
     lazyp: bool,
     dcons: dconDesc list,
     sign: A.consig}

and dtypeFamily = 
  {mkey: ST.stamp,
   members: dtmember vector,
   lambdatyc: (LT.tyc * DI.depth) option ref}

fun mkTyvar(kind: tvKind) : tyvar = ref kind

fun copyTyvar(tv: tyvar) = ref(!tv)

val infinity = 10000000

datatype datacon (* data constructors *)
  = DATACON of
      {name   : S.symbol,
       typ    : ty,
       rep    : A.conrep,
       lazyp  : bool,    (* LAZY: constructor belongs to lazy datatype? *)
       const  : bool,     (* redundant, could be determined from typ *)
       sign   : A.consig} (* redundant, ditto *)

end (* local *)
end (* structure Types *)


(*
 * $Log$
 *)
