structure TypesTP =
struct

local
    structure T = Types
    structure ST = Stamps
    structure IP = InvPath
    structure LT = LtyExtern
    structure EP = EntPath
    structure TU = TypesUtil
    structure A = Access
    structure S = Symbol
    structure IP = InvPath
in

datatype tycpath (* (instantiated) functor type parameter path *)
  = TP_VAR of { tdepth: DebIndex.depth, num: int, kind: LT.tkind }   
  | TP_TYC of tycon
  | TP_FCT of tycpath list * tycpath list
  | TP_APP of tycpath * tycpath list
  | TP_SEL of tycpath * int

and tycon
  = GENtycTP of {stamp : ST.stamp,
	       arity : int,
	       eq : T.eqprop ref,
	       kind : tyckind,
	       path : IP.path,
	       stub : T.stubinfo option}
  | NoTP of T.tycon

and tyckind
  = PRIMITIVE of int
  | DATATYPE of 
    {index : int,
     stamps : ST.stamp vector,
     root : EP.entVar option,
     freetycs : tycon list,
     family : T.dtypeFamily}
  | ABSTRACT of tycon
  | FLEXTYC of tycpath 

datatype ty
  = TyNoTP of T.ty
  | CONty of tycon * ty list

exception TYCTP (* Unexpected FLEXTYC, can convert from tyctp to tyc only 
		   when no FLEXTYC *)

(* TycToTypes : tycon -> Types.tycon *)
fun tycStripTP(NoTP tc) = tc
  | tycStripTP(GENtycTP{stamp,arity,eq,kind,path,stub}) =
    let 
	fun tyckind(PRIMITIVE i) = T.PRIMITIVE i
	  | tyckind(DATATYPE{index,stamps,root,freetycs,family}) =
	    T.DATATYPE{index=index,stamps=stamps,root=root,
		       freetycs=map tycStripTP freetycs,
		       family=family}
	  | tyckind(ABSTRACT tc) =
	    T.ABSTRACT(tycStripTP tc)
	  | tyckind(FLEXTYC _) = raise TYCTP
	val kind' = tyckind kind
    in
	T.GENtyc{stamp=stamp,arity=arity,eq=eq,kind=kind',path=path,stub=stub}
    end
(* TyToTypes : ty -> Types.ty *)
fun tyStripTP(TyNoTP t) = t
  | tyStripTP(CONty(tc,args)) = T.CONty(tycStripTP tc, map tyStripTP args)

exception IncomparableTypesTP

(* eqTycon : TypesTP.tycon * TypesTP.tycon -> bool *)
fun eqTycon(x : tycon, y : tycon) =
    (case (x, y) 
      of (NoTP tc, NoTP tc') => TU.eqTycon (tc, tc')
       | (GENtycTP{stamp=s,...}, GENtycTP{stamp=s',...}) => ST.eq(s,s')
       | _ => raise IncomparableTypesTP)

(* Other TypeUtil functions used in FLINT trans *)

(* applyTyfun : Types.tyfun * TypesTP.ty list -> TypesTP.ty *)
(* MU.transType *)
(* BT.isArrowType *)
(* BT.--> *) 
end (* local *)

end (* struct *)
