(* Copyright 1996 by Bell Laboratories *)
(* typesutil.sml *)

structure TypesUtil : TYPESUTIL = struct

local
  structure EM = ErrorMsg
  structure SS = Substring
  structure EP = EntPath
  structure BT = CoreBasicTypes
  structure SP = SymPath
  structure IP = InvPath
  structure S = Symbol
  structure ST = Stamps
  structure A = Access
  open Types VarCon 
in

val array = Array.array
val sub = Array.sub
val update = Array.update
infix 9 sub

val --> = CoreBasicTypes.-->
infix -->

val say = Control_Print.say
val debugging = ref false
fun bug msg = EM.impossible("TypesUtil: "^msg)

fun eqpropToString p =
  case p
   of NO => "NO"
    | YES => "YES"
    | IND => "IND"
    | OBJ => "OBJ"
    | DATA => "DATA"
    | UNDEF => "UNDEF"
    | ABS => "ABS"


(*************** operations to build tyvars, VARtys ***************)

fun mkMETA depth =
    OPEN{kind=META, depth=depth, eq=false}

fun mkFLEX(fields, depth) =
    OPEN{kind=FLEX fields, depth=depth, eq=false}

fun extract_varname_info name =
    let val name = SS.triml 1 (SS.full name)  (* remove leading "'" *)
	val (name, eq) =
	  if SS.sub(name,0) = #"'"      (* initial "'" signifies equality *) 
          then (SS.triml 1 name,true)
          else (name,false)
     in (SS.string name, eq)
    end

fun mkUBOUND(id : Symbol.symbol) : tvKind =
    let val (name, eq) = extract_varname_info (Symbol.name id)
     in UBOUND{name=Symbol.tyvSymbol name, depth=infinity, eq=eq}
    end

fun mkLITERALty (k: litKind, r: SourceMap.region) : ty =
    VARty(mkTyvar(LITERAL{kind=k,region=r}))

fun mkSCHEMEty () : ty = VARty(mkTyvar(SCHEME false))

(*
 * mkMETAty:
 *
 *   This function returns a type that represents a new meta variable
 * which does NOT appear in the "context" anywhere.  To do the same
 * thing for a meta variable which will appear in the context (because,
 * for example, we are going to assign the resulting type to a program
 * variable), use mkMETAtyBounded with the appropriate depth.
 *)

fun mkMETAtyBounded depth : ty = VARty(mkTyvar (mkMETA depth))

fun mkMETAty() = mkMETAtyBounded infinity



(*************** primitive operations on tycons ***************)
fun bugTyc (s: string, tyc) =
  case tyc
   of GENtyc { path, ... } => bug (s ^ " GENtyc " ^ S.name (IP.last path))
    | DEFtyc {path,...} => bug (s ^ " DEFtyc " ^ S.name(IP.last path))
    | RECORDtyc _ => bug (s ^ " RECORDtyc")
    | PATHtyc{path,...} => bug (s ^ " PATHtyc " ^ S.name(IP.last path))
    | RECtyc _ => bug (s ^ " RECtyc")
    | FREEtyc _ => bug (s ^ " FREEtyc")
    | ERRORtyc => bug (s ^ " ERRORtyc")

(* short (single symbol) name of tycon *)
fun tycName (GENtyc { path, ... } | DEFtyc{path,...} | PATHtyc{path,...}) =
    IP.last path
  | tycName (RECORDtyc _) = S.tycSymbol "<RECORDtyc>"
  | tycName (RECtyc _) = S.tycSymbol "<RECtyc>"
  | tycName (FREEtyc _) = S.tycSymbol "<FREEtyc>"
  | tycName ERRORtyc = S.tycSymbol "<ERRORtyc>"

(* get the stamp of a tycon *)
fun tycStamp (GENtyc { stamp, ... } | DEFtyc { stamp, ... }) = stamp
  | tycStamp tycon = bugTyc("tycStamp",tycon)

(* full path name of tycon, an InvPath.path *)
fun tycPath (GENtyc{path,...} | DEFtyc{path,...} | PATHtyc{path,...}) = path
  | tycPath ERRORtyc = IP.IPATH[S.tycSymbol "error"]
  | tycPath tycon  = bugTyc("tycPath",tycon)

fun tycEntPath(PATHtyc{entPath,...}) = entPath
  | tycEntPath tycon = bugTyc("tycEntPath",tycon)

fun tyconArity(GENtyc { arity, ... } | PATHtyc{arity,...}) = arity
  | tyconArity(DEFtyc{tyfun=TYFUN{arity,...},...}) = arity
  | tyconArity(RECORDtyc l) = length l
  | tyconArity(ERRORtyc) = 0
  | tyconArity tycon = bugTyc("tyconArity",tycon)

fun setTycPath(tycon,path) =
    case tycon
     of GENtyc { stamp, arity, eq, kind, path = _, stub = _ } =>
	GENtyc { stamp = stamp, arity = arity, eq = eq, kind = kind,
		 path = path, stub = NONE }
      | DEFtyc{tyfun,strict,stamp,path=_} =>
          DEFtyc{tyfun=tyfun,path=path,strict=strict,stamp=stamp}
      | _ => bugTyc("setTycName",tycon)

fun eqRecordLabels(nil,nil) = true
  | eqRecordLabels(x::xs,y::ys) = Symbol.eq(x,y) andalso eqRecordLabels(xs,ys)
  | eqRecordLabels _ = false

fun eqTycon (GENtyc g, GENtyc g') = Stamps.eq (#stamp g, #stamp g')
  | eqTycon (ERRORtyc,_) = true
  | eqTycon (_,ERRORtyc) = true
  (* this rule for PATHtycs is conservatively correct, but is only an
     approximation *)
  | eqTycon(PATHtyc{entPath=ep,...},PATHtyc{entPath=ep',...}) =
      EP.eqEntPath(ep,ep')
  (*
   * This last case used for comparing DEFtyc's, RECORDtyc's.
   * Also used in PPBasics to check data constructors of
   * a datatype.  Used elsewhere?
   *)
  | eqTycon(RECORDtyc l1, RECORDtyc l2) = eqRecordLabels(l1,l2)
  | eqTycon _ = false

	(* for now... *)
fun mkCONty(ERRORtyc, _) = WILDCARDty
  | mkCONty(tycon as DEFtyc{tyfun,strict,...}, args) =
      CONty(tycon, ListPair.map
	              (fn (ty,strict) => if strict then ty else WILDCARDty)
                      (args,strict))
  | mkCONty(tycon, args) = CONty(tycon, args);

fun prune(VARty(tv as ref(INSTANTIATED ty))) : ty =
      let val pruned = prune ty
       in tv := INSTANTIATED pruned; pruned
      end
  | prune ty = ty
    
fun eqTyvar(tv1: tyvar, tv2: tyvar) = (tv1 = tv2)

fun bindTyvars(tyvars: tyvar list) : unit =
    let fun loop([],_) = ()
	  | loop(tv::rest,n) =
	      (tv := INSTANTIATED (IBOUND n);
	       loop(rest,n+1))
     in loop(tyvars,0)
    end

fun bindTyvars1(tyvars: tyvar list) : Types.polysign =
    let fun loop([],_) = []
	  | loop((tv as ref(UBOUND{eq,...}))::rest,n) =
	       (tv := INSTANTIATED (IBOUND n);
	        eq :: loop(rest,n+1))
	  | loop _ = bug "bindTyvars1:UBOUND"
     in loop(tyvars,0)
    end

exception SHARE

(* assume that f fails on identity, i.e. f x raises SHARE instead of 
   returning x *)
fun shareMap f nil = raise SHARE
  | shareMap f (x::l) =
      (f x) :: ((shareMap f l) handle SHARE => l)
      handle SHARE => x :: (shareMap f l)

(*** This function should be merged with instantiatePoly soon --zsh ***)
fun applyTyfun(TYFUN{arity,body},args) =
  let fun subst(IBOUND n) = List.nth(args,n)
        | subst(CONty(tyc,args)) = CONty(tyc, shareMap subst args)
        | subst(VARty(ref(INSTANTIATED ty))) = subst ty
        | subst _ = raise SHARE
   in if arity > 0
      then subst body
              handle SHARE => body
		   | Subscript => bug "applyTyfun - not enough arguments"
      else body
  end

fun mapTypeFull f =
    let fun mapTy ty =
	    case ty
	      of CONty (tc, tl) => 
		  mkCONty(f tc, map mapTy tl)
	       | POLYty {sign, tyfun=TYFUN{arity, body}} =>
		  POLYty{sign=sign, tyfun=TYFUN{arity=arity,body=mapTy body}}
	       | VARty(ref(INSTANTIATED ty)) => mapTy ty
	       | _ => ty
     in mapTy
    end

fun appTypeFull f =
    let fun appTy ty =
	    case ty
	      of CONty (tc, tl) => (f tc;  app appTy tl)
	       | POLYty {sign, tyfun=TYFUN{arity, body}} => appTy body
	       | VARty(ref(INSTANTIATED ty)) => appTy ty
	       | _ => ()
     in appTy
    end


exception ReduceType

fun reduceType(CONty(DEFtyc{tyfun,...}, args)) = applyTyfun(tyfun,args)
  | reduceType(POLYty{sign=[],tyfun=TYFUN{arity=0,body}}) = body
  | reduceType(VARty(ref(INSTANTIATED ty))) = ty
  | reduceType _ = raise ReduceType

fun headReduceType ty = headReduceType(reduceType ty) handle ReduceType => ty

fun equalType(ty,ty') =
    let fun eq(IBOUND i1, IBOUND i2) = i1 = i2
	  | eq(VARty(tv),VARty(tv')) = eqTyvar(tv,tv')
	  | eq(ty as CONty(tycon, args), ty' as CONty(tycon', args')) =
	      if eqTycon(tycon, tycon') then ListPair.all equalType(args,args') 
	      else (eq(reduceType ty, ty')
		    handle ReduceType =>
		      (eq(ty,reduceType ty') handle ReduceType => false))
	  | eq(ty1 as (VARty _ | IBOUND _), ty2 as CONty _) =
	      (eq(ty1,reduceType ty2)
	       handle ReduceType => false)
	  | eq(ty1 as CONty _, ty2 as (VARty _ | IBOUND _)) =
	      (eq(reduceType ty1, ty2)
	       handle ReduceType => false)
	  | eq(WILDCARDty,_) = true
	  | eq(_,WILDCARDty) = true
	  | eq _ = false
     in eq(prune ty, prune ty')
    end

local
  (* making dummy argument lists to be used in equalTycon *)
    val generator = Stamps.newGenerator()
    fun makeDummyType() =
	CONty(GENtyc{stamp = Stamps.fresh generator,
		     path = IP.IPATH[Symbol.tycSymbol "dummy"],
		     arity = 0, eq = ref YES, stub = NONE,
                     kind = PRIMITIVE CorePrimTycNum.ptn_void},[])
         (*
          * Making dummy type is a temporary hack ! pt_void is not used
          * anywhere in the source language ... Requires major clean up 
          * in the future. (ZHONG)
	  * DBM: shouldn't cause any problem here.  Only thing relevant
	  * property of the dummy types is that they have different stamps
	  * and their stamps should not agree with those of any "real" tycons.
          *)
    (* precomputing dummy argument lists
     * -- perhaps a bit of over-optimization here. [dbm] *)
    fun makeargs (0,args) = args
      | makeargs (i,args) = makeargs(i-1, makeDummyType()::args)
    val args10 = makeargs(10,[])  (* 10 dummys *)
    val args1 = [hd args10]
    val args2 = List.take (args10,2)
    val args3 = List.take (args10,3)  (* rarely need more than 3 args *)
 in fun dummyargs 0 = []    
      | dummyargs 1 = args1
      | dummyargs 2 = args2
      | dummyargs 3 = args3
      | dummyargs n =
	if n <= 10 then List.take (args10,n) (* should be plenty *)
	else makeargs(n-10,args10)  (* but make new dummys if needed *)
end

(* equalTycon.  This definition deals only partially with types that
   contain PATHtycs.  There is no interpretation of the PATHtycs, but
   PATHtycs with the same entPath will be seen as equal because of the
   definition on eqTycon. *)
fun equalTycon(ERRORtyc,_) = true
  | equalTycon(_,ERRORtyc) = true
  | equalTycon(t1,t2) =
     let val a1 = tyconArity t1 and a2 = tyconArity t2
      in if a1<>a2 then false
         else let val args = dummyargs a1
	       in equalType(mkCONty(t1,args),mkCONty(t2,args))
	      end
     end

(* instantiating polytypes *)

fun typeArgs n = 
    if n>0
    then mkMETAty() :: typeArgs(n-1)
    else []

val default_tvprop = false

fun mkPolySign 0 = []
  | mkPolySign n = default_tvprop :: mkPolySign(n-1)

fun dconTyc(DATACON{typ,const,name,...}) =
    let (* val _ = say "*** the screwed datacon ***"
           val _ = say (S.name(name))
           val _ = say " \n" *)
        fun f (POLYty{tyfun=TYFUN{body,...},...},b) = f (body,b)
	  | f (CONty(tyc,_),true) = tyc
	  | f (CONty(_,[_,CONty(tyc,_)]),false) = tyc
	  | f _ = bug "dconTyc"
     in f (typ,const)
    end

fun boundargs n = 
    let fun loop(i) =
	if i>=n then nil
	else IBOUND i :: loop(i+1)
     in loop 0
    end

fun dconType (tyc,domain) =
    let val arity = tyconArity tyc
    in
	case arity of
	    0 => (case domain of
		      NONE => CONty(tyc,[])
		    | SOME dom => dom --> CONty(tyc,[]))
	  | _ =>
	    POLYty{sign=mkPolySign arity,
		   tyfun=TYFUN{arity=arity,
			       body = case domain of
					  NONE => CONty(tyc,boundargs(arity))
					| SOME dom =>
					  dom --> CONty(tyc,boundargs(arity))}}
    end

(* matching a scheme against a target type -- used declaring overloadings *)
fun matchScheme (TYFUN{arity,body}: tyfun, target: ty) : ty =
    let val tyenv = array(arity,UNDEFty)
	fun matchTyvar(i:int, ty: ty) : unit = 
	    case tyenv sub i
	      of UNDEFty => update(tyenv,i,ty)
	       | ty' => if equalType(ty,ty')
			then () 
 			else bug("this compiler was inadvertantly \
			          \distributed to a user who insists on \
 				  \playing with 'overload' declarations.")
        fun match(scheme:ty, target:ty) =
	    case (prune scheme,prune(target))
	      of (WILDCARDty, _) => ()		(* Wildcards match any type *)
	       | (_, WILDCARDty) => ()		(* Wildcards match any type *)
	       | ((IBOUND i),ty) => matchTyvar(i,ty)
	       | (CONty(tycon1,args1), pt as CONty(tycon2,args2)) =>
		   if eqTycon(tycon1,tycon2)
		   then ListPair.app match (args1, args2)
		   else (match(reduceType scheme, target)
			 handle ReduceType =>
			   (match(scheme, reduceType pt)
			    handle ReduceType =>
			      bug "matchScheme, match -- tycons "))
	       | _ => bug "matchScheme, match"
     in case prune target
	  of POLYty{sign,tyfun=TYFUN{arity=arity',body=body'}} =>
	       (match(body,body');
	        POLYty{sign = sign,
		       tyfun = TYFUN{arity = arity',
			        body = if arity>1
				    then BT.tupleTy(Array.foldr (op ::)
								nil tyenv)
				    else tyenv sub 0}})
	   | ty => 
	       (match(body,ty);
	        if arity>1
		then BT.tupleTy(Array.foldr (op ::) nil tyenv)
		else tyenv sub 0)
    end

val rec compressTy =
   fn t as VARty(x as ref(INSTANTIATED(VARty(ref v)))) =>
	(x := v; compressTy t)
    | VARty(ref(OPEN{kind=FLEX fields,...})) =>
	app (compressTy o #2) fields
    | CONty(tyc,tyl) => app compressTy tyl
    | POLYty{tyfun=TYFUN{body,...},...} => compressTy body
    | _ => ()

(*
 * 8/18/92: cleaned up occ "state machine" some and fixed bug #612.
 *
 * Known behaviour of the attributes about the context that are kept:
 *
 * lamd = # of Abstr's seen so far.  Starts at 0 with Root.
 *
 * top = true iff haven't seen a LetDef yet.
 *)

abstype occ = OCC of {lamd: int, top: bool}
with

 val Root = OCC{lamd=0, top=true}

 fun LetDef(OCC{lamd,...}) = OCC{lamd=lamd, top=false}

 fun Abstr(OCC{lamd,top})  = OCC{lamd=lamd+1, top=top}

 fun lamdepth (OCC{lamd,...}) = lamd

 fun toplevel (OCC{top,...})  = top

end (* abstype occ *)

(* instantiatePoly: ty -> ty * ty list
   if argument is a POLYty, instantiates body of POLYty with new META typa
   variables, returning the instantiatied body and the list of META tyvars.
   if argument is not a POLYty, does nothing, returning argument type *)
fun instantiatePoly(POLYty{sign,tyfun}) : ty * ty list =
      let val args =
	      map (fn eq => 
		      VARty(ref(OPEN{kind = META, depth = infinity, eq = eq})))
		  sign
       in (applyTyfun(tyfun, args), args)
      end
  | instantiatePoly ty = (ty,[])

local 
  exception CHECKEQ
in
fun checkEqTySig(ty, sign: polysign) =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(CONty(DEFtyc{tyfun,...}, args)) =
	      eqty(applyTyfun(tyfun,args))
	  | eqty(CONty(GENtyc { eq, ... }, args)) =
	     (case !eq
		of OBJ => ()
		 | YES => app eqty args
		 | (NO | ABS | IND) => raise CHECKEQ
		 | p => bug ("checkEqTySig: "^eqpropToString p))
	  | eqty(CONty(RECORDtyc _, args)) = app eqty args
	  | eqty(IBOUND n) = if List.nth(sign,n) then () else raise CHECKEQ
	  | eqty _ = ()
     in eqty ty;
	true
    end
    handle CHECKEQ => false
end

(* compType, compareTypes used to compare specification type with type of
 * corresponding actual element.  Check that spec type is an instance of
 * the actual type *)
exception CompareTypes
fun compType(specty, specsign:polysign, actty,
	     actsign:polysign, actarity): unit =
    let val env = array(actarity,UNDEFty) (* instantiations of IBOUNDs in actual body *)
	fun comp'(WILDCARDty, _) = ()
	  | comp'(_, WILDCARDty) = ()
	  | comp'(ty1, IBOUND i) =
	     (case env sub i
		of UNDEFty =>
		    (let val eq = List.nth(actsign,i)
		      in if eq andalso not(checkEqTySig(ty1,specsign))
			 then raise CompareTypes
			 else ();
			 update(env,i,ty1)
		     end handle Subscript => ())
		 | ty => if equalType(ty1,ty)
			 then ()
			 else raise CompareTypes)
	  | comp'(CONty(tycon1, args1), CONty(tycon2, args2)) =
	      if eqTycon(tycon1,tycon2)
	      then ListPair.app comp (args1,args2)
	      else raise CompareTypes
	  | comp' _ = raise CompareTypes
        and comp(ty1,ty2) = comp'(headReduceType ty1, headReduceType ty2)
     in comp(specty,actty)
    end

(* returns true if actual type > spec type, i.e. if spec is an instance of actual *)
fun compareTypes (spec : ty, actual: ty): bool = 
    let val actual = prune actual
     in case spec
	  of POLYty{sign,tyfun=TYFUN{body,...}} =>
	      (case actual
		 of POLYty{sign=sign',tyfun=TYFUN{arity,body=body'}} =>
		      (compType(body,sign,body',sign',arity); true)
		  | WILDCARDty => true
		  | _ => false)
	   | WILDCARDty => true
	   | _ =>
	      (case actual
		 of POLYty{sign,tyfun=TYFUN{arity,body}} =>
		      (compType(spec,[],body,sign,arity); true)
		  | WILDCARDty => true
		  | _ => equalType(spec,actual))
    end handle CompareTypes => false

(* given a single-type-variable type, extract out the tyvar *)
fun tyvarType (VARty (tv as ref(OPEN _))) = tv
  | tyvarType (VARty (tv as ref(INSTANTIATED t))) = tyvarType t
  | tyvarType WILDCARDty = ref(mkMETA infinity)  (* fake a tyvar *)
  | tyvarType (IBOUND i) = bug "tyvarType: IBOUND"
  | tyvarType (CONty(_,_)) = bug "tyvarType: CONty"
  | tyvarType (POLYty _) = bug "tyvarType: POLYty"
  | tyvarType UNDEFty = bug "tyvarType: UNDEFty"
  | tyvarType _ = bug "tyvarType - unexpected argument" 

(* 
 * getRecTyvarMap : int * ty -> (int -> bool) 
 * see if a bound tyvar has occurred in some datatypes, e.g. 'a list. 
 * this is useful for representation analysis. This function probably
 * will soon be obsolete. 
 *)
fun getRecTyvarMap (n,ty) =
    let val s = Array.array(n,false)
	fun notArrow tyc = not (eqTycon (tyc, BT.arrowTycon))
	                  (* orelse eqTycon(tyc,contTycon) *)
	fun special (tyc as GENtyc { arity, ... }) =
	    arity <> 0 andalso notArrow tyc
	  | special(RECORDtyc _) = false
	  | special tyc = notArrow tyc

	fun scan(b,(IBOUND n)) = if b then (update(s,n,true)) else ()
	  | scan(b,CONty(tyc,args)) = 
	     let val nb = (special tyc) orelse b
	      in app (fn t => scan(nb,t)) args
	     end
	  | scan(b,VARty(ref(INSTANTIATED ty))) = scan(b,ty)
	  | scan _ = ()

	val _ = scan(false,ty)

     in fn i => (Array.sub(s,i) handle General.Subscript => 
		   bug "Strange things in TypesUtil.getRecTyvarMap")
    end

fun gtLabel(a,b) =
    let val a' = Symbol.name a and b' = Symbol.name b
        val a0 = String.sub(a',0) and b0 = String.sub(b',0)
     in if Char.isDigit a0
	  then if Char.isDigit b0
	    then (size a' > size b' orelse size a' = size b' andalso a' > b')
	    else false
	  else if Char.isDigit b0
	    then true
	    else (a' > b')
    end

(* Tests used to implement the value restriction *)
(* Based on Ken Cline's version; allows refutable patterns *)
(* Modified to support CAST, and special binding CASEexp. (ZHONG) *)
(* Modified to allow applications of lazy val rec Y combinators to
   be nonexpansive. (Taha, DBM) *) 
local open Absyn in

fun isValue (VARexp _) = true
  | isValue (CONexp _) = true
  | isValue (INTexp _) = true
  | isValue (WORDexp _) = true
  | isValue (REALexp _) = true
  | isValue (STRINGexp _) = true
  | isValue (CHARexp _) = true
  | isValue (FNexp _) = true
  | isValue (RECORDexp fields) =
    foldr (fn ((_,exp),x) => x andalso (isValue exp)) true fields
  | isValue (SELECTexp(_, e)) = isValue e
  | isValue (VECTORexp (exps, _)) =
    foldr (fn (exp,x) => x andalso (isValue exp)) true exps
  | isValue (SEQexp nil) = true
  | isValue (SEQexp [e]) = isValue e
  | isValue (SEQexp _) = false
  | isValue (APPexp(rator, rand)) =
    let fun isrefdcon(DATACON{rep=A.REF,...}) = true
          | isrefdcon _ = false

        fun iscast (VALvar { info, ... }) = InlInfo.isPrimCast info
          | iscast _ = false

        (* LAZY: The following function allows applications of the
         * fixed-point combinators generated for lazy val recs to
         * be non-expansive. *)
        fun issafe(VALvar{path=(SymPath.SPATH [s]),...}) = 
            (case String.explode (Symbol.name s)
              of (#"Y" :: #"$" :: _) => true
               | _ => false)
          | issafe _ = false

        fun iscon (CONexp(dcon,_)) = not (isrefdcon dcon)
          | iscon (MARKexp(e,_)) = iscon e
          | iscon (VARexp(ref v, _)) = (iscast v) orelse (issafe v)
          | iscon _ = false
    in if iscon rator then isValue rand
       else false
    end
  | isValue (CONSTRAINTexp(e,_)) = isValue e
  | isValue (CASEexp(e, (RULE(p,_))::_, false)) = 
    (isValue e) andalso (irref p) (* special bind CASEexps *)
  | isValue (LETexp(VALRECdec _, e)) = (isValue e) (* special RVB hacks *)
  | isValue (MARKexp(e,_)) = isValue e
  | isValue _ = false

(* testing if a binding pattern is irrefutable --- complete *)
and irref pp  = 
  let fun udcon(DATACON{sign=A.CSIG(x,y),...}) = ((x+y) = 1)
        | udcon _ = false

      fun g (CONpat(dc,_)) = udcon dc
        | g (APPpat(dc,_,p)) = (udcon dc) andalso (g p)
        | g (RECORDpat{fields=ps,...}) = 
              let fun h((_, p)::r) = if g p then h r else false
                    | h _ = true   
               in h ps
              end
        | g (CONSTRAINTpat(p, _)) = g p
        | g (LAYEREDpat(p1,p2)) = (g p1) andalso (g p2)
        | g (ORpat(p1,p2)) = (g p1) andalso (g p2)
        | g (VECTORpat(ps,_)) = 
              let fun h (p::r) = if g p then h r else false
                    | h _ = true
               in h ps
              end
        | g _ = true
   in g pp
  end
end (* local *)

fun isVarTy(VARty(ref(INSTANTIATED ty))) = isVarTy ty
  | isVarTy(VARty _) = true
  | isVarTy(_) = false


(* sortFields, mapUnZip: two utility functions used in type checking
   (typecheck.sml, mtderiv.sml, reconstruct.sml) *)

fun sortFields fields =
    ListMergeSort.sort (fn ((Absyn.LABEL{number=n1,...},_),
		   (Absyn.LABEL{number=n2,...},_)) => n1>n2)
              fields

fun mapUnZip f nil = (nil,nil)
  | mapUnZip f (hd::tl) =
     let val (x,y) = f(hd)
	 val (xl,yl) = mapUnZip f tl
      in (x::xl,y::yl)
     end

fun foldTypeEntire f =
    let fun foldTc (tyc, b0) = 
          case tyc
           of GENtyc { kind, ... } =>
	      (case kind of
		   DATATYPE{family={members=ms,...},...} => b0
(*             foldl (fn ({dcons, ...},b) => foldl foldDcons b dcons) b0 ms *)
		 | ABSTRACT tc => foldTc (tc, b0)
		 | _ => b0)
            | DEFtyc{tyfun=TYFUN{arity,body}, ...} => foldTy(body, b0)
            | _ => b0

        and foldDcons({name, rep, domain=NONE}, b0) = b0
          | foldDcons({domain=SOME ty, ...}, b0) = foldTy(ty, b0)

        and foldTy (ty, b0) =
	  case ty
	   of CONty (tc, tl) => 
                let val b1 = f(tc, b0)
                    val b2 = foldTc(tc, b1)
                 in foldl foldTy b2 tl
                end
	    | POLYty {sign, tyfun=TYFUN{arity, body}} => foldTy(body, b0)
	    | VARty(ref(INSTANTIATED ty)) => foldTy(ty, b0)
	    | _ => b0
     in foldTy
    end

fun mapTypeEntire f =
    let fun mapTy ty =
	  case ty
	   of CONty (tc, tl) => 
		mkCONty(f(mapTc, tc), map mapTy tl)
	    | POLYty {sign, tyfun=TYFUN{arity, body}} =>
		POLYty{sign=sign, tyfun=TYFUN{arity=arity,body=mapTy body}}
	    | VARty(ref(INSTANTIATED ty)) => mapTy ty
	    | _ => ty

        and mapTc tyc = 
          case tyc
	   of GENtyc { stamp, arity, eq, path, kind, stub = _ } =>
	      (case kind of
                   DATATYPE{index,family={members,...},...} => tyc
(*
 *  The following code needs to be rewritten !!! (ZHONG)

               GENtyc{stamp=stamp, arity=arity, eq=eq, path=path,
                       kind=DATATYPE {index=index, members=map mapMb members, 
                                      lambdatyc = ref NONE}}
*)
		 | ABSTRACT tc =>
		   GENtyc {stamp=stamp, arity=arity, eq=eq, path=path,
			   kind= ABSTRACT (mapTc tc),
			   stub = NONE}
		 | _ => tyc)
	    | DEFtyc{stamp, strict, tyfun, path} => 
              DEFtyc{stamp=stamp, strict=strict, tyfun=mapTf tyfun,
                     path=path}
            | _ => tyc

         and mapMb {tycname, stamp, arity, dcons, lambdatyc} = 
              {tycname=tycname, stamp=stamp, arity=arity, 
               dcons=(map mapDcons dcons), lambdatyc=ref NONE}

         and mapDcons (x as {name, rep, domain=NONE}) = x
           | mapDcons (x as {name, rep, domain=SOME ty}) = 
              {name=name, rep=rep, domain=SOME(mapTy ty)}

         and mapTf (TYFUN{arity, body}) = 
              TYFUN{arity=arity, body=mapTy body}

     in mapTy
    end


(*
 * Here, using a set implementation should suffice, however, 
 * I am using a binary dictionary instead. (ZHONG)
 *)
local
  structure TycSet = StampMap
in
  type tycset = tycon TycSet.map

  val mkTycSet = fn () => TycSet.empty

  fun addTycSet(tyc as GENtyc { stamp, ... }, tycset) = 
      TycSet.insert (tycset, stamp, tyc)
    | addTycSet _ = bug "unexpected tycons in addTycSet"

  fun inTycSet(tyc as GENtyc { stamp, ... }, tycset) =
      isSome (TycSet.find(tycset, stamp))
    | inTycSet _ = false

  fun filterSet(ty, tycs) = 
    let fun inList (a::r, tc) = if eqTycon(a, tc) then true else inList(r, tc)
          | inList ([], tc) = false

        fun pass1 (tc, tset) = 
          if inTycSet(tc, tycs) then
              (if inList(tset, tc) then tset else tc::tset)
          else tset
     in foldTypeEntire pass1 (ty, [])
    end
(*
val filterSet = fn x =>
  Stats.doPhase(Stats.makePhase "Compiler 034 filterSet") filterSet x
*)

end (* local TycSet *)

(*
(* The reformat function is called inside translate.sml to reformat
 * a type abstraction packing inside PACKexp absyn. It is a hack. (ZHONG)
 *)
fun reformat { tp_var, tp_tyc } (ty, tycs, depth) = 
  let fun h ([], i, ks, ps, nts) = (rev ks, rev ps, rev nts)
	| h (tc :: rest, i, ks, ps, nts) = let
	      fun noabs () = bug "non-abstract tycons seen in TU.reformat"
	  in
	      case tc
	       of GENtyc { stamp, arity, eq, path, kind, stub } =>
		  (case kind of
		       ABSTRACT itc => let
			   val tk = LT.tkc_int arity
(*
			   val tps = TP_VAR (TVI.toExn
						 {depth=depth, num=i, kind=tk})
*)
			   val tps = tp_var { depth=depth, num=i, kind=tk}
			   val nkind = FLEXTYC tps
			   val ntc =
			       GENtyc { stamp = stamp, arity = arity,
					eq = eq, kind = nkind, path = path,
					stub = NONE}
		       in
			   h (rest, i+1, tk::ks, (tp_tyc itc)::ps, ntc::nts)
		       end
		     | _ => noabs ())
		| _ => noabs ()
	  end

      val (tks, tps, ntycs) = h(tycs, 0, [], [], [])

      fun getTyc (foo, tc) = 
        let fun h(a::r, tc) = if eqTycon(a, tc) then a else h(r, tc)
              | h([], tc) = foo tc
         in h(ntycs, tc)
        end

      val nty = mapTypeEntire getTyc ty

   in (nty, tks, tps)
  end

val reformat = Stats.doPhase(Stats.makePhase "Compiler 047 reformat") reformat
*)

fun dtSibling(n,tyc as GENtyc { kind = DATATYPE dt, ... }) =
    let val {index,stamps,freetycs,root, family as {members,...} } = dt
    in
	if n = index then tyc
	else let val {tycname,arity,dcons,eq,lazyp,sign} =
		     Vector.sub(members,n)
		 val stamp= Vector.sub(stamps,n)
	     in
		 GENtyc {stamp=stamp,
			 arity=arity,eq=eq,path=IP.IPATH[tycname],
			 kind=DATATYPE{index=n,stamps=stamps,
				       freetycs=freetycs,
				       root=NONE (*!*),
				       family=family},
			 stub = NONE}
	     end
    end
  | dtSibling _ = bug "dtSibling"

(* NOTE: this only works (perhaps) for datatype declarations, but not 
   specifications. The reason: the root field is used to connect mutually
   recursive datatype specifications together, its information cannot be
   fully recovered in dtSibling. (ZHONG)
 *)
fun extractDcons (tyc as GENtyc { kind = DATATYPE dt, ... }) =
    let val {index,stamps,freetycs,root,family as {members,...}} = dt
	val {dcons,sign,lazyp,...} = Vector.sub(members,index)
	fun expandTyc(PATHtyc _) =
	    bug "expandTyc:PATHtyc" (* use expandTycon? *)
	  | expandTyc(RECtyc n) = dtSibling(n,tyc)
          | expandTyc(FREEtyc n) = 
	    ((List.nth(freetycs,n))
	     handle _ => bug "unexpected freetycs in extractDcons")
	  | expandTyc tyc = tyc

	fun expand ty = mapTypeFull expandTyc ty

	fun mkDcon({name,rep,domain}) =
	    DATACON{name = name, rep = rep, sign = sign, lazyp = lazyp,
		    typ = dconType (tyc, Option.map expand domain),
		    const = case domain of NONE => true | _ => false}

    in map mkDcon dcons
    end
  | extractDcons _ = bug "extractDcons"

fun mkStrict 0 = []
  | mkStrict n = true :: mkStrict(n-1)

(* used in ElabSig for datatype replication specs, where the tyc arg
 * is expected to be either a GENtyc/DATATYPE or a PATHtyc. *)
fun wrapDef(tyc as DEFtyc _,_) = tyc
  | wrapDef(tyc,s) =
    let val arity = tyconArity tyc
	val name = tycName tyc
        val args = boundargs arity
     in DEFtyc{stamp=s,strict=mkStrict arity,path=IP.IPATH[name],
	       tyfun=TYFUN{arity=arity,body=CONty(tyc,args)}}
    end

(* eta-reduce a type function: \args.tc args => tc *)
fun unWrapDef1(tyc as DEFtyc{tyfun=TYFUN{body=CONty(tyc',args),arity},...}) =
     let fun formals((IBOUND i)::rest,j) = if i=j then formals(rest,j+1) else false
	   | formals(nil,_) = true
	   | formals _ = false
      in if formals(args,0) then SOME tyc' else NONE
     end
  | unWrapDef1 tyc = NONE

(* closure under iterated eta-reduction *)
fun unWrapDefStar tyc =
     (case unWrapDef1 tyc
	of SOME tyc' => unWrapDefStar tyc'
         | NONE => tyc)

fun dummyTyGen () : unit -> Types.ty =
    let val count = ref 0
	fun next () = (count := !count + 1; !count)
        fun nextTy () =
	    let val name = "X"^Int.toString(next())
	     in CONty(GENtyc{stamp = ST.special name,
			     path = IP.IPATH[S.tycSymbol name],
			     arity = 0, eq = ref NO,
			     kind = ABSTRACT BT.boolTycon,
			     stub = NONE},
		      [])
	    end
     in nextTy
    end

end (* local *)
end (* structure TypesUtil *)
