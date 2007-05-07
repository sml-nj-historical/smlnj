(* Copyright 1997 Bell Laboratories *)
(* unify.sml *)

signature UNIFY =
sig

  datatype unifyFail
    = CIRC (* circularity *)
    | EQ (* equality type required *)
    | TYC of Types.tycon * Types.tycon (* tycon mismatch *)
    | TYP of Types.ty * Types.ty (* type mismatch *)
    | LIT of Types.tvKind (* literal *)
    | UBVE of Types.tvKind (* UBOUND, equality mismatch *)
    | UBV of Types.tvKind (* UBOUND match *)
    | SCH (* SCHEME, equality mismatch  *)
    | REC (* record labels *)

  exception Unify of unifyFail
  val failMessage: unifyFail -> string

  val unifyTy : Types.ty * Types.ty -> unit

  val debugging : bool ref

end (* signature UNIFY *)


structure Unify: UNIFY =
struct

(*** type unification ***)
val debugging = ElabControl.unidebugging

local
  structure T = Types
  structure TU = TypesUtil
  structure OLL = OverloadLit
  structure ED = ElabDebug
  open Types 

  (* debugging *)
  val say = Control_Print.say
  fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()

  fun bug msg = ErrorMsg.impossible("Unify: "^msg)

  val ppType = PPType.ppType StaticEnv.empty
  fun debugPPType (msg,ty) =
      ED.debugPrint debugging (msg, ppType, ty)

in

datatype unifyFail
  = CIRC (* circularity *)
  | EQ (* equality type required *)
  | TYC of Types.tycon * Types.tycon (* tycon mismatch *)
  | TYP of Types.ty * Types.ty (* type mismatch *)
  | LIT of Types.tvKind (* literal *)
  | UBVE of Types.tvKind (* UBOUND, equality mismatch *)
  | UBV of Types.tvKind (* UBOUND match *)
  | SCH (* SCHEME, equality mismatch  *)
  | REC (* record labels *)

fun failMessage failure =
    case failure
      of CIRC => "circularity"
       | EQ => "equality type required"
       | TYC(tyc1,tyc2) => "tycon mismatch"
       | TYP(ty1,ty2) => "type mismatch"
       | LIT(info) => "literal"
       | UBVE(info) => "UBOUND, equality mismatch"
       | UBV(info) => "UBOUND match"
       | SCH => "SCHEME, equality mismatch"
       | REC => "record labels"


exception Unify of unifyFail


(*************** misc functions *****************************************)

val eqLabel = Symbol.eq

fun eqLitKind (lk : T.litKind) =
    case lk of (INT | WORD | CHAR | STRING) => true | REAL => false

(*
 * tyconEqprop tycon:
 *
 *    This function returns the eqprop of tycon for use in determining
 * when a CONty is an equality type.
 *
 * Note: Calling this function on ERRORtyc produces an impossible
 * because an ERRORtyc should never occur in a CONty and hence an eqprop
 * of one of them should never be needed.
 *
 * [GK 5/7/07] The above note is not true. See bug271. Since an error
 * was already flagged, it seems harmless to return YES for the eqprop 
 * to avoid possibly spurious eqprop related warnings.  
 * 
 * Calling this function on a DEFtyc also produces an impossible because
 * the current eqprop scheme is insufficiently expressive to describe
 * the possibilities.  (Ex: first argument must be an eq type but not
 * necessarily the second)  Because of this, it is currently necessary to
 * expand DEFtyc's before checking for equality types.
 *)
fun tyconEqprop (GENtyc { eq, ... }) =
    (case !eq of ABS => NO | ep => ep)
  | tyconEqprop (RECORDtyc _)  = YES
  | tyconEqprop (DEFtyc _) = bug "tyconEqprop: DEFtyc"
  | tyconEqprop (ERRORtyc) = YES
  | tyconEqprop _ = bug "unexpected tycon in tyconEqprop"

(*
 * fieldwise(just1,just2,combine,fields1,fields2):
 *
 *    This function merges two sorted lists of (label, type) pairs
 * (sorted by label) into a single sorted list of (label, type) pairs.
 * If (l1,t1) occurs in fields1 but l1 doesn't occur in fields2 then
 * (l1, just1 t1) occurs in the output.  Similarly with just2.
 * If (l, t1) occurs in fields1 and (l,t2) in fields2, then 
 * (l, combine t1 t2) occurs in the output.
 *)
fun fieldwise(_,just2,_,[],fields2) = map (fn (n,t) => (n,just2 t)) fields2
  | fieldwise(just1,_,_,fields1,[]) = map (fn (n,t) => (n,just1 t)) fields1
  | fieldwise(just1,just2,combine,((n1,t1)::r1),((n2,t2)::r2)) =
      if eqLabel(n1,n2) then
	(n1,combine(t1,t2))::(fieldwise(just1,just2,combine,r1,r2))
      else if TU.gtLabel(n2,n1) then
	(n1,just1 t1)::(fieldwise(just1,just2,combine,r1,((n2,t2)::r2)))
      else
	(n2,just2 t2)::(fieldwise(just1,just2,combine,((n1,t1)::r1),r2))


(*************** adjust function *****************************************)

(* propagate depth and eq while checking for circularities in the
 * type ty that is going to unify with tyvar var *)

(* ASSERT: VARty var <> ty *)
fun adjustType (var,depth,eq,ty) =
    let val _ = debugPPType(">>adjustType: ",ty)
	fun iter _ WILDCARDty = ()
	  | iter eq (VARty(var' as ref(info))) =
	      (case info
		 of INSTANTIATED ty => 
		      (debugmsg "adjustType INSTANTIATED";
		       iter eq ty)
		  | OPEN{kind=k,depth=d,eq=e} =>
		      (* check for circularity, propagage eq and depth *)
		      if TU.eqTyvar(var,var')
		      then raise Unify CIRC
		      else (case k
			      of FLEX fields =>
				  (* recurse into FLEX field types *)
				  app (fn (l,t) => adjustType(var,depth,e,t))
				      fields
			       | _ => ();
			    var' := OPEN{depth=Int.min(depth,d),
					 eq=eq orelse e, kind=k})
		  | UBOUND{depth=d,eq=e,name} =>
		      (* check if eq is compatible and propagate depth *)
		      if eq andalso not e
		      then raise Unify EQ
		      else if depth < d
		      then var' := UBOUND{depth=depth,eq=e,name=name}
		      else ()
		  | SCHEME eq' =>
		      if TU.eqTyvar(var,var')
		      then raise Unify CIRC
		      else if eq andalso not eq'
		      then var' := SCHEME eq
		      else ()
		  | LITERAL{kind=k,...} =>
		      (* check if eq is compatible *)
		      if eq andalso not(eqLitKind k)
		      then raise Unify EQ
		      else ()
		  | LBOUND _ => bug "unify:adjustType:LBOUND")
	  | iter eq (ty as CONty(DEFtyc{tyfun=TYFUN{body,...},...}, args)) =
	      (app (iter eq) args; iter eq (TU.headReduceType ty))
	      (* A headReduceType here may cause instTyvar to 
	       * infinite loop if this CONty has a nonstrict arg 
	       * against which we are unifying/instantiating
	       * Because we may be instantiating to nonstrict 
	       * univariables, it is safer to do an occurrence 
	       * check on all the arguments. 
	       * [GK 4/28/07] *)
 	  | iter eq (CONty(tycon,args)) =
	      (case tyconEqprop tycon
		 of OBJ => app (iter false) args
		  | YES => app (iter eq) args
		  | _ =>
		    if eq then raise Unify EQ
		    else app (iter false) args)
 (* BUG? why don't these cases blow up (in tyconEqprop) when iter is applied
    to arguments that are unreduced applications of DEFtycs? *)
          | iter _ (POLYty _) = bug "adjustType 1"
          | iter _ (IBOUND _) = bug "adjustType 2"
	  | iter _ _ = bug "adjustType 3"
     in iter eq ty; debugmsg "<<adjustType"
    end

(*************** unify functions *****************************************)

(* LITERAL can be instantiated to a compatible LITERAL or a monotype of
 *   its LITERAL class
 * UBOUND cannot be instantiated, but it's depth property can be reduced
 * FLEX can merge with another FLEX or instantiate a META
 * META can be instantiated to anything
 *)

(* reorder two tyvars in descending order according to the ordering
 * LITERAL > UBOUND > SCHEME > OPEN/FLEX > OPEN/META *)
fun sortVars(v1 as ref i1, v2 as ref i2) =
    case (i1,i2)
      of (LITERAL _, _) => (v1,v2)
       | (_, LITERAL _) => (v2,v1)
       | (UBOUND _, _) => (v1,v2)
       | (_, UBOUND _)=> (v2,v1)
       | (SCHEME _, _) => (v1,v2)
       | (_, SCHEME _)=> (v2,v1)
       | (OPEN{kind=FLEX _,...}, _) => (v1,v2)
       | (_, OPEN{kind=FLEX _,...})=> (v2,v1)
       | _ => (v1,v2) (* both OPEN/META *)

(* unifyTy expects that there are no POLYtys with 0-arity 
   CONty(DEFtyc, _) are reduced only if absolutely necessary. *)
fun unifyTy(type1,type2) =
    let val type1 = TU.prune type1
	val type2 = TU.prune type2
	val _ = debugPPType(">>unifyTy: type1: ",type1)
	val _ = debugPPType(">>unifyTy: type2: ",type2)
	fun unifyRaw(type1, type2) = (* unify without reducing CONty(DEFtycs) *)
	    case (type1, type2)
	  of (VARty var1,VARty var2) =>
	       unifyTyvars(var1,var2)  (* used to take type1 and type2 as args *)
	   | (VARty var1,etype2) => (* etype2 may be WILDCARDty *)
	       instTyvar(var1,type2,etype2)
	   | (etype1,VARty var2) => (* etype1 may be WILDCARDty *)
	       instTyvar(var2,type1,etype1)
	   | (CONty(tycon1,args1),CONty(tycon2,args2)) =>
	       if TU.eqTycon(tycon1,tycon2) then
		   (* Because tycons are equal, they must have the 
		      same arity. Assume that lengths of args1 and
		      args2 are the same. Type abbrev. strictness
		      optimization. If tycons equal, then only check
		      strict arguments. [GK 4/28/07] *)
		   (case tycon1 
		     of DEFtyc{strict, ...} =>
			let fun unifyArgs([],[],[]) = ()
			      | unifyArgs(true::ss, ty1::tys1, ty2::tys2) =
				(unifyTy(ty1,ty2); unifyArgs(ss,tys1,tys2))
			      | unifyArgs(false::ss, _::tys1, _::tys2) =
				unifyArgs(ss,tys1,tys2)
			      | unifyArgs _ = 
				  bug "unifyTy: arg ty lists wrong length"
			in unifyArgs(strict,args1,args2)
			end
		      | _ => ListPair.app unifyTy (args1,args2))
	       else raise Unify (TYC(tycon1,tycon2))
	  (* if one of the types is WILDCARDty, propagate it down into the
	   * other type to eliminate tyvars that might otherwise cause
	   * generalizeTy to complain. *)
	   | (WILDCARDty, CONty(_, args2)) => 
               (app (fn x => unifyTy(x, WILDCARDty)) args2)
           | (CONty(_, args1), WILDCARDty) =>
               (app (fn x => unifyTy(x, WILDCARDty)) args1)
	   | (WILDCARDty,_) => ()
	   | (_,WILDCARDty) => ()
	   | tys => raise Unify (TYP tys)
    in unifyRaw(type1, type2) 
       handle Unify _ => (* try reducing CONty(DEFtyc, _) to make types equal *)
	      let val type1' = TU.headReduceType type1
	      in unifyRaw(type1', type2)
		 handle Unify _ => (* try reducing type2 *)
			unifyRaw(type1', TU.headReduceType type2)
			(* if unification still fails, then type1 and type2
			   really cannot be made to be equal *)
	      end
    end

and unifyTyvars (var1, var2) =
    let fun unify(var1 as ref i1, var2 as ref i2) =
	    (* ASSERT: var1 <> var2 *)
	    case i1
	      of LITERAL{kind,region} =>
		  (case i2
		     of LITERAL{kind=kind',...} =>
			 if kind = kind'
			 then var2 := INSTANTIATED (VARty var1)
			 else raise Unify (LIT i1)
		      | (OPEN{kind=META,eq=e2,...} | SCHEME e2)=>
			 (* check eq compatibility *)
			 if not e2 orelse eqLitKind kind
			 then var2 := INSTANTIATED (VARty var1)
			 else raise Unify (LIT i1)
		      | _ => raise Unify (LIT i1))

	       | UBOUND {depth=d1,eq=e1,name} =>
		  (case i2
		     of OPEN{kind=META,eq=e2,depth=d2} =>
			 if e1 orelse (not e2)
			 then (if d2 < d1
				   then var1 := UBOUND{depth=d2,eq=e1,name=name}
				   else ();
			       var2 := INSTANTIATED (VARty var1))
			 else raise Unify (UBV i1)
		      | _ => raise Unify (UBV i1))

	       | SCHEME e1 =>
		  (case i2
		     of SCHEME e2 =>
			 if e1 orelse not e2 then var2 := INSTANTIATED (VARty var1)
			 else var1 := INSTANTIATED(VARty var2)
		      | OPEN{kind=META,eq=e2,depth=d2} =>
			 if e1 orelse (not e2)
			 then var2 := INSTANTIATED (VARty var1)
		         else (var1 := SCHEME e2;
			       var2 := INSTANTIATED (VARty var1))
		      | _ => raise Unify SCH)

	       | OPEN{kind=k1 as FLEX f1,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=k2,eq=e2,depth=d2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in case k2
			       of FLEX f2 =>
				   (app (fn (l,t) => adjustType(var1,d,e,t)) f2;
				    app (fn (l,t) => adjustType(var2,d,e,t)) f1;
				    var1 :=
				      OPEN{depth=d, eq=e,
					   kind=FLEX(merge_fields(true,true,f1,f2))};
				    var2 := INSTANTIATED(VARty var1))
			        | META =>
				   (app (fn (l,t) => adjustType(var2,d,e,t)) f1;
				    var1 := OPEN{kind=k1,depth=d,eq=e};
				    var2 := INSTANTIATED(VARty var1))
			 end
		      | _ => bug "unifyTyvars 2")
			 
	       | OPEN{kind=META,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=META,depth=d2,eq=e2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in var1 := OPEN{kind=META,depth=d,eq=e};
			     var2 := INSTANTIATED(VARty var1)
			 end
		      | _ => bug "unifyTyvars 3")

	       | _ => bug "unifyTyvars 4"
val _ = debugmsg ">>unifyTyvars"			 
     in if TU.eqTyvar(var1,var2) then ()
        else unify(sortVars(var1,var2))
    end

and instTyvar (var as ref(OPEN{kind=META,depth,eq}),ty,ety) =
      (case ety
         of WILDCARDty => ()
	  | _ => adjustType(var,depth,eq,ety);
       debugPPType("instTyvar ", VARty var);
       debugPPType("instTyvar to ", ty);
       (* Also need to check for circularity with ty here *)
       var := INSTANTIATED ty)

  | instTyvar (var as ref(OPEN{kind=FLEX fields,depth,eq}),ty,ety) =
      (case ety
	 of CONty(RECORDtyc field_names, field_types) =>
	      let val record_fields = ListPair.zip (field_names,field_types)
	       in app (fn t => adjustType(var,depth,eq,t)) field_types;
		  merge_fields(false,true,fields,record_fields);
		  var := INSTANTIATED ty
	      end
          | WILDCARDty => (* propagate WILDCARDty to the fields *)
	      (app (fn (lab,ty) => unifyTy(WILDCARDty,ty)) fields)
          | _ => raise Unify (TYP(VARty(var), ety)))

  | instTyvar (var as ref(i as SCHEME eq),ty,ety) =
      (adjustType(var,infinity,eq,ety);
       var := INSTANTIATED ty)

  | instTyvar (var as ref(i as LITERAL{kind,...}),ty,ety) =
      (case ety
	 of WILDCARDty => ()
	  | _ => 
	     if OLL.isLiteralTy(kind,ety)
	     then var := INSTANTIATED ty
	     else raise Unify (LIT i))   (* could return the ty for error msg*)

  | instTyvar (ref(i as UBOUND _),_,ety) =
      (case ety
         of WILDCARDty => ()
          | _ =>  raise Unify (UBV i))   (* could return the ty for error msg*)

  | instTyvar (ref(INSTANTIATED _),_,_) = bug "instTyvar: INSTANTIATED"
  | instTyvar (ref(LBOUND _),_,_) = bug "instTyvar: LBOUND"

(*
 * merge_fields(extra1,extra2,fields1,fields2):
 *
 *    This function merges the 2 sorted field lists.  Fields occuring
 * in both lists have their types unified.  If a field occurs in only
 * one list, say fields{i} then if extra{i} is true, an Unify error
 * is raised.
 *)
and merge_fields(extra1,extra2,fields1,fields2) =
    let fun extra allowed t =
	if not allowed
	then raise Unify REC
	else t
     in fieldwise(extra extra1, extra extra2, 
                  (fn (t1,t2) => (unifyTy(t1,t2); t1)),
		  fields1, fields2)
    end

end (* local *)
end (* structure Unify *)

