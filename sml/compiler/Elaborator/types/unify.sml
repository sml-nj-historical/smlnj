(* Copyright 1997 Bell Laboratories *)
(* unify.sml *)

signature UNIFY =
sig

  datatype unifyFail
    = CIRC of Types.tyvar * Types.ty * SourceMap.region * SourceMap.region
        (* circularity *)
    | TYC of Types.tycon * Types.tycon * SourceMap.region * SourceMap.region
        (* tycon mismatch *)
    | TYP of Types.ty * Types.ty * SourceMap.region * SourceMap.region
        (* type mismatch *)
    | UBV of Types.tvKind * Types.ty * SourceMap.region * SourceMap.region
        (* UBOUND match *)
    | OVLD_F of Types.ovldSource list   (* overload mismatch *)
    | OVLD_EQ of Types.ovldSource list  (* overload - equality mismatch *)
    | OVLD_UB of Types.ovldSource list  (* overload and user-bound ty var mismatch *)
    | EQ                                (* equality type required *)
    | REC                               (* record labels *)
    | UBVE of Types.tvKind              (* UBOUND, equality mismatch -- never used *)

  exception Unify of unifyFail

  val failMessage: unifyFail -> string

  val unifyTy : Types.ty * Types.ty * SourceMap.region  * SourceMap.region -> unit

  val debugging : bool ref

end (* signature UNIFY *)

(*** type unification ***)
structure Unify: UNIFY =
struct

val debugging = ElabControl.unidebugging

local
  structure S = Symbol
  structure T = Types
  structure TU = TypesUtil
  structure OLL = OverloadLit
  structure ED = ElabDebug
  open Types 

  fun bug msg = ErrorMsg.impossible("Unify: "^msg)

  (* debugging *)
  val say = Control_Print.say
  fun debugMsg (msg: string) =
      ED.debugMsg debugging msg

  val ppType = PPType.ppType StaticEnv.empty
  fun debugPPType (msg,ty) =
      ED.debugPrint debugging (msg, ppType, ty)

in

(* for the time being, not region instrumenting the EQ and REC failures *)
datatype unifyFail
  = CIRC of tyvar * ty * SourceMap.region * SourceMap.region  (* circularity *)
  | EQ                               (* equality type required *)
  | TYC of tycon * tycon * SourceMap.region * SourceMap.region
                                     (* tycon mismatch *)
  | TYP of ty * ty * SourceMap.region * SourceMap.region     (* type mismatch *)
  | OVLD_F of Types.ovldSource list  (* overload mismatch *)
  | OVLD_EQ of Types.ovldSource list (* overload - equality mismatch *)
  | OVLD_UB of Types.ovldSource list (* mismatch of OVLD and UBOUND tyvars *)
  | UBV of tvKind * ty * SourceMap.region * SourceMap.region  (* UBOUND match *)
  | UBVE of tvKind                   (* UBOUND, equality mismatch -- never used *)
  | REC                              (* record labels *)

fun failMessage (failure: unifyFail) =
    case failure
      of CIRC _ =>    "circularity"
       | EQ =>        "equality type required"
       | TYC _ =>     "tycon mismatch"
       | TYP _ =>     "type mismatch"
       | OVLD_F _ =>  "overload conflict"
       | OVLD_EQ _ => "overload - equality mismatch"
       | OVLD_UB _ => "overload - user bound tyvar" (* DBM: fixes bug 145 *)
       | UBVE _ =>    "UBOUND, equality mismatch"
       | UBV _ =>     "UBOUND match"
       | REC =>       "record labels"

exception Unify of unifyFail


(*************** misc functions *****************************************)

val eqLabel = Symbol.eq

(* eqLitKind : litKind -> bool.  Does a litKind support equality? *)
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
 * of an ERRORtyc should never be needed.
 *
 * [GK 5/7/07] The above note is not true. See bug271. Since an error
 * was already flagged, it seems harmless to return YES for the eqprop 
 * to avoid possibly spurious eqprop related warnings.  
 * 
 * Calling this function on a DEFtyc also produces an impossible because
 * the current eqprop scheme is insufficiently expressive to describe
 * the possibilities. (Eg: first argument must be an eq type but not
 * necessarily the second.)  Because of this, it is currently necessary to
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
  | fieldwise(just1,just2,combine,r1 as ((n1,t1)::rest1),r2 as ((n2,t2)::rest2)) =
      if eqLabel(n1,n2) then
	(n1,combine(t1,t2))::(fieldwise(just1,just2,combine,rest1,rest2))
      else if TU.gtLabel(n2,n1) then
	(n1,just1 t1)::(fieldwise(just1,just2,combine,rest1,r2))
      else
	(n2,just2 t2)::(fieldwise(just1,just2,combine,r1,rest2))


(* filter out the non-equality-types from a list of types *)
fun filterEq (nil) = nil
  | filterEq (ty::tys) =
    if EqTypes.isEqType ty then ty :: filterEq tys
    else filterEq tys


(*************** adjust function *****************************************)

(* propagate depth and eq while checking for circularities in the
 * type ty that is going to unify with tyvar var *)

fun adjustType (var,depth,eq,ty,reg1,reg2) =
    (* ASSERT: VARty var <> ty *)
    (* reg1 is for var, reg2 is for ty and may update through iter *)
    let val _ = debugPPType(">>adjustType: ",ty)
	fun iter _ (WILDCARDty,_) = ()
	  | iter eq (MARKty(ty, reg2'), _) = iter eq (ty, reg2')
	  | iter eq (ty' as VARty(var' as ref(info)), reg2) =
	      (case info
		 of INSTANTIATED ty => 
		      (debugMsg "adjustType INSTANTIATED";
		       iter eq (ty,reg2))
		  | OPEN{kind=k,depth=d,eq=e} =>
		      (* check for circularity, propagage eq and depth *)
		      if TU.eqTyvar(var,var')
		      then raise Unify (CIRC(var,ty',reg1,reg2))
		      else (case k
			      of FLEX fields =>
				  (* recurse into FLEX field types *)
				  app (fn (l,t) => adjustType(var,depth,e,t,reg1,reg2))
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
		  | OVLD{sources,options} =>
		      if TU.eqTyvar(var,var')
		      then bug "Unify.adjustType[OVLD]"
		         (* can't happen, because var can't be OVLD *)
		      else if eq
		      then (case filterEq options
		             of nil => (* no equality types in options *)
				raise Unify (OVLD_EQ sources)
			      | options' =>
			        var' := OVLD{sources=sources, options=options'})
		      else ()
		  | LBOUND _ => bug "unify:adjustType:LBOUND")
	  | iter eq (ty as CONty(DEFtyc{tyfun=TYFUN{body,...},...}, args), reg2) =
	      (app (fn t => iter false (t,reg2)) args; iter eq (TU.headReduceType ty, reg2))
	      (* A headReduceType here may cause instTyvar to 
	       * infinite loop if this CONty has a nonstrict arg 
	       * against which we are unifying/instantiating
	       * Because we may be instantiating to nonstrict 
	       * univariables, it is safer to do an occurrence 
	       * check on all the arguments. (typing/tests/20.sml)
	       * [GK 4/28/07] 
	       * iter should only do the occurrence check and 
	       * not propagate eq to the args. 
	       * MLRISC/library/dynamic-array.sml's checkArray
	       * is an example. [GK 2/24/08] *)
              (* Note that is involves redundancey -- iter may be, and in
               * general will be, applied to args twice -- rethink? *)
 	  | iter eq (CONty(tycon,args), reg2) =
	      (case tyconEqprop tycon
		 of OBJ => app (fn t => iter false (t,reg2)) args
		  | YES => app (fn t => iter eq (t,reg2)) args
		  | _ =>
		    if eq then raise Unify EQ
		    else app (fn t => iter false (t,reg2)) args)
 (* BUG? why don't these cases blow up (in tyconEqprop) when iter is applied
    to arguments that are unreduced applications of DEFtycs? *)
          | iter _ (POLYty _, _) = bug "Unify.adjustType[POLYty]"
          | iter _ (IBOUND _, _) = bug "Unify.adjustType[IBOUND]"
	  | iter _ _ = bug "adjustType 3"
     in iter eq (ty,reg2); debugMsg "<<adjustType"
    end

(*************** unify functions *****************************************)

(* OVLD can be instantiated to a compatible monotype or a narrowed
 *   OVLD (when unified with another OVLD)
 * UBOUND cannot be instantiated, but it's depth property can be reduced
 * OPEN/FLEX can merge with another FLEX or instantiate a META
 * OPEN/META can be instantiated to anything
 *)

(* reorder two tyvars in descending order according to the ordering
 * OVLD > UBOUND > OPEN/FLEX > OPEN/META *)
fun sortVars(v1 as ref i1, v2 as ref i2) =
    case (i1,i2)
      of (OVLD _, _) => (v1,v2)
       | (_, OVLD _) => (v2,v1)
       | (UBOUND _, _) => (v1,v2)
       | (_, UBOUND _) => (v2,v1)
       | (OPEN{kind=FLEX _,...}, _) => (v1,v2)
       | (_, OPEN{kind=FLEX _,...}) => (v2,v1)
       | _ => (v1,v2) (* both OPEN/META *)

(* unifyTy expects that there are no POLYtys with 0-arity; 
   CONty(DEFtyc, _) are reduced only if absolutely necessary. *)
fun unifyTy(type1, type2, reg1, reg2) =
    let val type1 = TU.prune type1
	val type2 = TU.prune type2
	val _ = debugPPType(">>unifyTy: type1: ",type1)
	val _ = debugPPType(">>unifyTy: type2: ",type2)
	fun unifyRaw(type1, type2, reg1, reg2) = 
	 case (TU.prune type1, TU.prune type2)
	  of (MARKty (ty1, reg1'), type2) => unifyRaw(TU.prune ty1, type2, reg1', reg2)
	   | (type1, MARKty (ty2, reg2')) => unifyRaw(type1, TU.prune ty2, reg1, reg2')
	      (* missing region args to unify, so MARKs are discarded *)
	   | (VARty var1, VARty var2) =>
	       unifyTyvars(var1, var2, reg1, reg2)
	         (* this used to take type1 and type2 as args *)
	   | (VARty var1, type2) =>       (* type2 may be WILDCARDty *)
	       instTyvar(var1, type2, reg1, reg2)
	   | (type1, VARty var2) =>       (* type1 may be WILDCARDty *)
	       instTyvar(var2, type1, reg2, reg1)
	   | (CONty(tycon1,args1), CONty(tycon2,args2)) =>
	       if TU.eqTycon(tycon1,tycon2) then
		   (* Because tycons are equal, they must have the 
		      same arity and strictness signatures. Thus lengths of args1 and
		      args2 are the same. Type abbrev. strictness
		      optimization. If tycons equal, then only check
		      strict arguments. [GK 4/28/07] *)
		   (case tycon1 
		     of DEFtyc{strict, ...} =>
			let fun unifyArgs([],[],[]) = ()
			      | unifyArgs(true::ss, ty1::tys1, ty2::tys2) =
				(unifyTy(ty1,ty2,reg1,reg2); unifyArgs(ss,tys1,tys2))
			      | unifyArgs(false::ss, _::tys1, _::tys2) =
				unifyArgs(ss,tys1,tys2)
			      | unifyArgs _ = 
				  bug "unifyTy: arg ty lists wrong length"
			in unifyArgs(strict,args1,args2)
			end
		      | _ => ListPair.app (fn (t1,t2) => unifyTy(t1,t2,reg1,reg2))
					  (args1,args2))
	       else raise Unify (TYC(tycon1,tycon2,reg1,reg2))
	  (* if one of the types is WILDCARDty, propagate it down into the
	   * other type to eliminate tyvars that might otherwise cause
	   * generalizeTy to complain. *)
	   | (WILDCARDty, CONty(_, args2)) => 
                app (fn x => unifyTy(x, WILDCARDty, reg1, reg2)) args2
           | (CONty(_, args1), WILDCARDty) =>
                app (fn x => unifyTy(x, WILDCARDty, reg1, reg2)) args1
	   | (WILDCARDty,_) => ()
	   | (_,WILDCARDty) => ()
	   | (ty1,ty2) => raise Unify (TYP(ty1,ty2,reg1,reg2))
    in (* first try unifying without reducing CONty(DEFtycs) *)
       unifyRaw(type1, type2, reg1, reg2) 
       handle Unify _ => 
         (* try head reducing type1 *)
         let val type1' = TU.headReduceType type1
         in unifyRaw(type1', type2, reg1, reg2)   (* regions? *)
            handle Unify _ => (* try head reducing type2 *)
                   unifyRaw(type1', TU.headReduceType type2, reg1,reg2) (* regions? *)
                   (* if unification still fails, then type1 and type2
                      really cannot be made to be equal *)
         end
    end

and unifyTyvars (var1: tyvar, var2: tyvar, reg1, reg2) =
    let fun unify(var1 as ref i1, var2 as ref i2) =
	    (* ASSERT: var1 <> var2  -- cf equality test in let-body of unifyTyvars, and
	     *         var1 =< var2 by sortVars, same reason
	     *         var1 and var2 are pruned, i.e. are not INSTANTIATED *)
	    case i1
	      of OVLD{sources,options} =>
		 (case i2
		   of OVLD{sources=sources2,options=options2} =>
		      (debugMsg ("@unifyTyvars[OVLD/OVLD] options: " ^
				 Int.toString(length options) ^ ", options2: " ^
				 Int.toString(length options2));
		       case (List.filter (fn ty => TU.inClass(ty,options2)) options)
			of nil =>
			   (debugMsg ("<<unifyTyvars[OVLD/OVLD] no options");
			    raise Unify (OVLD_F (sources@sources2)))
			 | options' =>
			   (debugMsg ("<<unifyTyvars[OVLD/OVLD] options': " ^
				      Int.toString(length options'));
			    var1 := OVLD{sources=sources@sources2,options=options'};
			    var2 := INSTANTIATED(MARKty(VARty var1, reg1))))
		    | OPEN{eq,...} =>
		      (debugMsg ("@unifyTyvars[OVLD/OPEN] eq: "^Bool.toString eq);
		       if eq then
			  (case filterEq options
			    of nil => raise Unify (OVLD_EQ sources)
			     | options' =>
			       (debugMsg ("<<unifyTyvars[OVLD/OPEN] " ^
					  Bool.toString(length options' < length options));
				var1 := OVLD{sources=sources, options=options'};
				var2 := INSTANTIATED(MARKty(VARty var1, reg1))))
		       else var2 := INSTANTIATED(MARKty(VARty var1, reg1)))
		    | UBOUND _ => raise Unify (OVLD_UB sources))

	       | UBOUND {depth=d1,eq=e1,name} =>
		 (* Note: UBOUND tyvars unify only if equal *)
		  (case i2
		     of OPEN{kind=META,eq=e2,depth=d2} =>
			 if e1 orelse (not e2)
			 then (if d2 < d1
				   then var1 := UBOUND{depth=d2,eq=e1,name=name}
				   else ();
			       var2 := INSTANTIATED (MARKty(VARty var1, reg1)))
			 else raise Unify (UBV(i1,VARty var2,reg1,reg2))
		      | _ => raise Unify (UBV(i1,VARty var2,reg1,reg2)))

	       | OPEN{kind=k1 as FLEX f1,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=k2,eq=e2,depth=d2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in case k2
			       of FLEX f2 =>
				   (app (fn (l,t) => adjustType(var1,d,e,t,reg1,reg2)) f2;
				    app (fn (l,t) => adjustType(var2,d,e,t,reg1,reg2)) f1;
				    var1 :=
				      OPEN{depth=d, eq=e,
					   kind=FLEX(merge_fields(true,true,f1,f2,reg1,reg2))};
				    var2 := INSTANTIATED(MARKty(VARty var1, reg1)))
			        | META =>
				   (app (fn (l,t) => adjustType(var2,d,e,t,reg1,reg2)) f1;
				    var1 := OPEN{kind=k1,depth=d,eq=e};
				    var2 := INSTANTIATED(MARKty(VARty var1, reg1)))
			 end
		      | _ => bug "unifyTyvars 2")
			 
	       | OPEN{kind=META,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=META,depth=d2,eq=e2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in var1 := OPEN{kind=META,depth=d,eq=e};
			     var2 := INSTANTIATED(MARKty(VARty var1, reg1))
			 end
		      | _ => bug "unifyTyvars 3")

	       | _ => bug "unifyTyvars 4"
        val _ = debugMsg ">>unifyTyvars"			 
     in if TU.eqTyvar(var1,var2) then ()
        else unify(sortVars(var1,var2))
    end

(* instTyvar: tyvar * ty * srcloc * srcloc -> unit
 * instTyvar(tv,ty,reg1,reg2) -- instantiate tyvar tv to type ty.
 * ty is not necessarily head normal form.
 * ASSERT: ty is not a VARty (otherwise unifyTyvars would have been
 * used instead. *)
and instTyvar (var as ref(OPEN{kind=META,depth,eq}), ty, reg1, reg2) =
      (case ty
         of WILDCARDty => ()
	  | MARKty(ty1, reg2') => instTyvar (var, ty1, reg1, reg2')
	  | _ => adjustType(var, depth, eq, ty, reg1, reg2);
       debugPPType("@instTyvar[OPEN] from:", VARty var);
       debugPPType("@instTyvar[OPEN] to:", ty);
       (* Also need to check for circularity with ty here -- done by adjustType. *)
       var := INSTANTIATED ty)

  | instTyvar (var as ref(OPEN{kind=FLEX fields,depth,eq}), ty, reg1, reg2) =
      let val ty' = TU.headReduceType ty (* try to reduce to a record type *)
       in case ty'
	   of CONty(RECORDtyc field_names, field_types) =>
                let val record_fields = ListPair.zip (field_names,field_types)
                 in app (fn t => adjustType(var,depth,eq,t,reg1,reg2)) field_types;
                    merge_fields(false, true, fields, record_fields, reg1, reg2);
                    var := INSTANTIATED ty
                end
	    | MARKty(ty1, reg2') => instTyvar (var, ty1, reg1, reg2')
            | WILDCARDty => (* propagate WILDCARDty to the fields *)
	       (app (fn (lab,ty) => unifyTy(WILDCARDty,ty,reg1,reg2)) fields)
            | _ => raise Unify (TYP(VARty(var), ty, reg1, reg2))
      end

  | instTyvar (var as ref(i as OVLD{sources,options}), ty, reg1, reg2) =
      (debugPPType(">>instTyvar[OVLD]",ty);
       case TU.headReduceType ty
	 of WILDCARDty => ()
	  | MARKty(ty1, reg2') => instTyvar(var, ty1, reg1, reg2')
	  | ty' =>
	    (debugPPType("instTyvar[OVLD 3]",ty');
            if TU.inClass(ty',options)
	    then (debugPPType("instTyvar[OVLD-inClass]",ty');
		  var := INSTANTIATED(ty'))
	    else (debugMsg ("instTyvar[OVLD] bad options ["^Int.toString(length options)^"]");
		  app (fn t => debugPPType("***", t)) options;
		  raise Unify (OVLD_F sources))))

  | instTyvar (var as ref(i as UBOUND _), ty, reg1, reg2) =
      (case ty
         of WILDCARDty => ()
	  | MARKty(ty1, reg2') => instTyvar(var, ty1, reg1, reg2')
          | _ => raise Unify (UBV(i, ty, reg1, reg2)))
              (* could return the ty for error msg*)

  | instTyvar (ref(INSTANTIATED _),_,_,_) = bug "instTyvar: INSTANTIATED"
  | instTyvar (ref(LBOUND _),_,_,_) = bug "instTyvar: LBOUND"

(*
 * merge_fields(extra1,extra2,fields1,fields2):
 *
 *    This function merges the 2 sorted field lists.  Fields occuring
 * in both lists have their types unified.  If a field occurs in only
 * one list, say fields{i} then if extra{i} is true, an Unify error
 * is raised.
 *)
and merge_fields(extra1, extra2, fields1, fields2, reg1, reg2) =
    let fun extra allowed t =
	if not allowed
	then raise Unify REC
	else t
     in fieldwise(extra extra1, extra extra2, 
                  (fn (t1,t2) => (unifyTy(t1, t2, reg1, reg2); t1)),
		  fields1, fields2)
    end

end (* local *)
end (* structure Unify *)
