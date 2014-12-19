(* COPYRIGHT 1996 AT&T Bell Laboratories. *)
(* overload.sml *)

signature OVERLOAD =
sig
  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchScheme : Types.tyfun * Types.ty -> Types.ty

  val new : unit ->
	    { pushv : VarCon.var ref * SourceMap.region * ErrorMsg.complainer
		      -> Types.ty,
	      pushl : Types.ty -> unit,
	      resolve : StaticEnv.staticEnv -> unit }

  val debugging : bool ref (* = ElabControl.debugging = Control.Elab.debugging *)
end  (* signature OVERLOAD *)

structure Overload : OVERLOAD = 
struct

val debugging = ElabControl.ovlddebugging

local 
  structure EM = ErrorMsg
  structure BT = BasicTypes
  structure TU = TypesUtil
  structure ED = ElabDebug
  structure PP = PrettyPrintNew
  structure PU = PPUtilNew
  open VarCon Types
in

fun bug msg = ErrorMsg.impossible("Unify: "^msg)

fun debugMsg (msg: string) =
    ED.debugMsg debugging msg

val ppType = PPType.ppType StaticEnv.empty
fun debugPPType (msg,ty) =
    ED.debugPrint debugging (msg, ppType, ty)

(* matching a scheme against a target type to extract an indicator type
   for an overload declaration (in ElabCore) *)
fun matchScheme (TYFUN{arity,body}: tyfun, target: ty) : ty =
    (* Assert: arity = 1; target is a (pruned) monomorphic type *)
    let val tyref = ref UNDEFty (* holds unique instantiation of IBOUND 0 *)
	fun matchTyvar(ty: ty) : unit = 
	    case !tyref
	      of UNDEFty => tyref := ty
	       | ty' => if TU.equalType(ty,ty')
			then ()
 			else bug("this compiler was inadvertantly \
			          \distributed to a user who insists on \
 				  \playing with 'overload' declarations.")
        fun match(scheme:ty, target:ty) : unit =
	    case (TU.prune scheme, TU.prune target)
	      of ((IBOUND 0),ty) => matchTyvar ty
	       | (CONty(tycon1,args1), CONty(tycon2,args2)) =>
		   if TU.eqTycon(tycon1,tycon2)
		   then ListPair.app match (args1, args2)
		   else (match(TU.reduceType scheme, target)
			 handle TU.ReduceType =>
			   (match(scheme, TU.reduceType target)
			    handle TU.ReduceType =>
				   bug "matchScheme, match -- tycons "))
	       | _ => bug "TypesUtil.matchScheme > match"
    in 
        match(body,target);
	debugPPType("matchScheme type:", !tyref);
	!tyref
    end

(* overloaded functions *)
fun new () =
let val overloadedvars = ref (nil: (var ref * ErrorMsg.complainer * tyvar) list)
    val overloadedlits = ref (nil: ty list)
    fun pushvar (refvar as ref(OVLDvar{name,options,scheme}), region, err) = 
	let val indicators = map #indicator options
	    val tyvar = ref(OVLD{sources=[OVAR(name,region)],options=indicators})
	    val scheme' = TU.applyTyfun(scheme,[VARty tyvar])
	in
	    debugMsg ">>ovld-push";
	    map (fn ty => debugPPType("%%%",ty)) indicators;
	    overloadedvars := (refvar,err,tyvar) :: !overloadedvars;
	    debugPPType("<<ovld-push "^Symbol.name name, scheme');
	    scheme'
	end
      | pushvar _ = bug "Overload.push"

    fun pushlit ty_err =
	overloadedlits := ty_err :: !overloadedlits

    fun resolve env  =
       (* this resolveOverloaded implements defaulting behavior -- if more
	* than one variant matches the context type, the first one matching
	* (which will always be the first variant) is used as the default.
	* For defaulting to work correctly when matching different OVLD tyvars,
	* it is assumed that the ordering of options is consistent (e.g. between
	* operators like +, -, * ). *)
	let fun resolveOVLDvar(rv as ref(OVLDvar{name,options,...}),err,context) =
		let val contextTy = TU.headReduceType(VARty context)
		    val _ = debugPPType(">>resolveOVLDvar " ^ Symbol.name name ^
					", contextTy:", contextTy)
		    val (isCompatible, instantiate) =
			case contextTy
			 of VARty(tvar as ref(OVLD{options,...})) =>
			     (map (fn ty => debugPPType("$$$",ty)) options;
			      ((fn ty => TU.inClass(ty,options)),
			       (fn ty => tvar := INSTANTIATED ty)))
			  | _ =>
			    ((fn ty => TU.equalType(contextTy, ty)),
			     (fn ty => ()))
		    fun select ({indicator,variant}::rest) =
			if isCompatible indicator
			then (debugPPType("@resolveOVLDvar: match",indicator);
			      instantiate indicator;
			      rv := variant)
			else (debugPPType("@resolveOVLDvar: no match",indicator);
			      select rest)
		      | select nil =
			  err EM.COMPLAIN "overloaded variable not defined at type"
			    (fn ppstrm =>
			      (PPType.resetPPType();
			       PP.newline ppstrm;
			       PP.string ppstrm "symbol: "; 
			       PU.ppSym ppstrm name;
			       PP.newline ppstrm;
			       PP.string ppstrm "type: ";
			       PPType.ppType env ppstrm (VARty context)))
		in select options
		end

	    fun resolveOVLDlit ty =
		case ty
		  of VARty(tyvar as ref(OVLD{sources,options})) =>
		     (case options
		       of ty::_ =>
			  tyvar := INSTANTIATED(ty) (* default *)
			| nil => bug "resolveOVLDlit 1")
		   | VARty(ref(INSTANTIATED _)) => ()
		       (* already resolved by type checking *)
		   | _ => bug "resolveOVLDlit 2"
	in
	    app resolveOVLDlit (rev(!overloadedlits));
	    app resolveOVLDvar (rev(!overloadedvars))
	end
 in
    {pushv = pushvar, pushl = pushlit, resolve = resolve}
end (* new *)

end (* local *)
end (* structure Overload *)
