(* overload.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

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

    structure EM = ErrorMsg
    structure BT = BasicTypes
    structure TU = TypesUtil
    structure ED = ElabDebug
    structure PP = PrettyPrintNew
    structure PU = PPUtilNew
    structure Ty = Types
    structure VC = VarCon

    fun bug msg = ErrorMsg.impossible("Unify: "^msg)

    fun debugMsg (msg: string) = ED.debugMsg debugging msg

    val ppType = PPType.ppType StaticEnv.empty
    fun debugPPType (msg, ty) = ED.debugPrint debugging (msg, ppType, ty)

  (* matching a scheme against a target type to extract an indicator type
   * for an overload declaration (in ElabCore)
   *)
    fun matchScheme (Ty.TYFUN{arity, body}: Ty.tyfun, target: Ty.ty) : Ty.ty = let
	(* Assert: arity = 1; target is a (pruned) monomorphic type *)
	  val tyref = ref Ty.UNDEFty (* holds unique instantiation of IBOUND 0 *)
	  fun matchTyvar(ty: Ty.ty) : unit = (case !tyref
		 of Ty.UNDEFty => tyref := ty
		  | ty' => if TU.equalType(ty, ty')
		      then ()
		      else bug("this compiler was inadvertantly \
				\distributed to a user who insists on \
				\playing with 'overload' declarations.")
		(* end case *))
	  fun match(scheme:Ty.ty, target:Ty.ty) : unit = (
		case (TU.prune scheme, TU.prune target)
		 of ((Ty.IBOUND 0),ty) => matchTyvar ty
		  | (Ty.CONty(tycon1,args1), Ty.CONty(tycon2,args2)) =>
		      if TU.eqTycon(tycon1,tycon2)
			then ListPair.app match (args1, args2)
			else (match(TU.reduceType scheme, target)
			     handle TU.ReduceType =>
			       (match(scheme, TU.reduceType target)
				handle TU.ReduceType =>
				  bug "matchScheme, match -- tycons "))
		  | _ => bug "TypesUtil.matchScheme > match"
		(* end case *))
	  in
	    match(body,target);
	    debugPPType("matchScheme type:", !tyref);
	    !tyref
	  end

  (* overloaded functions *)
    fun new () = let
	  val overloadedvars = ref (nil: (VC.var ref * ErrorMsg.complainer * Ty.tyvar) list)
	  val overloadedlits = ref (nil: Ty.ty list)
	(* push an overloaded variable onto the var list *)
	  fun pushvar (refvar as ref(VC.OVLDvar{name,options,scheme}), region, err) = let
	        val indicators = map #indicator options
		val tyvar = ref(Ty.OVLD{sources=[Ty.OVAR(name,region)],options=indicators})
		val scheme' = TU.applyTyfun(scheme,[Ty.VARty tyvar])
		in
		  debugMsg ">>ovld-push";
		  map (fn ty => debugPPType("%%%",ty)) indicators;
		  overloadedvars := (refvar,err,tyvar) :: !overloadedvars;
		  debugPPType("<<ovld-push "^Symbol.name name, scheme');
		  scheme'
		end
	    | pushvar _ = bug "Overload.push"
      (* push an overloaded literal onto the var list *)
	fun pushlit ty_err = overloadedlits := ty_err :: !overloadedlits
      (* resolve overloadings *)
	fun resolve env = let
	    (* this function implements defaulting behavior -- if more
	     * than one variant matches the context type, the first one matching
	     * (which will always be the first variant) is used as the default.
	     * For defaulting to work correctly when matching different OVLD tyvars,
	     * it is assumed that the ordering of options is consistent (e.g., between
	     * operators like +, -, and * ).
	     *)
	      fun resolveOVLDvar(rv as ref(VC.OVLDvar{name,options,...}), err, context) =
		    let
		    val contextTy = TU.headReduceType(Ty.VARty context)
		    val _ = debugPPType(concat[
			    ">>resolveOVLDvar ", Symbol.name name, ", contextTy:"
			  ], contextTy)
		    val (isCompatible, instantiate) = (case contextTy
			   of Ty.VARty(tvar as ref(Ty.OVLD{options,...})) => (
				app (fn ty => debugPPType("$$$",ty)) options;
				((fn ty => TU.inClass(ty,options)),
				 (fn ty => tvar := Ty.INSTANTIATED ty)))
			    | _ =>
				((fn ty => TU.equalType(contextTy, ty)),
				 (fn ty => ()))
			  (* end case *))
		    fun select ({indicator,variant}::rest) =
			  if isCompatible indicator
			    then (debugPPType("@resolveOVLDvar: match",indicator);
				  instantiate indicator;
				  rv := variant)
			    else (debugPPType("@resolveOVLDvar: no match",indicator);
				  select rest)
		      | select [] =
			  err EM.COMPLAIN "overloaded variable not defined at type"
			    (fn ppstrm =>
			      (PPType.resetPPType();
			       PP.newline ppstrm;
			       PP.string ppstrm "symbol: ";
			       PU.ppSym ppstrm name;
			       PP.newline ppstrm;
			       PP.string ppstrm "type: ";
			       PPType.ppType env ppstrm (Ty.VARty context)))
		    in
		      select options
		    end
	    (* resolve overloaded literals *)
	      fun resolveOVLDlit ty = (case ty
		     of Ty.VARty(tyvar as ref(Ty.OVLD{sources,options})) => (
		          case options
			   of ty::_ => tyvar := Ty.INSTANTIATED ty (* default *)
			    | [] => bug "resolveOVLDlit 1"
			  (* end case *))
		      | Ty.VARty(ref(Ty.INSTANTIATED _)) => ()
			  (* already resolved by type checking *)
		      | _ => bug "resolveOVLDlit 2"
		    (* end case *))
	      in
		app resolveOVLDlit (rev(!overloadedlits));
		app resolveOVLDvar (rev(!overloadedvars))
	      end
	  in
	    {pushv = pushvar, pushl = pushlit, resolve = resolve}
	  end (* new *)

  end (* structure Overload *)
