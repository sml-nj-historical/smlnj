(* COPYRIGHT 1996 AT&T Bell Laboratories. *)
(* overload.sml *)

signature OVERLOAD = sig
    val new : unit ->
	      { push : VarCon.var ref * ErrorMsg.complainer -> Types.ty,
		resolve : StaticEnv.staticEnv -> unit }
end  (* signature OVERLOAD *)

structure Overload : OVERLOAD = 
struct

local 
  structure EM = ErrorMsg
  structure BT = BasicTypes
  structure TU = TypesUtil
  structure ED = ElabDebug
  structure PP = PrettyPrintNew
  structure PU = PPUtilNew
  open VarCon Types
in

fun bug msg = EM.impossible("Overload: "^msg)

type subst = (tyvar * tvKind) list

exception SoftUnify

fun copyScheme (tyfun as TYFUN{arity,...}) : ty * ty =
  let fun typeArgs n = if n>0 then TU.mkSCHEMEty() :: typeArgs(n-1) else []
      val tvs = typeArgs arity
   in (TU.applyTyfun(tyfun,tvs),if arity>1 then BT.tupleTy tvs else hd tvs)
  end

fun rollBack subst =
  let fun loop (nil,trace) = trace
        | loop (((tv as ref kind),oldkind)::subst,trace) =
            (tv := oldkind;
             loop(subst,(tv,kind)::trace))
   in loop(subst,nil)
  end

fun redoSubst nil = ()
  | redoSubst ((tv as ref(OPEN{kind=META, ...}),INSTANTIATED ty)::rest) =
      (tv := INSTANTIATED ty; redoSubst rest)
  | redoSubst (_) = bug "Overload--redoSubst"

fun softUnify(ty1: ty, ty2: ty): unit =
  let val subst: subst ref = ref nil
      fun softInst(tv as ref info: tyvar, ty: ty) : unit =
	    let fun scan eq (ty: ty) : unit =  (* simple occurrence check *)
		    case ty
		      of VARty(tv') => 
			   if TU.eqTyvar(tv, tv')
			   then raise SoftUnify
			   else (case tv'
				   of ref(OPEN{kind=FLEX fields,...}) =>
					(* DBM: can this happen? *)
					app (fn (_,ty') => scan eq ty') fields
				    | _ => ())
		       | CONty(tycon, args) =>
			 (* check equality property if necessary *)
			   if eq
			   then (case tycon
			           of DEFtyc _ => 
				       scan eq (TU.headReduceType ty)
		                    | GENtyc gt =>
				       (case ! (#eq gt)
                                          of YES => app (scan eq) args
					   | OBJ => app (scan false) args
					      (* won't happen *)
					   | _ => raise SoftUnify)
                                    | _ => raise SoftUnify) (* won't happen? *)
			   else app (scan eq) args
		       | ty => ()  (* propagate error *)
	     in case info
		  of (SCHEME eq | OPEN{kind=META,eq,...}) =>
		      (scan eq ty;
		       subst := (tv, info)::(!subst);
		       tv := INSTANTIATED ty)
		   | _ => raise SoftUnify
	    end
	
	fun unify(ty1: ty, ty2: ty): unit =
	    let val ty1 = TU.prune ty1
		and ty2 = TU.prune ty2
	     in case (ty1,ty2)
		  of (WILDCARDty, _) => ()  (* wildcards unify with anything *)
		   | (_, WILDCARDty) => ()  (* wildcards unify with anything *)
		   | (VARty(tv1),VARty(tv2)) =>
		       if TU.eqTyvar(tv1,tv2) then () else softInst(tv1,ty2)
		   | (VARty(tv1),_) => softInst(tv1,ty2)
		   | (_,VARty(tv2)) => softInst(tv2,ty1)
		   | (CONty(tycon1, args1), CONty(tycon2, args2)) =>
		       if TU.eqTycon(tycon1, tycon2)
		       then unifyLists(args1, args2)
		       else (unify(TU.reduceType ty1, ty2)
			     handle TU.ReduceType => 
			       unify(ty1, TU.reduceType ty2)
			       handle TU.ReduceType => raise SoftUnify)
		   | _ => raise SoftUnify
	    end
	
	and unifyLists([],[]) = ()
	  | unifyLists(ty1::rest1, ty2::rest2) = 
	      (unify(ty1,ty2); unifyLists(rest1,rest2))
	  | unifyLists(_) = raise SoftUnify

     in unify(ty1,ty2)
	  handle SoftUnify => (rollBack(!subst); raise SoftUnify)
    end

(* overloaded functions *)
fun new () = let
    val overloaded = ref (nil: (var ref * ErrorMsg.complainer * ty) list)
    fun push (refvar as ref(OVLDvar{options,scheme,...}), err) = 
	let val (scheme',ty) = copyScheme(scheme)
	in
	    overloaded := (refvar,err,ty) :: !overloaded;
	    scheme'
	end
      | push _ = bug "overload.1"

    (* this resolveOverloaded implements defaulting behavior -- if more
     * than one variant matches the context type, the first one matching
     * (which will always be the first variant) is used as the default *)
    fun resolve env  =
	let fun resolveOVLDvar(rv as ref(OVLDvar{name,options,...}),err,context) =
		let fun firstMatch({indicator, variant}::rest) =
			let val (nty,_) = TU.instantiatePoly indicator
			in (softUnify(nty, context); rv := variant)
			   handle SoftUnify => firstMatch(rest)
			end
		      | firstMatch(nil) =
			(err EM.COMPLAIN "overloaded variable not defined at type"
			     (fn ppstrm =>
				 (PPType.resetPPType();
				  PP.newline ppstrm;
				  PP.string ppstrm "symbol: "; 
				  PU.ppSym ppstrm name;
				  PP.newline ppstrm;
				  PP.string ppstrm "type: ";
				  PPType.ppType env ppstrm context));
			     ())

		in firstMatch(!options)
		end
	      | resolveOVLDvar _ = bug "overload.2"
	in
	    app resolveOVLDvar (!overloaded)
	end
in
    { push = push, resolve = resolve }
end (* new *)

end (* local *)
end (* structure Overload *)
