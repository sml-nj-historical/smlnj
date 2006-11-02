(* types-reformat.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * A utility function pulled out of typesutil.sml to break certain
 * unfortunate dependencies.  The function does not seem to be
 * used anywhere anyway.
 *)
structure TypesReformat : sig
    (* This used to be in typesutil, but it does not belong there. *)
  val reformat : Types.ty * Types.tycon list * DebIndex.depth ->
                 Types.ty * PLambdaType.tkind list * Types.tycpath list
end = struct

    open Types VarCon

    (* The reformat function is called inside translate.sml to reformat
     * a type abstraction packing inside PACKexp absyn. It is a hack. (ZHONG)
     *)
    fun reformat (ty, tycs, depth) = 
	let fun h ([], i, ks, ps, nts) = (rev ks, rev ps, rev nts)
	      | h (tc :: rest, i, ks, ps, nts) = let
		    fun noabs () =
			bug "non-abstract tycons seen in TU.reformat"
		in
		    case tc of
			GENtyc { stamp, arity, eq, path, kind, stub } =>
			(case kind of
			     ABSTRACT itc => let
				 val tk = LT.tkc_int arity
				 val tps =
				     TP_VAR (TVI.toExn
						 {depth=depth, num=i, kind=tk})
				 val nkind = FLEXTYC tps
				 val ntc =
				     GENtyc { stamp = stamp, arity = arity,
					      eq = eq, kind = nkind,
					      path = path, stub = NONE}
			     in
				 h (rest, i+1, tk::ks, (TP_TYC itc)::ps,
				    ntc::nts)
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

    val reformat =
	Stats.doPhase(Stats.makePhase "Compiler 047 reformat") reformat
end
