(* infcnv.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Expand out any remaining occurences of test_inf, trunc_inf, extend_inf,
 * and copy_inf.  These primops carry a second argument which is a
 * function that performs the operation for 32 bit precision.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure InfCnv : sig

    val elim : { function : CPS.function,
		 mkKvar : unit -> LambdaVar.lvar,     (* new cont var. *)
		 mkI32var : unit -> LambdaVar.lvar }  (* new int32 var. *)
	       -> CPS.function

end = struct

    structure C = CPS

    fun boxIntTy sz = C.NUMt{tag = false, sz = sz}

    val zero = C.NUM{ival = 0, ty={tag = true, sz = Target.defaultIntSz}}
    val one  = C.NUM{ival = 1, ty={tag = true, sz = Target.defaultIntSz}}

    fun elim { function = cfun, mkKvar, mkI32var } = let
	fun cexp (C.RECORD (rk, xl, v, e)) =
	      C.RECORD (rk, xl, v, cexp e)
	  | cexp (C.SELECT (i, x, v, t, e)) =
	      C.SELECT (i, x, v, t, cexp e)
	  | cexp (C.OFFSET (i, v, x, e)) =
	      C.OFFSET (i, v, x, cexp e)
	  | cexp (C.APP (x, xl)) =
	      C.APP (x, xl)
	  | cexp (C.FIX (fl, e)) =
	      C.FIX (map function fl, cexp e)
	  | cexp (C.SWITCH (x, v, el)) =
	      C.SWITCH (x, v, map cexp el)
	  | cexp (C.BRANCH (b, xl, v, e1, e2)) =
	      C.BRANCH (b, xl, v, cexp e1, cexp e2)
	  | cexp (C.SETTER (s, xl, e)) =
	      C.SETTER (s, xl, cexp e)
	  | cexp (C.LOOKER (l, xl, v, t, e)) =
	      C.LOOKER (l, xl, v, t, cexp e)
	  | cexp (C.PURE (C.P.copy_inf 32, [x, f], v, t, e)) =
	      let val k = mkKvar ()
		  val e' = cexp e
	      in
		  C.FIX ([(C.CONT, k, [v], [t], e')],
			 C.APP (f, [C.VAR k, x, zero]))
	      end
	  | cexp (C.PURE (C.P.extend_inf 32, [x, f], v, t, e)) =
	      let val k = mkKvar ()
		  val e' = cexp e
	      in
		  C.FIX ([(C.CONT, k, [v], [t], e')],
			 C.APP (f, [C.VAR k, x, one]))
	      end
	  | cexp (C.ARITH (C.P.test_inf 32, [x, f], v, t, e) |
		  C.PURE (C.P.trunc_inf 32, [x, f], v, t, e)) =
	      let val k = mkKvar ()
		  val e' = cexp e
	      in
		  C.FIX ([(C.CONT, k, [v], [t], e')], C.APP (f, [C.VAR k, x]))
	      end
	  | cexp (C.ARITH (C.P.test_inf i, [x, f], v, t, e)) =
	      let val k = mkKvar ()
		  val v' = mkI32var ()
		  val e' = cexp e
	      in
		  C.FIX ([(C.CONT, k, [v'], [boxIntTy 32],  (* 64BIT: FIXME *)
			   C.ARITH (C.P.test (32, i), [C.VAR v'], v, t, e'))],
			 C.APP (f, [C.VAR k, x]))
	      end
	  | cexp (C.ARITH (a, xl, v, t, e)) =
	      C.ARITH (a, xl, v, t, cexp e)
	  | cexp (C.PURE (C.P.trunc_inf i, [x, f], v, t, e)) =
	      let val k = mkKvar ()
		  val v' = mkI32var ()
		  val e' = cexp e
	      in
		  C.FIX ([(C.CONT, k, [v'], [boxIntTy 32],  (* 64BIT: FIXME *)
			   C.PURE (C.P.trunc (32, i), [C.VAR v'], v, t, e'))],
			 C.APP (f, [C.VAR k, x]))
	      end
	  | cexp (C.PURE (C.P.copy_inf i, [x, f], v, t, e)) =
	      let val k = mkKvar ()
		  val v' = mkI32var ()
		  val e' = cexp e
	      in
		  C.FIX ([(C.CONT, k, [v], [t], e')],
			 C.PURE (C.P.copy (i, 32), [x], v', boxIntTy 32,  (* 64BIT: FIXME *)
				 C.APP (f, [C.VAR k, C.VAR v', zero])))
	      end
	  | cexp (C.PURE (C.P.extend_inf i, [x, f], v, t, e)) =
	      let val k = mkKvar ()
		  val v' = mkI32var ()
		  val e' = cexp e
	      in
		  C.FIX ([(C.CONT, k, [v], [t], e')],
			 C.PURE (C.P.extend (i, 32), [x], v', boxIntTy 32,  (* 64BIT: FIXME *)
				 C.APP (f, [C.VAR k, C.VAR v', one])))
	      end
	  | cexp (C.PURE (p, xl, v, t, e)) =
	      C.PURE (p, xl, v, t, cexp e)
	  | cexp (C.RCC (k, s, p, xl, vtl, e)) =
	      C.RCC (k, s, p, xl, vtl, cexp e)

	and function (fk, f, vl, tl, e) = (fk, f, vl, tl, cexp e)
    in
	function cfun
    end
end
