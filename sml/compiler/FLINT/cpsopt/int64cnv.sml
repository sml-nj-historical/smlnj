(* num64cnv.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This module supports the 64-bit int/word types on 32-bit machines
 * by expanding then to pairs of 32-bit words and replacing the primitive
 * operations with 32-bit code.
 *)

structure Num64Cvt : sig

  (* eliminate 64-bit literals and operations on a 32-bit machine; this function is
   * the identity on 64-bit machines.
   *)
    val elim : CPS.function -> CPS.function

  end = struct

    structure C = CPS
    structure P = C.P
    structure LV = LambdaVar

    val pairTy = C.PTRt C.RPT
    val box32Ty = C.PTRt C.VPT
    val raw32Ty = C.NUMt{sz = 32, tag = false}	(* assuming a 32-bit machine *)
    val si32 = P.INT 32
    val ui32 = P.UINT 32

  (* split a 64-bit integer/word literal into two 32-bit unsigned values.  We assume
   * that the argument is in the range -2^63 .. 2^64-1, which is the union of the ranges
   * of Int64.int and Word64.word.
   *)
    fun split (n : IntInf.int) = let
	  val n = if (n < 0) then 0x10000000000000000 + n else n
	  val hi = C.NUM{ival = IntInf.~>>(n, 0w32), ty = {sz = 32, tag = false}}
	  val lo = C.NUM{ival = IntInf.andb(n, 0xffffffff), ty = {sz = 32, tag = false}}
	  in
	    (hi, lo)
	  end

  (* short names for various CPS constructs *)
    val zero = C.NUM{ival=0, ty={sz = 32, tag = false}}
    val one = C.NUM{ival=1, ty={sz = 32, tag = false}}
    fun num n = C.NUM{ival=n, ty={sz = 32, tag = false}}
    fun uIf (oper, a, b, tr, fl) = (* unsigned conditional *)
	  C.BRANCH(P.cmp{oper=oper, kind=ui32}, [a, b], LV.mkLvar(), tr, fl)
    fun sIf (oper, a, b, tr, fl) = (* signed conditional *)
	  C.BRANCH(P.cmp{oper=oper, kind=si32}, [a, b], LV.mkLvar(), tr, fl)
    fun ifzero (v, tr, fl) = uIf(P.eql, v, zero, tr, fl)
    fun pure (rator, args, ty, k) = let
	  val x = LV.mkVar()
	  in
	    C.PURE(rator, args, x, ty, k(C.VAR x))
	  end

  (* bind a continuation around a cexp to avoid code duplication; `res` is the variable
   * to use as a parameter for the code `cexp`.
   *)
    fun join (res, cty, cexp, k) = let
	  val fnId = LV.mkLvar()
	  in
	    C.FIX([(C.CONT, fnId, [res], [cty], cexp)], k (fn v => C.APP(fnId, [v])))
	  end

  (* given two 32-bit values that comprise a 64-bit number and a continuation `k`,
   * make code to create the 64-bit object
   *)
    fun to64 (hi, lo, k) = let
	  val pair = LV.mkLvar()
	  in
	    pure(P.i32wrap, [lo], box32Ty, fn lo' =>
	    pure(P.i32wrap, [hi], box32Ty, fn hi' =>
	      C.RECORD(C.RK_RECORD, [(hi', C.OFFp 0), (lo', C.OFFp 0)], pair, k(C.VAR pair))))
	  end

  (* given a 64-bit object and a continuation `k`, make code to unpackage the value into
   * two 32-bit values, which are passed to `k`.
   *)
    fun from64 (n, k) =
	  C.SELECT(0, n, hi, box32Ty,
	  pure(P.i32unwrap, [hi], raw32Ty, fn hi' =>
	  C.SELECT(1, n, lo, box32Ty,
	  pure(P.i32unwrap, [lo], raw32Ty, fn lo' =>
	    k (hi', lo')))))

  (* given a 64-bit object and a continuation `k`, make code to unpackage the low 32 word,
   * which is passed to `k`.
   *)
    fun getLo32 (n, k) =
	  C.SELECT(1, n, lo, box32Ty,
	  pure(P.i32unwrap, [lo], raw32Ty, fn lo' =>
	    k lo'))

  (* given a 64-bit object and a continuation `k`, make code to unpackage the high 32 word,
   * which is passed to `k`.
   *)
    fun getHi32 (n, k) =
	  C.SELECT(0, n, hi, box32Ty,
	  pure(P.i32unwrap, [hi], raw32Ty, fn hi' =>
	    k hi'))

  (* split a 32-bit value into two 16-bit values *)
    fun split32 (n, k) =
	  pure(P.rshift, [n, num 16], fn hi =>
	  pure(P.andb, [n, num 0xffff], fn lo =>
	    k (hi, lo)))

  (***** Word64 primitive operations *****)

(* Word64 primops from primopmap.sml
       ("w64mul",	P.pure_arith{oper=P.*, kind=P.UINT 64})
       ("w64div",	P.pure_arith{oper=P./, kind=P.UINT 64})
       ("w64mod",	P.pure_arith{oper=P.rem, kind=P.UINT 64})
       ("w64add",	P.pure_arith{oper=P.+, kind=P.UINT 64})
       ("w64sub",	P.pure_arith{oper=P.-, kind=P.UINT 64})
       ("w64orb",	P.pure_arith{oper=P.orb, kind=P.UINT 64})
       ("w64xorb",	P.pure_arith{oper=P.xorb, kind=P.UINT 64})
       ("w64andb",	P.pure_arith{oper=P.andb, kind=P.UINT 64})
       ("w64notb",	P.pure_arith{oper=P.notb, kind=P.UINT 64})
       ("w64neg",	P.pure_arith{oper=P.~, kind=P.UINT 64})
       ("w64rshift",	P.pure_arith{oper=P.rshift, kind=P.UINT 64})
       ("w64rshiftl",   P.pure_arith{oper=P.rshiftl, kind=P.UINT 64})
       ("w64lshift",	P.pure_arith{oper=P.lshift, kind=P.UINT 64})
       ("w64gt",	P.cmp{oper=P.>, kind=P.UINT 64})
       ("w64ge",	P.cmp{oper=P.>=, kind=P.UINT 64})
       ("w64lt",	P.cmp{oper=P.<, kind=P.UINT 64})
       ("w64le",	P.cmp{oper=P.<=, kind=P.UINT 64})
       ("w64eq",	P.cmp{oper=P.eql, kind=P.UINT 64})
       ("w64ne",	P.cmp{oper=P.neq, kind=P.UINT 64})
 *)

(*
    fun mul64 ((hi1, lo1), (hi2, lo2)) =
        let val ((a1, b1), (c1, d1)) = (split16 hi1, split16 lo1)
            val ((a2, b2), (c2, d2)) = (split16 hi2, split16 lo2)
            val dd = d1 * d2
            val (cd, dc) = (c1 * d2, d1 * c2)
            val (bd, cc, db) = (b1 * d2, c1 * c2, d1 * b2)
            val (ad, bc, cb, da) = (a1 * d2, b1 * c2, c1 * b2, d1 * a2)
            val diag0 = dd
            val diag1 = cd + dc
            val diag2 = bd + cc + db
            val diag3 = ad + bc + cb + da
            val lo = diag0 + (diag1 << 0w16)
            val diag1carry = if diag1 < cd then 0wx10000 else 0w0
            val locarry = if lo < diag0 then 0w1 else 0w0
            val hi = (diag1 >> 0w16) + diag2 + (diag3 << 0w16)
                     + locarry + diag1carry
        in (hi, lo)
        end
*)
    fun w64Mul ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  split16 (hi1, fn (a1, b1) =>
	  split16 (lo1, fn (c1, d1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  split16 (hi1, fn (a2, b2) =>
	  split16 (lo1, fn (c2, d2) =>
	    pure (P.*, [d1, d2], fn dd =>
	    pure (P.*, [c1, d2], fn cd =>
	    pure (P.*, [d1, c2], fn dc =>
	    pure (P.*, [b1, d2], fn bd =>
	    pure (P.*, [c1, c2], fn cc =>
	    pure (P.*, [d1, b2], fn db =>
	    pure (P.*, [a1, d2], fn ad =>
	    pure (P.*, [b1, c2], fn bc =>
	    pure (P.*, [c1, b2], fn cb =>
	    pure (P.*, [d1, a2], fn da =>
	    pure (P.+, [cd, dc], fn diag1 =>
	    pure (P.+, [bd, cc], fn t1 =>
	    pure (P.+, [t1, db], fn diag2 =>
	    pure (P.+, [ad, bc], fn t2 =>
	    pure (P.+, [t2, cb], fn t3 =>
	    pure (P.+, [t3, da], fn diag4 =>
	    pure (P.lshift, [diag1, num 16], fn t4 =>
	    pure (P.+, [dd, t4], fn lo' =>

    fun w64Div ([n1, n2], res, cty, cexp) =

    fun w64Mod ([n1, n2], res, cty, cexp) =

(*
    fun add64 ((hi1, lo1), (hi2, lo2)) =
        let val (lo, hi) = (lo1 + lo2, hi1 + hi2)
        in (if lo < lo1 then hi + 0w1 else hi, lo)
        end
*)
    fun w64Add ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	    pure(P.+, [lo1, lo2], fn lo =>
	    pure(P.+, [hi1, hi2], fn hi =>
	      uIf(P.<, lo, lo1,
		pure(P.+, [hi, one], fn hi' => to64(hi', lo, k)),
		to64(hi, lo, k)))))))

(*
    fun sub64 ((hi1, lo1), (hi2, lo2)) =
        let val (lo, hi) = (lo1 - lo2, hi1 - hi2)
        in (if lo1 < lo then hi - 0w1 else hi, lo)
        end
*)
    fun w64Sub ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure(P.-, [lo1, lo2], fn lo =>
	  pure(P.-, [hi1, hi2], fn hi =>
	    uIf(P.<, lo1, lo,
	      pure(P.-, [hi, one], fn hi' => to64(hi', lo, k)),
	      to64(hi, lo, k)))))))

    fun w64Orb ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure(P.orb, [lo1, lo2], fn lo =>
	  pure(P.orb, [hi1, hi2], fn hi =>
	    to64 (hi, lo, k))))))

    fun w64Xorb ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure(P.xorb, [lo1, lo2], fn lo =>
	  pure(P.xorb, [hi1, hi2], fn hi =>
	    to64 (hi, lo, k))))))

    fun w64Andb ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64 (n1, fn (hi1, lo1) =>
	  from64 (n2, fn (hi2, lo2) =>
	  pure(P.andb, [lo1, lo2], fn lo =>
	  pure(P.andb, [hi1, hi2], fn hi =>
	    to64 (hi, lo, k))))))

    fun w64Notb ([n], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64 (n, fn (hi, lo) =>
	  pure(P.notb, [hi], fn hi' =>
	  pure(P.notb, [lo], fn lo' =>
	    to64 (hi', lo', k)))))

    fun w64Neg ([n], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    ifzero(lo,
	      pure(P.pure_arith{oper=P.~, kind=ui32}, [hi], fn hi' =>
		to64 (hi', lo, k)),
	      pure(P.pure_arith{oper=P.notb, kind=ui32}, [hi], fn hi' =>
	      pure(P.pure_arith{oper=P.~, kind=ui32}, [lo], lo' =>
		to64 (hi', lo', k))))))

(* assume amt < 64
    fun w64RShiftL ((hi, lo), amt) =
	  (W32.>> (hi, amt), W32.orb (W32.>> (lo, amt), W32.<< (hi, 0w32 - amt)))
*)
    fun w64RShiftL ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    pure(P.rshift, [hi, amt], fn hi' =>
	    pure(P.rshift, [lo, amt], fn t1 =>
	    pure(P.-, [num 32, amt], fn amt' =>
	    pure(P.lshift, [hi, amt'], fn t2 =>
	    pure(P.orb, [t1, t2], fn lo' =>
	      to64(hi', lo', k))))))))

(* assume amt < 64
    fun w64RShift ((hi, lo), amt) =
	  if (amt < 32)
	    then (W32.~>> (hi, amt), W32.orb (W32.>> (lo, amt), W32.<< (hi, 0w32 - amt))
	    else (W32.~>> (hi, 0w31), W32.~>> (hi, amt - 0w32))
*)
    fun w64RShift ([n1, n2], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  uIf(P.<, amt, num 32,
	    from64(n, fn (hi, lo) =>
	      pure(P.rshiftl, [hi, amt], fn hi' =>
	      pure(P.rshift, [lo, amt], fn t1 =>
	      pure(P.-, [num 32, amt], fn amt' =>
	      pure(P.lshift, [hi, amt'], fn t2 =>
	      pure(P.orb, [t1, t2], fn lo' =>
		to64(hi', lo', k)))))))
	    getHi32(n, fn hi =>
	      pure(P.rshiftl, [hi, num 31], fn hi' =>
	      pure(P.-, [amt, num 32], fn amt' =>
	      pure(P.rshiftl, [hi, amt'], fn lo' =>
		to64(hi', lo', k)))))))

(* assume amt < 64
    fun w64LShift ((hi, lo), amt) =
	  (W32.orb (W32.<< (hi, amt), W32.>> (lo, 0w32 - amt)), W32.<< (lo, amt))
*)
    fun w64LShift ([w, amt], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    pure(P.lshift, [hi, amt], fn t1 =>
	    pure(P.-, [num 32, amt], fn amt' =>
	    pure(P.rshift, [lo, amt'], fn t2 =>
	    pure(P.orb, [t1, t2], fn hi' =>
	    pure(P.lshift, [lo, amt], fn lo' =>
	      to64(hi', lo', k))))))))

(*
    fun w64Eql ((hi1, lo1), (hi2, lo2)) = (lo1 = lo2) andalso (hi1 = hi2)
*)
    fun w64Eql ([n1, n2], tr, fl) = let
	(* a continuation for the false uIf so that we can avoid code duplication *)
	  val flFnId = LV.mkLvar()
	  val fl' = C.APP(L.VAR flFnId, [])
	  in
	    C.FIX([(C.CONT, flFnId, [], [], fl)],
	    (* (lo1 = lo2) andalso (hi1 = hi2) *)
	      getLo32(n1, fn lo1 =>
	      getLo32(n2, fn lo2 =>
		uIf(P.eql, lo1, lo2,
		  getHi32(n1, fn hi1 =>
		  getHi32(n2, fn hi2 =>
		    uIf(P.eql, hi1, hi2, tr, fl'))),
		  fl'))))
	  end

    fun w64Less ([n1, n2], tr, fl) = let
	(* continuations for the branches so that we can avoid code duplication *)
	  val trFnId = LV.mkLvar()
	  val tr' = C.APP(L.VAR trFnId, [])
	  val flFnId = LV.mkLvar()
	  val fl' = C.APP(L.VAR flFnId, [])
	  in
	    C.FIX([(C.CONT, trFnId, [], [], tr), (C.CONT, flFnId, [], [], fl)],
	    (* (hi1 < hi2) orelse ((hi1 = hi2) andalso (lo1 < lo2)) *)
	      getHi32(n1, fn hi1 =>
	      getHi32(n2, fn hi2 =>
		uIf(P.<, hi1, hi2,
		  tr',
		  uIf(P.eql, hi1, hi2),
		    getLo32(n1, fn lo1 =>
		    getLo32(n2, fn lo2 =>
		      uIf(P.<, lo1, lo2, tr', fl'))),
		    fl')))
	  end

    fun w64Greater ([n1, n2], tr, fl) = let
	(* continuations for the branches so that we can avoid code duplication *)
	  val trFnId = LV.mkLvar()
	  val tr' = C.APP(L.VAR trFnId, [])
	  val flFnId = LV.mkLvar()
	  val fl' = C.APP(L.VAR flFnId, [])
	  in
	    C.FIX([(C.CONT, trFnId, [], [], tr), (C.CONT, flFnId, [], [], fl)],
	    (* (hi1 > hi2) orelse ((hi1 = hi2) andalso (lo1 > lo2)) *)
	      getHi32(n1, fn hi1 =>
	      getHi32(n2, fn hi2 =>
		uIf(P.>, hi1, hi2,
		  tr',
		  uIf(P.eql, hi1, hi2),
		    getLo32(n1, fn lo1 =>
		    getLo32(n2, fn lo2 =>
		      uIf(P.>, lo1, lo2, tr', fl'))),
		    fl')))
	  end

  (***** Int64 primitive operations *****)

(* Int64 primops from primopmap.sml
       ("i64mul",       P.arith{oper=P.*, kind=P.INT 64})
       ("i64div",       P.arith{oper=P.div, kind=P.INT 64})
       ("i64mod",       P.arith{oper=P.mod, kind=P.INT 64})
       ("i64quot",      P.arith{oper=P./, kind=P.INT 64})
       ("i64rem",       P.arith{oper=P.rem, kind=P.INT 64})
       ("i64add",       P.arith{oper=P.+, kind=P.INT 64})
       ("i64sub",       P.arith{oper=P.-, kind=P.INT 64})
       ("i64orb",       P.pure_arith{oper=P.orb, kind=P.INT 64})
       ("i64andb",      P.pure_arith{oper=P.andb, kind=P.INT 64})
       ("i64xorb",      P.pure_arith{oper=P.xorb, kind=P.INT 64})
       ("i64lshift",    P.pure_arith{oper=P.lshift, kind=P.INT 64})
       ("i64rshift",    P.pure_arith{oper=P.rshift, kind=P.INT 64})
       ("i64neg",       P.arith{oper=P.~, kind=P.INT 64})
       ("i64lt",	P.cmp{oper=P.<, kind=P.INT 64})
       ("i64le",	P.cmp{oper=P.<=, kind=P.INT 64})
       ("i64gt",	P.cmp{oper=P.>, kind=P.INT 64})
       ("i64ge",	P.cmp{oper=P.>=, kind=P.INT 64})
       ("i64eq",	P.cmp{oper=P.eql, kind=P.INT 64})
       ("i64ne",	P.cmp{oper=P.neq, kind=P.INT 64})
*)

    fun i64Neg ([n], res, cty, cexp) = join (res, cty, cexp, fn k =>
	  from64(n, fn (hi, lo) =>
	    ifzero(lo,
	      pure(P.arith{oper=P.~, kind=ui32}, [hi], fn hi' =>
		to64 (hi', lo, k)),
	      pure(P.pure_arith{oper=P.notb, kind=ui32}, [hi], fn hi' =>
	      pure(P.pure_arith{oper=P.~, kind=ui32}, [lo], lo' =>
		to64 (hi', lo', k))))))

    val i64Eql = w64Eql

(*
    fun i64Less ((hi1, lo1), (hi2, lo2)) =
	  (hi1 <_s hi2) orelse ((hi1 = hi2) andlalso (lo1 <_u lo2))
*)
    fun i64Less ([n1, n2], tr, fl) = let
	(* continuations for the branches so that we can avoid code duplication *)
	  val trFnId = LV.mkLvar()
	  val tr' = C.APP(L.VAR trFnId, [])
	  val flFnId = LV.mkLvar()
	  val fl' = C.APP(L.VAR flFnId, [])
	  in
	    C.FIX([(C.CONT, trFnId, [], [], tr), (C.CONT, flFnId, [], [], fl)],
	      getHi32(n1, fn hi1 =>
	      getHi32(n2, fn hi2 =>
		sIf(P.<, hi1, hi2,
		  tr',
		  sIf(P.eql, hi1, hi2),
		    getLo32(n1, fn lo1 =>
		    getLo32(n2, fn lo2 =>
		      uIf(P.<, lo1, lo2, tr', fl'))),
		    fl')))
	  end

(*
    fun i64Greater ((hi1, lo1), (hi2, lo2)) =
	  (hi1 >_s hi2) orelse ((hi1 = hi2) andlalso (lo1 >_u lo2))
*)
    fun i64Greater ([n1, n2], tr, fl]) =
	(* continuations for the branches so that we can avoid code duplication *)
	  val trFnId = LV.mkLvar()
	  val tr' = C.APP(L.VAR trFnId, [])
	  val flFnId = LV.mkLvar()
	  val fl' = C.APP(L.VAR flFnId, [])
	  in
	    C.FIX([(C.CONT, trFnId, [], [], tr), (C.CONT, flFnId, [], [], fl)],
	      getHi32(n1, fn hi1 =>
	      getHi32(n2, fn hi2 =>
		sIf(P.>, hi1, hi2,
		  tr',
		  sIf(P.eql, hi1, hi2),
		    getLo32(n1, fn lo1 =>
		    getLo32(n2, fn lo2 =>
		      uIf(P.>, lo1, lo2, tr', fl'))),
		    fl')))
	  end

  (***** main function *****)

    fun elim cfun = let
	  fun value (C.NUM{ival, ty={sz=64, ...}}, k) = let
		val (hi, lo) = split ival
		in
		  to64 (hi, lo, k)
		end
	    | value (v, k) = k v
	  and values (vl, k) = let
		fun f ([], vl') = k (List.rev vl')
		  | f (C.NUM{ival, ty={sz=64, ...}}::vs, vl') = let
		      val (hi, lo) = split ival
		      in
			to64 (hi, lo, fn v => f (vs, v::vl'))
		      end
		  | f (v::vs, vl') = f (vs, v::vl')
		in
		  f (vl, [])
		end
	  fun cexp (C.RECORD (rk, xl, v, e)) = let
		fun f ([], args') = C.RECORD (rk, List.rev args', v, cexp e)
		  | f ((C.NUM{ival, ty={sz=64, ...}}, offp)::args, args') = let
		      val (hi, lo) = split ival
		      in
			to64 (hi, lo, fn v => f (args, (v, offp)::args'))
		      end
		  | f (arg::args, args') = f (args, arg::args')
		in
		  f (xl, [])
		end
	    | cexp (C.SELECT(i, x, v, t, e)) = C.SELECT(i, x, v, t, cexp e)
	    | cexp (C.OFFSET(i, v, x, e)) = C.OFFSET(i, v, x, cexp e)
	    | cexp (C.APP(f, xl)) = values (xl, fn xl' => C.APP (f, xl'))
	    | cexp (C.FIX(fl, e)) = C.FIX(List.map function fl, cexp e)
	    | cexp (C.SWITCH(x, v, el)) = value (x, fn x' => C.SWITCH(x', v, List.map cexp el))
	    | cexp (C.BRANCH(b, xl, v, e1, e2)) = values (xl, fn xl' => (case b
		 of P.cmp{P.>, kind=P.UINT 64} => w64Greater (xl', cexp e1, cexp e2)
		  | P.cmp{P.>=, kind=P.UINT 64} => w64Less (xl', cexp e2, cexp e1)
		  | P.cmp{P.<, kind=P.UINT 64} => w64Less (xl', cexp e1, cexp e2)
		  | P.cmp{P.<=, kind=P.UINT 64} => w64Greater (xl', cexp e2, cexp e1)
		  | P.cmp{P.eql, kind=P.UINT 64} => w64Eql (xl', cexp e1, cexp e2)
		  | P.cmp{P.neq, kind=P.UINT 64} => w64Eql (xl', cexp e2, cexp e1)
		  | P.cmp{P.>, kind=P.INT 64} => i64Greater (xl', cexp e1, cexp e2)
		  | P.cmp{P.>=, kind=P.INT 64} => i64Less (xl', cexp e2, cexp e1)
		  | P.cmp{P.<, kind=P.INT 64} => i64Less (xl', cexp e1, cexp e2)
		  | P.cmp{P.<=, kind=P.INT 64} => i64Greater (xl', cexp e2, cexp e1)
		  | P.cmp{P.eql, kind=P.INT 64} => i64Eql (xl', cexp e1, cexp e2)
		  | P.cmp{P.neq, kind=P.INT 64} => i64Eql (xl', cexp e2, cexp e1)
		  | _ => C.BRANCH(b, xl', v, cexp e1, cexp e2)
		(* end case *)))
	    | cexp (C.SETTER(s, xl, e)) = values (xl, fn xl' => C.SETTER (s, xl', cexp e))
	    | cexp (C.LOOKER (rator, xl, v, t, e)) =
		values (xl, fn xl' => C.LOOKER (l, xl', v, t, cexp e))
	    | cexp (C.ARITH(rator, args, res, ty, e)) = (case rator
		 of P.arith{oper=P.*, kind=P.INT 64} => ??
		  | P.arith{oper=P.div, kind=P.INT 64} => i64Div(args, res, ty, e)
		  | P.arith{oper=P.mod, kind=P.INT 64} => i64Mod(args, res, ty, e)
		  | P.arith{oper=P./, kind=P.INT 64} => i64Div(args, res, ty, e)
		  | P.arith{oper=P.rem, kind=P.INT 64} => i64Rem(args, res, ty, e)
		  | P.arith{oper=P.+, kind=P.INT 64} => i64Add(args, res, ty, e)
		  | P.arith{oper=P.-, kind=P.INT 64} => i64Sub(args, res, ty, e)
		  | P.arith{oper=P.~, kind=P.INT 64} => i64Neg(args, res, ty, e)
		  | _ => C.ARITH(rator, args, res, ty, cexp e)
		(* end case *))
	    | cexp (C.PURE(rator, args, res, ty, e)) = (case rator
		 of P.pure_arith{oper=P.*, kind=P.UINT 64} => w64Mul(args, res, ty, e)
		  | P.pure_arith{oper=P./, kind=P.UINT 64} => w64Div(args, res, ty, e)
		  | P.pure_arith{oper=P.rem, kind=P.UINT 64} => w64Mod(args, res, ty, e)
		  | P.pure_arith{oper=P.+, kind=P.UINT 64} => w64Add(args, res, ty, e)
		  | P.pure_arith{oper=P.-, kind=P.UINT 64} => w64Sub(args, res, ty, e)
		  | P.pure_arith{oper=P.orb, kind=P.UINT 64} => w64Orb(args, res, ty, e)
		  | P.pure_arith{oper=P.xorb, kind=P.UINT 64} => w64Xorb(args, res, ty, e)
		  | P.pure_arith{oper=P.andb, kind=P.UINT 64} => w64Andb(args, res, ty, e)
		  | P.pure_arith{oper=P.notb, kind=P.UINT 64} => w64Notb(args, res, ty, e)
		  | P.pure_arith{oper=P.~, kind=P.UINT 64} => w64Neg(args, res, ty, e)
		  | P.pure_arith{oper=P.rshift, kind=P.UINT 64} => w64RShift(args, res, ty, e)
		  | P.pure_arith{oper=P.rshiftl, kind=P.UINT 64} => w64RShiftL(args, res, ty, e)
		  | P.pure_arith{oper=P.lshift, kind=P.UINT 64} => w64LShift(args, res, ty, e)
		  | P.pure_arith{oper=P.orb, kind=P.INT 64} => w64Orb(args, res, ty, e)
		  | P.pure_arith{oper=P.xorb, kind=P.INT 64} => w64Xorb(args, res, ty, e)
		  | P.pure_arith{oper=P.andb, kind=P.INT 64} => w64Andb(args, res, ty, e)
		  | P.pure_arith{oper=P.lshift, kind=P.INT 64} => w64LShift(args, res, ty, e)
		  | P.pure_arith{oper=P.rshift, kind=P.INT 64} => w64RShift(args, res, ty, e)
		  | _ => C.ARITH(rator, args, res, ty, cexp e)
		(* end case *))
	    | cexp (C.RCC(rk, cc, proto, args, res, e)) = ??
	  in
	    if Target.is64
	      then cfun
	      else function cfun
	  end (* elim *)

  end

