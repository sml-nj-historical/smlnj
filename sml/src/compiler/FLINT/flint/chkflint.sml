(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* chkflint.sml *)

(* FLINT Type Checker *)

signature CHKFLINT = sig 

(** which set of the typing rules to use while doing the typecheck *)
type typsys (* currently very crude, i.e., = int or phases *)

val checkTop : FLINT.fundec * typsys -> bool
val checkExp : FLINT.lexp * typsys -> bool
val fname_ref : string ref  (* a filename hack *)

end (* signature CHKFLINT *)

structure ChkFlint : CHKFLINT = 
struct

local structure LT = LtyExtern
      structure LV = LambdaVar
      structure DA = Access 
      structure DI = DebIndex
      structure PP = PPFlint
      open FLINT
in

(** which set of the typing rules to use while doing the typecheck *)
type typsys = int (* currently very crude, use int for phases *)

(*** a hack of printing diagnostic output into a separate file ***) 
val fname_ref : string ref = ref "yyy"

fun bug s = ErrorMsg.impossible ("ChkFlint: "^s)
val say = Control.Print.say
val anyerror = ref false
val clickerror = fn () => (anyerror := true)

(****************************************************************************
 *                         BASIC UTILITY FUNCTIONS                          *
 ****************************************************************************)
fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"

fun simplify(le,0) = RET [STRING "<-dummy->"]
  | simplify(le,n) = 
      let fun h le = simplify(le, n-1)
          fun h1 (fk,v,args,le) = (fk,v,args,h le)
          fun h2 (v,tvs,le) = (v,tvs,h le)
       in case le 
           of LET(vs, e1, e2) => LET(vs, h e1, h e2)
            | FIX(fdecs, b) => FIX(map h1 fdecs, h b)
            | TFN(tdec, e) => TFN(h2 tdec, h e)
            | SWITCH(v, l, dc, opp) => 
               (let fun g(c, x) = (c, h x)
                    fun f x = case x of SOME y => SOME(h y) | NONE => NONE
                 in SWITCH(v, l, map g dc, f opp)
                end)
            | HANDLE(e, v) => HANDLE(h e, v)
            | _ => le
      end (* end of simplify *)

(** utility functions for printing *)
val tkPrint = say o LT.tk_print
val tcPrint = say o LT.tc_print
val ltPrint = say o LT.lt_print
fun lePrint le = PP.printLexp (simplify(le, 3))
fun svPrint sv = PP.printSval (sv)

(*** a hack for type checking ***)
fun laterPhase i = (i > 20)

(****************************************************************************
 *  MAIN FUNCTION --- val checkTop : Lambda.fundec * typsys -> bool         *
 ****************************************************************************)
fun checkTop ((fk,v,args,lexp), phase) = bug "not implemented"
(*
let 
val ltEquiv = LT.lt_eqv_bx
val ltAppChk = LT.lt_inst_chk
val ltString = if laterPhase(phase) then LT.ltc_void else LT.ltc_string
val ltExn = if laterPhase(phase) then LT.ltc_void else LT.ltc_exn
fun ltEtag lt = if laterPhase(phase) then LT.ltc_void 
                else LT.ltc_etag lt
fun ltVector t = if laterPhase(phase) then LT.ltc_void
                 else LT.ltc_tyc(LT.tcc_app(LT.tcc_vector,[t]))
exception tcUnbound = LT.tcUnbound
exception LtyArrow 
exception LtySelect
fun ltArrow lt = 
  (LT.lt_arrow lt) handle _ => raise LtyArrow
fun ltSel (lt, i) = 
  (LT.lt_select(lt, i)) handle _ => raise LtySelect
fun ltFun (t1, t2) = LT.ltc_fun(t1, t2)

(** utility values and functions on ltyEnv *)
type ltyEnv = LT.ltyEnv
val initLtyEnv : ltyEnv = LT.initLtyEnv
val ltLookup = LT.ltLookup
val ltInsert = LT.ltInsert

(** utility functions for type checking *)
fun ltTyApp le s (lt, ts, kenv) = 
      ((ltAppChk(lt, ts, kenv))
       handle zz => 
       (clickerror ();
        say (s ^ "  **** Kind conflicting in lexp =====> \n    ");
        case zz of LT.LtyAppChk => say "      exception LtyAppChk raised! \n"
                 | LT.TkTycChk =>  say "      exception TkTycChk raised! \n"
                 | _ => say "   other weird exception raised! \n";
        say "\n \n"; lePrint le; say "\n For Types: \n";  
        ltPrint lt; say "\n and   \n    "; 
        app (fn x => (tcPrint x; say "\n")) ts;   say "\n \n";  
        say "***************************************************** \n"; 
        bug "fatal typing error in ltTyApp"))

fun ltMatch le s (t1, t2) = 
  (if ltEquiv(t1,t2) then ()
   else (clickerror();
         say (s ^ "  **** Lty conflicting in lexp =====> \n    ");
         ltPrint t1; say "\n and   \n    "; ltPrint t2;
         say "\n \n";  MCprint.printLexp le;
         say "***************************************************** \n"))
  handle zz => 
  (clickerror();
   say (s ^ "  **** Lty conflicting in lexp =====> \n    ");
   say "uncaught exception found ";
   say "\n \n";  MCprint.printLexp le; say "\n";  
   ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n";  
   say "***************************************************** \n")

fun ltsMatch le s (ts1, ts2) = app2 (ltMatch le s) (ts1, ts2)

fun ltFnApp le s (t1, t2) = 
  let val (a1, b1) = 
        ((ltArrow t1) handle zz =>
            (clickerror ();
             say (s ^ "  **** Applying Non-Arrow Type in lexp =====> \n    ");
             case zz of LtyArrow => say "exception LtyArrow raised. \n"
                      | tcUnbound => say "exception tcUnbound raised. \n" 
                      | _ => say "other weird exceptions raised\n";
             say "\n \n";  lePrint le; say "\n For Types \n";
             ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n \n";  
             say "***************************************************** \n"; 
             bug "fatal typing error in ltFnApp"))

   in ltMatch le s (a1, t2); b1
  end

fun ltFnAppR le s (t1, t2) =  (*** used for DECON lexps ***)
  let val (a1, b1) = 
        ((ltArrow t1) handle zz => 
            (clickerror ();
             say (s ^ "  **** Rev-Apply Non-Arrow Type in lexp =====> \n    ");
             case zz of LtyArrow => say "exception LtyArrow raised. \n"
                      | tcUnbound => say "exception tcUnbound raised. \n"
                      | _ => say "other weird exceptions raised\n";
             say "\n \n";  lePrint le; say "\n For Types \n";
             ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n \n"; 
             say "***************************************************** \n"; 
             bug "fatal typing error in ltFnApp"))

   in ltMatch le s (b1, t2); a1
  end

fun ltSelect le s (lt, i) = 
  ((ltSel(lt, i))
     handle zz => 
       (clickerror ();
        say (s ^ "  **** Select from a wrong-type lexp  =====> \n    ");
        case zz of LtySelect => say "exception LtyArrow raised. \n"
                 | tcUnbound => say "exception tcUnbound raised. \n"
                 | _ => say "other weird exceptions raised\n";
        say "\n \n";  lePrint le; say "\n \n";
        say "Selecting "; say (Int.toString i); 
        say "-th component from the type: \n     "; ltPrint lt; say "\n \n "; 
        say "***************************************************** \n"; 
        bug "fatal typing error in ltSelect"))

(*
 * Right now, ltConChk does not check the case for DATAcon but this
 * will be fixed soon. (ZHONG)
 *)
fun ltConChk le s (DATAcon ((_, rep, lt), ts, vs), root, venv, kenv, d) = 
      let val nt = ltTyApp le "DECON" (lt, ts, kenv)
          val nts = ltFnAppR le "DECON" (nt, [root])
          fun h(env, v::r, x::s) = 
                h(ltInsert(env, v, x, d), r, s)
            | h(env, [], []) = env
            | h _ = (say ("** SWI BINDINGS mismatch ** vs=" ^
                     (Int.toString (length vs)) ^ "  e1s=" ^
                     (Int.toString (length nts)) ^ " \n");
                     bug "unexpected lambda code in ltConChk")
       in h(venv, vs, nts)
      end
  | ltConChk le s (c, lt, venv, kenv, d) = 
      let val nt = (case c of INT32con _ => LT.ltc_int32
                            | WORD32con _ => LT.ltc_int32
                            | REALcon _ => LT.ltc_real
                            | STRINGcon _ => ltString
                            |  _ => LT.ltc_int)
       in ltMatch le s (nt, lt); venv
      end

(** check : tkindEnv * ltyEnv * DI.depth -> lexp -> lty list *)
fun check (kenv, venv, d) = 
  let fun lpsv (INT _) = LT.ltc_int
        | lpsv (WORD _) = LT.ltc_int
        | lpsv (INT32 _) = LT.ltc_int32
        | lpsv (WORD32 _) = LT.ltc_int32
        | lpsv (REAL _) = LT.ltc_real
        | lpsv (STRING _) = ltString
        | lpsv (VAR v) = 
            (ltLookup(venv, v, d) 
             handle LT.ltUnbound => 
              (say ("** Lvar ** " ^ (LV.lvarName(v)) ^ " is unbound *** \n");
               bug "unexpected lambda code in checkTop"))

      fun loop le =
       (case le
         of RET vs => map lpsv sv
          | APP(v, vs) => ltFnApp le "APP" (lpsv v, map lpsv vs)
          | TAPP(sv, ts) => ltTyApp le "TAPP" (lpsv sv, ts, kenv)
          | LET(vs, e1, e2) => 
              let val ts = loop e1
                  fun h(env, v::r, t::s) = 
                        h(ltInsert(env,v,t,d), r, s)
                    | h(env, [], []) = env
                    | h _ = (say ("** LET BINDINGS mismatch ** vs=" ^
                                  (Int.toString (length vs)) ^ "  e1s=" ^
                                  (Int.toString (length ts)) ^ " \n");
                             bug "unexpected lambda code in checkTop")
                   val venv1 = h(venv, vs, ts)
               in check (kenv, venv1, d) e2
              end
          | FIX (fs, eb) =>
              let fun h (env, (_,v,t,_,_)::r) = h(ltInsert(env,v,t,d), r)
                    | h (env, []) = env
                  val venv1 = h(venv, fs)

                  fun g (_,_,_,args,e) =
                    let fun m (env, (v,t)::r) = m(ltInsert(env,v,t,d), r)
                          | m (env, []) = env
                        val venv2 = m(venv1, args)
                        val argts = map #1 args
                        val rests = check (kenv, venv2, d) e
                     in LT.ltc_fun(argts, rests)
                    end
                  val nts = map g fs
                  val _ = ltsMatch le "FIX1" (map #3 fs, nts)
               in check (kenv, venv1, d) eb
              end
          | TFN((v,t,tvs,e), eb) => 
              let val kenv1 = LT.tkInsert(kenv, map #2 tvs)
                  (*** temporarily using the old format of type variables ***)
                  val lts = check (kenv1, venv, DI.next d) e
                  val nt = LT.ltc_poly(ks, lts)
                  val _ = ltMatch le "TFN" (t, nt)
                  val venv1 = ltInsert(venv,v,t,d)
               in check (kenv, venv1, d) eb
              end
          | SWITCH(v, _, cl, opp) => 
              let val root = lpsv v
                  fun h (c, x) = 
                    let val venv1 = ltConChk le "SWT1" (c, root, venv, kenv, d)
                     in check (kenv, venv1, d) x
                    end
                  val ts = map h cl
               in (case ts
                    of [] => bug "empty switch in checkTop"
                     | a::r => 
                        (app (fn x => ltsMatch le "SWT2" (x, a)) r;
                         case opp
                          of NONE => a
                           | SOME be => (ltsMatch le "SWT3" (loop be, a); a)))
              end
          | CON((_, rep, lt), ts, vs, v, eb) =>   
              let val ts1 = ltTyApp le "CON" (lt, ts, kenv)
                  val ts2 = map lpsv vs
               in case ts1
                   of [t1] => 
                        let val nt = ltFnApp le "CON-A" (t1, ts2)
                            val venv1 = ltInsert(venv, v, nt, d)
                         in check (kenv, venv1, d) eb
                        end
                    | _ => bug "unexpected case in check CON"
              end
          | RECORD (RK_VECTOR t, vl) => 
              let val ts = map lpsv vl
               in app (fn x => ltMatch le "VECTOR" (x, LT.ltc_tyc t)) ts; 
                  ltVector t
              end
          | RECORD (_, []) => LT.ltc_unit
          | RECORD (RK_RECORD, vl) => LT.ltc_tuple (map lpsv vl)
          | RECORD (RK_STRUCT, vl) => LT.ltc_str (map lpsv vl)

          | SELECT(u, i, v, eb) => 
              let val t = ltSelect le "SEL" (lpsv u, i)
                  val venv1 = ltInsert(venv, v, t, d)
               in check (kenv, venv1, d) eb
              end

          | ETAG(v, t) =>   (* v is string but the result should be eflag[t] *)
              let val z = lpsv v   (* what do we check on e ? *)
                  val _ = ltMatch le "ETAG1" (z, LT.ltc_string) 
               in ltEtag t
              end

          | RAISE(v,t) => 
              (ltMatch le "RAISE" (lpsv v, ltExn); t)

          | HANDLE(e,sv) => 
             let val ts = loop e
                 val arg = ltFnAppR le "HANDLE" (lpsv sv, ts)
              in ts
             end

          | PRIM((p, t, ts), vs, v, eb) => 
             let val xx = ltTyApp (SVAL sv) "PRIM" (t, ts, kenv) 
              in check (kenv, ltInsert(venv, v, t, d), d) eb
             end

          | GENOP(dict, (p, t, ts), vs, v, eb) => 
             (* should check the consistency of dict here *)
             ltTyApp (SVAL sv) "GENOP" (t, ts, kenv)

          | PACK(lt, ts, nts, sv) => 
              let val argTy = ltTyApp le "PACK-A" (lt, ts, kenv)
               in ltMatch le "PACK-M" (argTy, lpsv sv);
                  ltTyApp le "PACK-R" (lt, nts, kenv)
              end

          (** these two cases should never happen before wrapping *)
          | WRAP(t, b, sv, v, eb) => 
              (ltMatch le "WRAP" (lpsv sv, LT.ltc_tyc t); 
               if laterPhase(phase) then LT.ltc_void
               else LT.ltc_tyc(if b then LT.tcc_box t else LT.tcc_abs t))

          | UNWRAP(t, b, sv, v, eb) => 
              let val ntc = if laterPhase(phase) then LT.tcc_void
                            else (if b then LT.tcc_box t else LT.tcc_abs t)
                  val nt = LT.ltc_tyc ntc
               in (ltMatch le "UNWRAP" (lpsv sv, nt); LT.ltc_tyc t)
              end

          | FN(v, t, e1) => 
              let val venv' = ltInsert(venv, v, t, d)
                  val res = check (kenv, venv', d) e1
               in ltFun(t, res) (* handle both functions and functors *)
              end)


  in loop 
 end (* end-of-fn-check *)

in 
anyerror := false; check (LT.initTkEnv, initLtyEnv, DI.top) lexp; !anyerror;
end (* end of function checkTop *)
*)

fun checkExp  _ = bug "checkExp not implemented yet"

end (* toplevel local *)
end (* structure ChkFlint *)

