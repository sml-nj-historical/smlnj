(* Copyright 1996 by AT&T Bell Laboratories *)
(* chkplexp.sml *)

signature CHKPLEXP = 
sig 

val checkLty : PLambda.lexp * int -> bool
val newlam_ref : PLambda.lexp ref
val fname_ref : string ref

end (* signature CHKPLEXP *)

structure ChkPlexp : CHKPLEXP = 
struct

local structure LT = PLambdaType
      structure LV = LambdaVar
      structure DA = Access 
      structure DI = DebIndex
      open PLambda 
in

(*** a hack of printing diagnostic output into a separate file ***) 
val newlam_ref : PLambda.lexp ref = ref (RECORD[])
val fname_ref : string ref = ref "yyy"

fun bug s = ErrorMsg.impossible ("CheckLty: "^s)
val say = Control.Print.say

val anyerror = ref false
val clickerror = fn () => (anyerror := true)

(****************************************************************************
 *                         BASIC UTILITY FUNCTIONS                          *
 ****************************************************************************)
fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"

fun simplify(le,0) = STRING "<dummy>"
  | simplify(le,n) = 
      let fun h le = simplify(le, n-1)
       in case le 
           of FN(v, t, e) => FN(v, t, h e)
            | APP(e1, e2) => APP(h e1, h e2)
            | LET(v, e1, e2) => LET(v, h e1, h e2)
            | TFN(ks, e) => TFN(ks, h e)
            | TAPP(e, ts) => TAPP(h e, ts)
            | PACK(lt, ts, nts, e) => PACK(lt, ts, nts, h e)
            | CON(l, x, e) => CON(l, x, h e)
(*          | DECON(l, x, e) => DECON(l, x, h e) *)
            | FIX(lv, lt, le, b) => FIX(lv, lt, map h le, h b)
            | SWITCH(e, l, dc, opp) => 
               (let fun g(c, x) = (c, h x)
                    fun f x = case x of SOME y => SOME(h y) | NONE => NONE
                 in SWITCH(h e, l, map g dc, f opp)
                end)
            | RECORD e => RECORD (map h e)
            | SRECORD e => SRECORD (map h e)
            | VECTOR(e, x) => VECTOR (map h e, x)
            | SELECT(i, e) => SELECT(i, h e)
            | HANDLE(e1, e2) => HANDLE(h e1, h e2)
            | WRAP(t, b, e) => WRAP(t, b, h e)
            | UNWRAP(t, b, e) => UNWRAP(t, b, h e)
            | _ => le
      end (* end of simplify *)

(** utility functions for printing *)
val tkPrint = say o LT.tk_print
val tcPrint = say o LT.tc_print
val ltPrint = say o LT.lt_print
fun lePrint le = PPLexp.printLexp (simplify(le, 3))

(*** a hack for type checking ***)
fun laterPhase i = (i > 20)

(****************************************************************************
 *           MAIN FUNCTION --- val checkLty : PLambda.lexp -> bool          *
 ****************************************************************************)
fun checkLty (lexp, phase) = 
let 

val ltEquiv = LT.lt_eqv
val ltString = if laterPhase(phase) then LT.ltc_void else LT.ltc_string
val ltExn = if laterPhase(phase) then LT.ltc_void else LT.ltc_exn
fun ltEtag lt = if laterPhase(phase) then LT.ltc_void 
                else LT.ltc_etag lt
fun ltVector t = if laterPhase(phase) then LT.ltc_void
                 else LT.ltc_tyc(LT.tcc_vector t)

(** lazily selecting a field from a record/structure type *)
exception LtySelect
fun ltSel (lt, i) = 
  (LT.lt_select(lt, i)) handle _ => raise LtySelect

(** build a function or functor type from a pair of arbitrary ltys *)
fun ltFun (x, y) = 
  if (LT.ltp_tyc x) andalso (LT.ltp_tyc y) then LT.ltc_parrow(x, y)
  else LT.ltc_pfct(x, y)

fun ltTup ts = LT.ltc_tyc(LT.tcc_tuple (map LT.ltd_tyc ts))

(** lazily finding out the arg and res of an lty *)
exception LtyArrow 
fun ltArrow lt = 
  (if LT.ltp_tyc lt then LT.ltd_parrow lt
   else LT.ltd_pfct lt) handle _ => raise LtyArrow

val lt_inst_chk = LT.lt_inst_chk_gen()

fun ltAppChk (lt, ts, kenv) = 
  (case lt_inst_chk(lt, ts, kenv) 
    of [b] => b 
     | _ => bug "unexpected ase in ltAppChk")

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
         say "\n \n";  PPLexp.printLexp le;
         say "***************************************************** \n"))
  handle zz => 
  (clickerror();
   say (s ^ "  **** Lty conflicting in lexp =====> \n    ");
   say "uncaught exception found ";
   say "\n \n";  PPLexp.printLexp le; say "\n";  
   ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n";  
   say "***************************************************** \n")

fun ltFnApp le s (t1, t2) = 
  let val (a1, b1) = 
        ((ltArrow t1) handle zz =>
            (clickerror ();
             say (s ^ "  **** Applying Non-Arrow Type in lexp =====> \n    ");
             case zz of LtyArrow => say "exception LtyArrow raised. \n"
                      | LT.tcUnbound => say "exception tcUnbound raised. \n"
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
                      | LT.tcUnbound => say "exception tcUnbound raised. \n"
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
                 | LT.tcUnbound => say "exception tcUnbound raised. \n"
                 | _ => say "other weird exceptions raised\n";
        say "\n \n";  lePrint le; say "\n \n";
        say "Selecting "; say (Int.toString i); 
        say "-th component from the type: \n     "; ltPrint lt; say "\n \n "; 
        say "***************************************************** \n"; 
        bug "fatal typing error in ltSelect"))

(** ltConChk currently does not check the case for DATAcon *)
(** Of course, we could easily check for monomorphic DATAcons *)
fun ltConChk le s (DATAcon ((_,rep,lt), ts, v), root, kenv, venv, d) = 
      let val t1 = ltTyApp le "DECON" (lt, ts, kenv)
          val t = ltFnAppR le "DECON" (t1, root)
       in LT.ltInsert(venv, v, t, d)
      end
  | ltConChk le s (c, root, kenv, venv, d) = 
      let val nt = (case c of INT32con _ => LT.ltc_int32
                            | WORD32con _ => LT.ltc_int32
                            | REALcon _ => LT.ltc_real
                            | STRINGcon _ => ltString
                            |  _ => LT.ltc_int)
       in ltMatch le s (nt, root); venv
      end


(** check : tkindEnv * ltyEnv * DI.depth -> lexp -> lty *)
fun check (kenv, venv, d) = 
  let fun loop le =
       (case le
         of VAR v => 
              (LT.ltLookup(venv, v, d) 
               handle LT.ltUnbound => 
                (say ("** Lvar ** " ^ (LV.lvarName(v)) ^ " is unbound *** \n");
                 bug "unexpected lambda code in checkLty"))
          | (INT _ | WORD _) => LT.ltc_int
          | (INT32 _ | WORD32 _) => LT.ltc_int32
          | REAL _ => LT.ltc_real
          | STRING _ => ltString
          | PRIM(p, t, ts) => ltTyApp le "PRIM" (t, ts, kenv) 

          | FN(v, t, e1) => 
              let val venv' = LT.ltInsert(venv, v, t, d)
                  val res = check (kenv, venv', d) e1
               in ltFun(t, res) (* handle both functions and functors *)
              end

          | FIX(vs, ts, es, eb) => 
              let fun h (env, v::r, x::z) = h(LT.ltInsert(env, v, x, d), r, z)
                    | h (env, [], []) = env
                    | h _ = bug "unexpected FIX bindings in checkLty."
                  val venv' = h(venv, vs, ts)

                  val nts = map (check (kenv, venv', d)) es
                  val _ = app2(ltMatch le "FIX1", ts, nts)

               in check (kenv, venv', d) eb
              end

          | APP(e1, e2) => ltFnApp le "APP" (loop e1, loop e2)

          | LET(v, e1, e2) => 
              let val venv' = LT.ltInsert(venv, v, loop e1, d)
               in check (kenv, venv', d) e2
              end

          | TFN(ks, e) => 
              let val kenv' = LT.tkInsert(kenv, ks)
                  val lt = check (kenv', venv, DI.next d) e
               in LT.ltc_poly(ks, [lt])
              end

          | TAPP(e, ts) => ltTyApp le "TAPP" (loop e, ts, kenv)
          | GENOP(dict, p, t, ts) => 
              ((* should type check dict also *)
               ltTyApp le "GENOP" (t, ts, kenv))

          | PACK(lt, ts, nts, e) => 
              let val argTy = ltTyApp le "PACK-A" (lt, ts, kenv)
               in ltMatch le "PACK-M" (argTy, loop e);
                  ltTyApp le "PACK-R" (lt, nts, kenv)
              end

          | CON((_, rep, lt), ts, e) =>   
              let val t1 = ltTyApp le "CON" (lt, ts, kenv)
                  val t2 = loop e
               in ltFnApp le "CON-A" (t1, t2)
              end
(*
          | DECON((_, rep, lt), ts, e) =>   
              let val t1 = ltTyApp le "DECON" (lt, ts, kenv)
                  val t2 = loop e
               in ltFnAppR le "DECON" (t1, t2)
              end
*)
          | RECORD el => ltTup (map loop el)
          | SRECORD el => LT.ltc_str (map loop el)

          | VECTOR (el, t)  => 
              let val ts = map loop el
               in app (fn x => ltMatch le "VECTOR" (x, LT.ltc_tyc t)) ts; 
                  ltVector t
              end

          | SELECT(i,e) => ltSelect le "SEL" (loop e, i)

          | SWITCH(e, _, cl, opp) => 
              let val root = loop e
                  fun h (c, x) = 
                    let val venv' = ltConChk le "SWT1" (c, root, kenv, venv, d)
                     in check (kenv, venv', d) x 
                    end
                  val ts = map h cl
               in (case ts
                    of [] => bug "empty switch in checkLty"
                     | a::r => 
                        (app (fn x => ltMatch le "SWT2" (x, a)) r;
                         case opp
                          of NONE => a
                           | SOME be => (ltMatch le "SWT3" (loop be, a); a)))
              end

          | ETAG(e, t) => 
              let val z = loop e   (* what do we check on e ? *)
                  val _ = ltMatch le "ETAG1" (z, LT.ltc_string) 
               in ltEtag t
              end

          | RAISE(e,t) => 
              (ltMatch le "RAISE" (loop e, ltExn); t)

          | HANDLE(e1,e2) => 
             let val t1 = loop e1
                 val arg = ltFnAppR le "HANDLE" (loop e2, t1)
              in t1
             end

          (** these two cases should never happen before wrapping *)
          | WRAP(t, b, e) => 
              (ltMatch le "WRAP" (loop e, LT.ltc_tyc t); 
               if laterPhase(phase) then LT.ltc_void
               else LT.ltc_tyc(if b then LT.tcc_box t else LT.tcc_abs t))

          | UNWRAP(t, b, e) => 
              let val ntc = if laterPhase(phase) then LT.tcc_void
                            else (if b then LT.tcc_box t else LT.tcc_abs t)
                  val nt = LT.ltc_tyc ntc
               in (ltMatch le "UNWRAP" (loop e, nt); LT.ltc_tyc t)
              end)
                            

  in loop 
 end (* end-of-fn-check *)

in 
anyerror := false;
check (LT.initTkEnv, LT.initLtyEnv, DI.top) lexp; !anyerror
end (* end of function checkLty *)

end (* toplevel local *)
end (* structure CheckLty *)

(*
 * $Log$
 *)
