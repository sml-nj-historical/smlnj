(* ltytostring.sml *)

(***************************************************************************
 *            UTILITY FUNCTIONS FOR PRETTY PRINTING                        *
 ***************************************************************************)
(* These should be superceded by corresponding functions in PPLty. *)
(** (pretty?) printing of tkinds, tycs, and ltys -- see pplty.sml for real
 ** pretty printing **)

structure LtyToString : LTYTOSTRING =
struct

structure PT = PrimTyc
structure DI = DebIndex
structure LT = Lty
structure LN = LtyNorm
structure LU = LtyUtil

fun bug msg = ErrorMsg.impossible("LtyToString: "^msg)

val itos = Int.toString

fun plist(p, []) = ""
  | plist(p, x::xs) = 
    (p x) ^ (String.concat (map (fn z => ("," ^ (p z))) xs))

fun pfflag (LT.FF_VAR b) = 
    let fun pff (true, true) = "rr"  | pff (true, false) = "rc"
          | pff (false, true) = "cr" | pff (false, false) = "cc"
    in pff b
    end
  | pfflag (LT.FF_FIXED) = "f"

fun parw(p, (ff, t1, t2)) = 
    "<" ^ (p t1) ^ "> -" ^ pfflag ff ^ "-> <" ^ (p t2) ^ ">"

fun tk_print (x : LT.tkind) = 
  (case LT.tk_out x
    of LT.TK_MONO => "K0"
     | LT.TK_BOX => "KB0"
     | LT.TK_FUN(ks, k) =>
         "<" ^ (plist(tk_print, ks)) ^ "->" ^ (tk_print k) ^ ">"
     | LT.TK_SEQ zs => "KS(" ^ (plist(tk_print, zs)) ^ ")")

fun tc_print (x : LT.tyc) =
  (case (LN.tc_out_nm x)
    of LT.TC_VAR(i,j) => "TV(" ^ (DI.di_print i) ^ "," ^ (itos j) ^ ")"
     | LT.TC_NVAR v => "NTV(v" ^ (itos v) ^ ")"
     | LT.TC_PRIM pt => PT.pt_print pt
     | LT.TC_FN(ks, t) =>
         "(\\[" ^ plist(tk_print, ks) ^ "]." ^ (tc_print t) ^ ")"
     | LT.TC_APP(t, []) => tc_print t ^ "[]"
     | LT.TC_APP(t, zs) =>
         (tc_print t) ^ "[" ^ (plist(tc_print, zs)) ^ "]"
     | LT.TC_SEQ zs => "TS(" ^ (plist(tc_print,zs)) ^ ")"
     | LT.TC_PROJ (t, i) =>
         "TP(" ^ (tc_print t) ^ "," ^ (itos i) ^ ")"
     | LT.TC_SUM tcs =>
         "TSUM(" ^ (plist(tc_print, tcs)) ^ ")"
     | LT.TC_FIX {family={gen=tc,params=ts,...}, index=i} =>
         if LN.tc_eqv(x,LU.tcc_bool) then "B" 
         else if LN.tc_eqv(x,LU.tcc_list) then "LST" 
         else (let (* val ntc = case ts of [] => tc
                                                 | _ => tcc_app(tc, ts) *)
                   val _ = 1
               in ("DT{" ^ "DATA"  ^ (* "[" ^ (tc_print tc)  
                   ^ "] &&" ^ (plist(tc_print, ts))
                   ^ "&&" ^*)  "===" ^ (itos i) ^ "}")
               end)
     | LT.TC_ABS t => "Ax(" ^ (tc_print t) ^ ")"
     | LT.TC_BOX t => "Bx(" ^ (tc_print t) ^ ")"
     | LT.TC_TUPLE(_,zs) => "TT<" ^ (plist(tc_print, zs)) ^ ">"
     | LT.TC_ARROW (ff,z1,z2) =>
         parw(fn u => plist(tc_print,u),(ff,z1,z2))
     | LT.TC_PARROW _ => bug "unexpected TC_PARROW in tc_print"
     | LT.TC_TOKEN (k, t) =>
         if LT.token_isvalid k then 
             (LT.token_abbrev k) ^ "(" ^ (tc_print t) ^ ")"
         else bug "unexpected TC_TOKEN tyc in tc_print"
     | LT.TC_CONT ts => "Cnt(" ^ (plist(tc_print,ts)) ^ ")"
     | LT.TC_IND _ => bug "unexpected TC_IND in tc_print"
     | LT.TC_ENV _ => bug "unexpected TC_ENV in tc_print")

fun lt_print (x : Lty.lty) =
  (case LN.lt_out_nm x
    of LT.LT_TYC t => tc_print t
     | LT.LT_STR zs => "S{" ^ (plist(lt_print, zs)) ^ "}"
     | LT.LT_FCT (ts1,ts2) => 
         "(" ^ (plist(lt_print, ts1)) ^ ") ==> ("
         ^ (plist(lt_print, ts2)) ^ ")"
     | LT.LT_POLY(ks, ts) =>
         "(Q[" ^ plist(tk_print, ks) ^ "]." ^ (plist(lt_print,ts)) ^ ")"
     | LT.LT_CONT ts => "CNT(" ^ (plist(lt_print, ts)) ^ ")"
     | LT.LT_IND _ => bug "unexpected LT_IND in lt_print"
     | LT.LT_ENV _ => bug "unexpected LT_ENV in lt_print")

end (* structure LtyToString *)
