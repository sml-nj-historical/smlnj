(* total.sml *)

structure Clos2TAL = struct

local
    open NFlint
    open NFlintChecker
    open ppNFlint
    open TAL

    exception TALTRANS
    fun cbug p s = (print ("\n" ^ p ^ " translation error : " ^ s ^ "!\n"); raise TALTRANS)
    fun cbug1 s = cbug "CLOS2TAL" s
in

fun getDb env tv =
    let fun checkDb [] _ = cbug "CLOS2TAL" "cannot find the type var"
          | checkDb (x::xs) n = if x = tv then n else checkDb xs (n + 1) 
    in  checkDb env 0
    end

fun transTy env t =
    case t of
      NT_VAR tv            => (*TF_VAR tv*) TF_VAR (getDb env tv)
    | NT_INT               => TF_INT
    | NT_SINT i            => (* TF_NAT i *) TF_INT
    | NT_ARRAY t'          => TF_ARRAY (transTy env t')
    | NT_REF t'            => TF_REF (transTy env t')
    | NT_FIX (tv, i, ts)   => TF_FIX (TF_SUM (i, map (transTy (tv :: env)) ts)) (* ?? *)
    | NT_TUPLE ts          => TF_TUPLE (map (transTy env) ts)
    | NT_EXIST (tv, t')    => TF_EXISTS (transTy (tv :: env) t') (* ?? *)
    | NT_CODE (tvs, ts, _) => TF_CODE (length tvs, map (fn t => (0, transTy (tvs @ env(* env @ tvs ? *)) t)) ts)
                              (* their var here is redundant after they changed EF_CALL ? *)

(*
fun deExist (TF_EXISTS t) = t
  | deExist _ = cbug "CLOS2TAL" "TF_EXISTS expected for PACK"
*)

fun transCop c =
    case c of
      NFlint.IEQ  => CI.IEQ
    | NFlint.INEQ => CI.INEQ
    | NFlint.ILEQ => CI.ILEQ
    | NFlint.IGEQ => CI.IGEQ
    | NFlint.ILT  => CI.ILT
    | NFlint.IGT  => CI.IGT

fun transAop a =
    case a of
      NFlint.MUL => CI.MUL
    | NFlint.DIV => CI.DIV
    | NFlint.MOD => CI.MOD
    | NFlint.SUB => CI.SUB
    | NFlint.ADD => CI.ADD
    | _          => cbug "CLOS2TAL" "unexpected non-arith op"

fun transVal v =
    case v of
      VAR lv   => V_VAR lv
    | LABEL lv => V_LBL lv
    | INT i    => V_INT i
    | SINT i   => V_INT i

fun transVp (v, ap) = (* ?? *)
    let fun transAp ap =
        case ap of
          OFFp i => VP_V (transVal v)
        | SELp (i, ap') => VP_SEL (transAp ap', i)
    in  transAp ap
    end

fun transExp env exp =
    case exp of
      RET _                  => cbug "CLOS2TAL" "unexpected RET in post-CLOS"
    | LET _                  => cbug "CLOS2TAL" "unexpected LET in post-CLOS"
    | FIX _                  => cbug "CLOS2TAL" "unexpected FIX in post-CLOS"
    | APP (v, ts, vs)        => EF_CALL (transVal v, map (transTy env) ts, map transVal vs) (* ok *)
    | PTAPP (v, ts, lv, e)   => EF_MOV (lv, transVal v, transExp env e) (* ??!! *)
    | PACK (t, v, t', lv, e) => EF_PACK (lv, transTy env t, transVal v, (*deExist*) (transTy env t'), transExp env e)
                                (* deExist is a temprory hacking, to fill the gap between diff defs of t' *)
    | UNPACK (tv, lv, v, e)  => EF_OPEN (lv, transVal v, transExp (tv :: env) e) (* ok ? *)
    | SWITCH (v, es, lves)   => EF_SWITCH (transVal v, map (transExp env) es,
                                           map (fn (lv, e) => (lv, transExp env e)) lves) (* ?? *)
    | CON (v, t, lv, e)      => EF_INJN (lv, transTy env t, 0, transExp env e)
                                                     (* ?? need to get i from v! *) (* EF_FOLD!! *)
    | RECORD (vps, lv, e)    => EF_RECORD (lv, map transVp vps, transExp env e) (* ok *)
    | SELECT (v, i, lv, e)   => EF_SELECT (lv, transVal v, i, transExp env e) (* ok *)
    | BRANCH (c, vs, e, e')  => (* ok *)
        EF_COND (transCop c, transVal (hd vs), transVal (hd (tl vs)), transExp env e, transExp env e')
    | PRIMOP (a, vs, lv, e) => (* ok *)
      (case a of
         (MUL | DIV | MOD | SUB | ADD) =>
           EF_PRIMOP (lv, transAop a, transVal (hd vs), transVal (hd (tl vs)), transExp env e)
       | MKARRAY   => EF_MKARRAY (lv, transVal (hd vs), transVal (hd (tl vs)), transExp env e)
       | SUBSCRIPT => EF_SUBSCRIPT (lv, transVal (hd vs), transVal (hd (tl vs)), transExp env e)
       | UPDATE    => EF_UPDATE (transVal (hd vs), transVal (hd (tl vs)), transVal (hd (tl (tl vs))), transExp env e)
       | REF       => EF_REF (lv, transVal (hd vs), transExp env e)
       | DEREF     => EF_DEREF (lv, transVal (hd vs), transExp env e)
       | ASSIGN    => EF_ASSIGN (transVal (hd vs), transVal (hd (tl vs)), transExp env e))

and transFun (fk, lv, tvs, lvts, e) =
    let val lvts' = map (fn (lv, t) => (lv, transTy tvs (* [] ?? *) t)) lvts
    in  (lv, length tvs, lvts', transExp tvs e)
    end

fun transProg p =
    let val codes = map transFun p
    in  Fprog {
          tdecls = [],
          code   = codes,
          start  = #1 (hd codes)
        }     
    end

end (* toplevel local *)
end (* structure Clos2TAL *)
