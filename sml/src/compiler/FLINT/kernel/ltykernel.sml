(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltykernel.sml *)

structure LtyKernel :> LTYKERNEL = 
struct 

(***************************************************************************
 *                UTILITY FUNCTIONS FOR HASHCONSING BASICS                 *
 ***************************************************************************)

(** hashconsing implementation basics *)
local open SortedList
      val MVAL = 10000
      val BVAL = MVAL * 2 (* all index i start from 0 *)
in 

type enc_tvar = int
fun tvToInt (d, i) = d * MVAL + i
fun tvFromInt x = ((x div MVAL), (x mod MVAL))

fun exitLevel xs = 
  let fun h ([], x) = rev x
        | h (a::r, x) = if a < BVAL then h(r, x) else h(r, (a-MVAL)::x)
   in h(xs, [])
  end
  
datatype aux_info = AX_REG of bool * enc_tvar list
                  | AX_NO

val mergeTvs = merge
val fmergeTvs = foldmerge

type 'a hash_cell = (int * 'a * aux_info) ref

end (* local of hashconsing implementation basics *)


(***************************************************************************
 *                 DATATYPE DEFINITIONS                                    *
 ***************************************************************************)

(** definition of kinds for all the lambda tycs *)
datatype tkindI
  = TK_MONO                                    (* ground mono tycon *)
  | TK_BOX                                     (* boxed/tagged tycon *)
  | TK_SEQ of tkind list                       (* sequence of tycons *)
  | TK_FUN of tkind list * tkind               (* tycon function *)

withtype tkind = tkindI hash_cell              (* hash-consing-impl of tkind *)

(* definitoins of named tyc variables *)
type tvar = LambdaVar.lvar                     (* temporary definitions *)
val mkTvar = LambdaVar.mkLvar

(** definitions of lambda type constructors *)
datatype tycI
  = TC_VAR of DebIndex.index * int             (* tyc variables *)
  | TC_NVAR of tvar * DebIndex.depth * int     (* named tyc variables *)
  | TC_PRIM of PrimTyc.primtyc                 (* primitive tyc *)

  | TC_FN of tkind list * tyc                  (* tyc abstraction *)
  | TC_APP of tyc * tyc list                   (* tyc application *)
  | TC_SEQ of tyc list                         (* tyc sequence *)
  | TC_PROJ of tyc * int                       (* tyc projection *)

  | TC_SUM of tyc list                         (* sum tyc *)
  | TC_FIX of (int * tyc * tyc list) * int     (* recursive tyc *)

  | TC_TUPLE of rflag * tyc list               (* std record tyc *)
  | TC_ARROW of fflag * tyc list * tyc list    (* std function tyc *)
  | TC_PARROW of tyc * tyc                     (* special fun tyc, not used *)

  | TC_BOX of tyc                              (* boxed tyc *)
  | TC_ABS of tyc                              (* abstract tyc, not used *)
  | TC_CONT of tyc list                        (* intern continuation tycon *)
  | TC_IND of tyc * tycI                       (* indirect tyc thunk *)
  | TC_ENV of tyc * int * int * tycEnv         (* tyc closure *)

withtype tyc = tycI hash_cell                  (* hash-consed tyc cell *)
     and tycEnv = tyc     (* 
                           * This really is (tyc list option * int) list,
                           * it is encoded using SEQ[(PROJ(SEQ tcs),i)]
                           * and SEQ[(PROJ(VOID, i))]. (ZHONG)
                           *)

     and fflag = bool * bool (* is the calling convention fixed ? *)
     and rflag = unit     (* record kind, not used as of now *)

val default_rflag = ()    (* a rflag template *)
val default_fflag = (true,true)

(** definitions of lambda types *)
datatype ltyI          
  = LT_TYC of tyc                              (* monomorphic type *)
  | LT_STR of lty list                         (* structure record type *)
  | LT_FCT of lty list * lty list              (* functor arrow type *)
  | LT_POLY of tkind list * lty list           (* polymorphic type *)

  | LT_PST of (int * lty) list                 (* partial-structure type *)
  | LT_CONT of lty list                        (* internal cont type *)
  | LT_IND of lty * ltyI                       (* a lty thunk and its sig *)
  | LT_ENV of lty * int * int * tycEnv         (* lty closure *)

withtype lty = ltyI hash_cell                  (* hash-consed lty cell *)

(***************************************************************************
 *                   HASHCONSING IMPLEMENTATIONS                           *
 ***************************************************************************)

(** Hash-consing implementations of tyc, tkind, lty *)

local structure Weak = SMLofNJ.Weak
      structure PT = PrimTyc
      structure DI = DebIndex

      fun bug msg = ErrorMsg.impossible("LtyKernel: "^msg)
 
      val itow = Word.fromInt
      val wtoi = Word.toIntX
      val andb = Word.andb

      val N = 2048 (* 1024 *)
      val NN = itow (N*N)
      val P = 0w509 (* was 0w1019, a prime < 1024 so that N*N*P < maxint *)

      val tk_table : tkind Weak.weak list Array.array = Array.array(N,nil)
      val tc_table : tyc Weak.weak list Array.array = Array.array(N,nil)
      val lt_table : lty Weak.weak list Array.array = Array.array(N,nil)

      fun vector2list v = Vector.foldr (op ::) [] v

      fun revcat(a::rest,b) = revcat(rest,a::b)
        | revcat(nil,b) = b

      fun combine [x] = itow x
        | combine (a::rest) = 
            andb(itow a +(combine rest)*P, NN - 0w1)
        | combine _ = bug "unexpected case in combine"

      (* 
       * Because of the "cmp" function below, it's necessary to keep
       * each bucket-list in a consistent order, and not reverse
       * or move-to-front or whatever. 
       *)
      fun look(table, h, t, eq, mk) =
        let val i = wtoi(andb(itow h, itow(N-1)))

            fun g(l, z as (w::rest)) = 
                  (case Weak.strong w
                    of SOME (r as ref(h',t',_)) =>
                        if (h=h') andalso (eq {new=t, old=t'})
                        then (Array.update(table, i, revcat(l,z)); r)
                        else g(w::l, rest)
                     | NONE => g(l, rest))
              | g(l, []) = 
                  let val r = mk(h, t)
                   in Array.update(table, i, (Weak.weak r) :: rev l); r
                  end

         in g([], Array.sub(table, i))
        end

      fun cmp(table, a as ref(ai,_,_), b as ref (bi,_,_)) =
        if ai < bi then LESS 
        else if ai > bi then GREATER
           else if a = b then EQUAL
                else let val index = wtoi (andb(itow ai,itow(N-1)))
                         fun g [] = bug "unexpected case in cmp"
                           | g (w::rest) =
                                 (case Weak.strong w
                                   of SOME r => 
                                        if a=r then LESS 
                                        else if b=r then GREATER
                                                    else g rest
                                    | NONE => g rest)
                      in g(Array.sub(table,index))
                     end


      fun getnum (ref(i,_,_)) = i
      fun tagnums nil = nil
        | tagnums ((i,t)::rest) = i::getnum t::tagnums rest

      fun tk_hash tk =
        let fun g (TK_MONO) = 0w1
              | g (TK_BOX) = 0w2
              | g (TK_SEQ ks) = combine (3::map getnum ks)
              | g (TK_FUN(ks, k)) = combine (4::getnum k::(map getnum ks))
         in g tk
        end

      fun tc_hash tc = 
        let fun g (TC_VAR(d, i)) = combine [1, (DI.di_key d)*10, i]
              | g (TC_NVAR(v, d, i)) = combine[15, v, (DI.dp_key d)*13, i]
              | g (TC_PRIM pt) = combine [2, PT.pt_toint pt]
              | g (TC_FN(ks, t)) = combine (3::(getnum t)::(map getnum ks))
              | g (TC_APP(t, ts)) = combine (4::(getnum t)::(map getnum ts))
              | g (TC_SEQ ts) = combine (5::(map getnum ts))
              | g (TC_PROJ(t, i)) = combine [6, (getnum t), i]
              | g (TC_SUM ts) = combine (7::(map getnum ts))
              | g (TC_FIX((n, t, ts), i)) = 
                     combine (8::n::i::(getnum t)::(map getnum ts))
              | g (TC_ABS t) = combine [9, getnum t]
              | g (TC_BOX t) = combine [10, getnum t]
              | g (TC_TUPLE (_, ts)) = combine (11::(map getnum ts))
              | g (TC_ARROW(rw, ts1, ts2)) = 
                     let fun h(true, true) = 10
                           | h(true, _) = 20
                           | h(_, true) = 30
                           | h _ = 40
                      in combine (12::(h rw)::(map getnum (ts1@ts2)))
                     end
              | g (TC_PARROW (t1,t2)) = combine [13, getnum t1, getnum t2]
              | g (TC_CONT ts) = combine (14::(map getnum ts))
              | g (TC_ENV(t,i,j,env)) = 
                     combine[15, getnum t, i, j, getnum env]
              | g (TC_IND _) = bug "unexpected TC_IND in tc_hash"

         in g tc
        end 

      fun lt_hash lt = 
        let fun g (LT_TYC t) = combine [1, getnum t]
              | g (LT_STR ts) = combine (2::(map getnum ts))
              | g (LT_PST ts) = combine (3::(tagnums ts))
              | g (LT_FCT(ts1, ts2)) = 
                     combine (4::(map getnum (ts1@ts2)))
              | g (LT_POLY(ks, ts)) = 
                     combine (5::((map getnum ts)@(map getnum ks)))
              | g (LT_CONT ts) = combine (6::(map getnum ts))
              | g (LT_ENV(t,i,j,env)) = 
                     combine [7, getnum t, i, j, getnum env]
              | g (LT_IND _) = bug "unexpected LT_IND in tc_hash"
         in g lt
        end

      fun tkI_eq {new: tkindI, old} = (new = old)
      
      (* the 1st is one being mapped; the 2nd is in the hash table *)
      fun tcI_eq {new : tycI, old=TC_IND(_,s)} = tcI_eq {new=new, old=s}
        | tcI_eq {new, old} = (new=old)

      fun ltI_eq {new : ltyI, old=LT_IND(_,s)} = ltI_eq {new=new, old=s}
        | ltI_eq {new, old} = (new=old)

      val baseAux = AX_REG (true, [])

      fun getAux (ref(i : int, _, x)) = x

      fun mergeAux(AX_NO, _) = AX_NO
        | mergeAux(_, AX_NO) = AX_NO
        | mergeAux(AX_REG(b1,vs1), AX_REG(b2,vs2)) =
            AX_REG(b2 andalso b1, mergeTvs(vs1, vs2))

      fun fsmerge [] = baseAux
        | fsmerge [x] = getAux x
        | fsmerge xs = 
            let fun loop([], z) = z
                  | loop(_, AX_NO) = AX_NO
                  | loop(a::r, z) = loop(r, mergeAux(getAux a, z))
             in loop(xs, baseAux)
            end

      fun exitAux(AX_REG(b, vs)) = AX_REG(b, exitLevel vs)
        | exitAux x = x

      fun tc_aux tc = 
        let fun g (TC_VAR(d, i)) = AX_REG(true, [tvToInt(d, i)])
              | g (TC_NVAR(v, d, i)) = baseAux (*** THIS IS WRONG ! ***)
              | g (TC_PRIM pt) = baseAux
              | g (TC_APP(ref(_, TC_FN _, AX_NO), _)) = AX_NO
              | g (TC_PROJ(ref(_, TC_SEQ _, AX_NO), _)) = AX_NO
              | g (TC_APP(ref(_, TC_FN _, AX_REG(_,vs)), ts)) = 
                     mergeAux(AX_REG(false, vs), fsmerge ts)       (* ? *)
              | g (TC_PROJ(ref(_, TC_SEQ _, AX_REG(_,vs)), _)) = 
                     AX_REG(false, vs)                             (* ? *)
              | g (TC_FN(ks, t)) = exitAux(getAux t)
              | g (TC_APP(t, ts)) = fsmerge (t::ts)
              | g (TC_SEQ ts) = fsmerge ts
              | g (TC_PROJ(t, _)) = getAux t
              | g (TC_SUM ts) = fsmerge ts
              | g (TC_FIX((_,t,ts), _)) = 
                     let val ax = getAux t
                      in case ax
                          of AX_REG(_,[]) => mergeAux(ax, fsmerge ts)
                           | _ => bug "unexpected TC_FIX freevars in tc_aux"
                     end
              | g (TC_ABS t) = getAux t
              | g (TC_BOX t) = getAux t
              | g (TC_TUPLE (_, ts)) = fsmerge ts
              | g (TC_ARROW(_, ts1, ts2)) = fsmerge (ts1@ts2)
              | g (TC_PARROW(t1, t2)) = fsmerge [t1, t2]
              | g (TC_CONT ts) = fsmerge ts
              | g (TC_IND _) = bug "unexpected TC_IND in tc_aux"
              | g (TC_ENV _) = AX_NO
         in g tc
        end 
        
      fun lt_aux lt = 
        let fun g (LT_TYC t) = getAux t
              | g (LT_STR ts) = fsmerge ts
              | g (LT_PST ts) = fsmerge (map #2 ts)
              | g (LT_FCT(ts1, ts2)) = fsmerge (ts1@ts2)
              | g (LT_POLY(ks, ts)) = exitAux(fsmerge ts)
              | g (LT_CONT ts) = fsmerge ts
              | g (LT_IND _) = bug "unexpected LT_IND in lt_aux"
              | g (LT_ENV _) = AX_NO
         in g lt
        end

      fun tk_mk (i : int, k: tkindI) = ref (i, k, AX_NO)
      fun tc_mk (i : int, tc : tycI) = ref (i, tc, tc_aux tc)
      fun lt_mk (i : int, lt : ltyI) = ref (i, lt, lt_aux lt)

in 

(** a temporary hack on getting the list of free tyvars *)
fun tc_vs (r as ref(_ : int, _ : tycI, AX_NO)) = NONE
  | tc_vs (r as ref(_ : int, _ : tycI, AX_REG (_,x))) = SOME x

fun lt_vs (r as ref(_ : int, _ : ltyI, AX_NO)) = NONE
  | lt_vs (r as ref(_ : int, _ : ltyI, AX_REG (_,x))) = SOME x


(** converting from the hash-consing reps to the standard reps *)
fun tk_outX (r as ref(_ : int, t : tkindI, _ : aux_info)) = t
fun tc_outX (r as ref(_ : int, t : tycI, _ : aux_info)) = t
fun lt_outX (r as ref(_ : int, t : ltyI, _ : aux_info)) = t


(** converting from the standard reps to the hash-consing reps *)
fun tk_injX t = look(tk_table, wtoi(tk_hash t), t, tkI_eq, tk_mk)
fun tc_injX t = look(tc_table, wtoi(tc_hash t), t, tcI_eq, tc_mk)
fun lt_injX t = look(lt_table, wtoi(lt_hash t), t, ltI_eq, lt_mk)


(** key-comparison on tkind, tyc, lty *)
fun tk_cmp (k1, k2) = cmp(tk_table, k1, k2)
fun tc_cmp (t1, t2) = cmp(tc_table, t1, t2)
fun lt_cmp (t1, t2) = cmp(lt_table, t1, t2)


(** get the hash key of each lty, only used by reps/coerce.sml; a hack *)
fun lt_key (ref (h : int, _ : ltyI, _ : aux_info)) = h

(***************************************************************************
 *            UTILITY FUNCTIONS ON TYC ENVIRONMENT                         *
 ***************************************************************************)

(** utility functions for manipulating the tycEnv *)
local val tc_void = tc_injX(TC_PRIM(PT.ptc_void))
      val rawtt = (true, true)
      fun tc_cons (t, b) = tc_injX(TC_ARROW(rawtt, [t],[b]))
      fun tc_interp x = 
        (case tc_outX x
          of TC_PROJ(y, i) =>
               (case tc_outX y of TC_SEQ ts => (SOME ts, i)
                               | TC_PRIM _ => (NONE, i)
                               | _ => bug "unexpected tycEnv1 in tc_interp")
           | _ => bug "unexpected tycEnv2 in tc_interp")

      fun tc_encode(NONE, i) = tc_injX(TC_PROJ(tc_void,i))
        | tc_encode(SOME ts, i) = 
            tc_injX(TC_PROJ(tc_injX(TC_SEQ(ts)), i))

in

exception tcUnbound
val initTycEnv : tycEnv = tc_void

fun tcLookup(i, tenv : tycEnv) = 
      if i > 1 then
        (case tc_outX tenv of TC_ARROW(_,_,[x]) => tcLookup(i-1, x)
                            | _ => bug "unexpected tycEnv in tcLookup")
      else if i = 1 then
             (case tc_outX tenv of TC_ARROW(_,[x],_) => tc_interp x 
                                 | _ => raise tcUnbound)
           else bug "unexpected argument in tcLookup"

fun tcInsert(tenv : tycEnv, et) = tc_cons(tc_encode et, tenv)

fun tcSplit(tenv : tycEnv) =
  (case tc_outX tenv of TC_ARROW(_,[x],[y]) => SOME (tc_interp x, y)
                     | _ => NONE)
  

end (* utililty function for tycEnv *)


(** checking if a tyc or an lty is in the normal form *)
fun tcp_norm ((t as ref (i, _, AX_REG(b,_))) : tyc) =  b
  | tcp_norm _ = false

fun ltp_norm ((t as ref (i, _, AX_REG(b,_))) : lty) =  b
  | ltp_norm _ = false


(** utility functions for tc_env and lt_env *)
local fun tcc_env_int(x, 0, 0, te) = x
        | tcc_env_int(x, i, j, te) = tc_injX(TC_ENV(x, i, j, te))

      fun ltc_env_int(x, 0, 0, te) = x
        | ltc_env_int(x, i, j, te) = lt_injX(LT_ENV(x, i, j, te))
 
      fun withEff ([], ol, nl, tenv) = false
        | withEff (a::r, ol, nl, tenv) = 
            let val (i, j) = tvFromInt a
                val neweff = 
                  if i > ol then (ol <> nl)
                  else (* case tcLookup(i, tenv)
                           of (NONE, n) => (nl - n) <> i
                            | (SOME ts, n) =>
                                 (let val y = List.nth(ts, j)
                                   in (case tc_outX y
                                        of TC_VAR(ni, nj) =>
                                            ((nj <> j) orelse ((ni+nl-n) <> i))
                                         | _ => true)
                                  end) *) true
             in neweff orelse (withEff(r, ol, nl, tenv))
            end

in 

fun tcc_env(x, ol, nl, tenv) =
  let val tvs = tc_vs x
   in case tvs
       of NONE => tcc_env_int(x, ol, nl, tenv)
        | SOME [] => x
        | SOME nvs => if withEff(nvs, ol, nl, tenv) 
                      then tcc_env_int(x, ol, nl, tenv)
                      else x 
  end

fun ltc_env(x, ol, nl, tenv) = 
  let val tvs = lt_vs x
   in case tvs
       of NONE => ltc_env_int(x, ol, nl, tenv)
        | SOME [] => x
        | SOME nvs => if withEff (nvs, ol, nl, tenv) 
                      then ltc_env_int(x, ol, nl, tenv)
                      else x 
  end

end (* utility functions for lt_env and tc_env *)


(** utility functions for updating tycs and ltys *)
fun tyc_upd (tgt as ref(i : int, old : tycI, AX_NO), nt) = 
      (tgt := (i, TC_IND (nt, old), AX_NO))
  | tyc_upd (tgt as ref(i : int, old : tycI, x as (AX_REG(false, _))), nt) = 
      (tgt := (i, TC_IND (nt, old), x))
  | tyc_upd _ = bug "unexpected tyc_upd on already normalized tyc"

fun lty_upd (tgt as ref(i : int, old : ltyI, AX_NO), nt) = 
      (tgt := (i, LT_IND (nt, old), AX_NO))
  | lty_upd (tgt as ref(i : int, old : ltyI, x as (AX_REG(false, _))), nt) = 
      (tgt := (i, LT_IND (nt, old), x))
  | lty_upd _ = bug "unexpected lty_upd on already normalized lty"

(***************************************************************************
 *            UTILITY FUNCTIONS ON REASONING ABOUT REDUCTIONS              *
 ***************************************************************************)

(** a list of constructor functions *)
val tcc_var = tc_injX o TC_VAR
val tcc_fn = tc_injX o TC_FN
val tcc_app = tc_injX o TC_APP
val tcc_seq = tc_injX o TC_SEQ
val tcc_proj = tc_injX o TC_PROJ
val tcc_fix = tc_injX o TC_FIX
val tcc_abs = tc_injX o TC_ABS
val tcc_tup  = tc_injX o TC_TUPLE
val tcc_parw = tc_injX o TC_PARROW
val ltc_tyc = lt_injX o LT_TYC
val ltc_str = lt_injX o LT_STR
val ltc_pst = lt_injX o LT_PST
val ltc_fct = lt_injX o LT_FCT
val ltc_poly = lt_injX o LT_POLY
val tcc_sum = tc_injX o TC_SUM

(** the following function contains the procedure on how to
    flatten the arguments and results of an arbitrary FLINT function
 *) 
fun isKnown tc = 
  (case tc_outX(tc_whnm tc)
    of (TC_PRIM _ | TC_ARROW _ | TC_BOX _ | TC_ABS _ | TC_PARROW _) => true 
     | (TC_CONT _ | TC_FIX _ | TC_SUM _ | TC_TUPLE _) => true
     | TC_APP(tc, _) => isKnown tc
     | TC_PROJ(tc, _) => isKnown tc
     | _ => false)

and tc_autoflat tc = 
  let val ntc = tc_whnm tc 
   in (case tc_outX ntc
        of TC_TUPLE (_, [_]) => (* singleton record is not flattened to ensure
                              isomorphism btw plambdatype and flinttype *)
             (true, [ntc], false)
         | TC_TUPLE (_, ts) => 
             if length ts < 10 then (true, ts, true)
             else (true, [ntc], false)  (* ZHONG added the magic number 10 *)
         | _ => if isKnown ntc then (true, [ntc], false)
                else (false, [ntc], false))
  end

and tc_autotuple [x] = x 
  | tc_autotuple xs = 
       if length xs < 10 then tcc_tup (default_rflag, xs)
       else bug "fatal error with tc_autotuple"

and tcs_autoflat (flag, ts) = 
  if flag then (flag, ts) 
  else (case ts 
         of [tc] => (let val (nraw, ntcs, _) = tc_autoflat tc
                      in (nraw, ntcs)
                     end)
          | _ => bug "unexpected cooked multiples in tcs_autoflat")

and lt_autoflat lt = 
  (case lt_outX(lt_whnm lt)
    of LT_TYC tc => 
         let val (raw, ts, flag) = tc_autoflat tc
          in (raw, map ltc_tyc ts, flag)
         end
     | _ => (true, [lt], false))

(** a special version of tcc_arw that does automatic flattening *)
and tcc_arw  (x as ((true, true), ts1, ts2)) = tc_injX (TC_ARROW x)
  | tcc_arw  (b as (b1, b2), ts1, ts2) =
      let val (nb1, nts1) = tcs_autoflat (b1, ts1)
          val (nb2, nts2) = tcs_autoflat (b2, ts2)
       in tc_injX (TC_ARROW((nb1, nb2),  nts1, nts2))
      end

(** tcc_wrap applies to tyc of all kinds *)
and tcc_wrap t = 
  let val nt = tc_whnm t
   in (case tc_outX nt  (* follow the kind relationship *)
        of TC_SEQ ts => tcc_seq (map tcc_wrap ts)
         | TC_FN(ks, tc) =>  tcc_fn(ks, tcc_wrap tc)
         | TC_APP(t, ts) => tcc_app(tcc_wrap t, map tcc_wrap ts)
         | (TC_PROJ _ | TC_VAR _ | TC_NVAR _) => nt
         | TC_PRIM pt => if PT.pt_arity pt > 0 then nt else tcc_box nt
         | _ => tcc_box nt)
  end (* function tc_wrap *)

(** tcc_box only applies to tyc of kind tkc_mono *)     
and tcc_box t = 
  let val nt = tcc_uncv t (* must produce a whnm *)
   in (case tc_outX nt
        of (TC_VAR _ | TC_NVAR _ | TC_APP _ | TC_PROJ _) => nt
         | (TC_FIX _ | TC_SUM _) => nt  (* simplification here *)
         | TC_PRIM pt => if PT.unboxed pt then tc_injX(TC_BOX nt) else nt
         | (TC_TUPLE _ | TC_ARROW _) => tc_injX(TC_BOX nt)
         | TC_BOX _ => bug "unexpected TC_BOX in tcc_box"
         | (TC_SEQ _ | TC_FN _) => bug "unexpected tyc (SEQ/FN) in tcc_box"
         | _ => bug "unsupported tycs in tcc_box")
  end

(** tcc_uncv is to recursively box a tyc of kind tkc_mono *)
and tcc_uncv t = 
  let val nt = tc_whnm t
   in (case tc_outX nt
        of (TC_VAR _ | TC_NVAR _ | TC_APP _ | TC_PROJ _) => nt
         | (TC_FIX _ | TC_SUM _ | TC_PRIM _) => nt  (* simplified here *)
         | (TC_SEQ _ | TC_FN _) => bug "unexpected tyc (SEQ/FN) in tcc_box"
         | TC_BOX x => x
         | TC_TUPLE (rk, ts) => tcc_tup(rk, map tcc_uncv ts)
(*
         | TC_ARROW ((b1,b2), ts1, ts2) => 
             let val nts1 = map tcc_uncv ts1
                 val nts2 = map tcc_uncv ts2
                 val nts1 = 
                   case (b1, ts1)
                    of (_, [t11, t12]) => [tcc_box t11, tcc_box t12]
                     | (true, _) => tcc_box(tc_autotuple ts1) 
                     | _ => bug "not implemented"
                         
*)
         | TC_ARROW ((true,b2), [t11,t12], ts2) => 
             let val nt11 = tcc_box t11
                 val nt12 = tcc_box t12
                 val t2 = tcc_box(tc_autotuple ts2)
                 (* after boxing, all calling conventions are fixed ! *)
              in tcc_arw((true,true),[nt11,nt12],[t2])
             end
         | TC_ARROW ((b1,b2), ts1, ts2) => 
             let val t1 = tcc_box(tc_autotuple ts1)
                 val t2 = tcc_box(tc_autotuple ts2)
                 (* after boxing, all calling conventions are fixed ! *)
              in tcc_arw((true,true),[t1],[t2])
             end
         | _ => bug "unsupported tycs in tcc_box")
  end

(** utility function to read the top-level of a tyc *)
and tc_lzrd t = 
  let fun g x = 
            (case tc_outX x
              of TC_IND (tc, _) => g tc
               | TC_ENV (tc, i, j, te) => 
                   let val ntc = g(h(tc, i, j, te))
                    in tyc_upd(x, ntc); ntc
                   end
               | _ => x)

      and h (x, 0, 0, _) = g x
        | h (x, ol, nl, tenv) = 
            let fun prop z = tcc_env(z, ol, nl, tenv)
             in (case tc_outX x
                  of TC_VAR (i,j) => 
                       if (i <= ol) then
                         (let val et = tcLookup(i, tenv)
                           in case et 
                               of (NONE, n) => tcc_var(nl - n, j)
                                | (SOME ts, n) => 
                                    (let val y = List.nth(ts, j) 
                                           handle _ => raise tcUnbound
                                      in h(y, 0, nl - n, initTycEnv)
                                     end)
                          end)
                       else tcc_var(i-ol+nl, j)
                   | TC_NVAR _ => x
                   | TC_PRIM _ => x
                   | TC_FN (ks, tc) => 
                      let val tenv' = tcInsert(tenv, (NONE, nl))
                       in tcc_fn(ks, tcc_env(tc, ol+1, nl+1, tenv'))
                      end
                   | TC_APP (tc, tcs) => tcc_app(prop tc, map prop tcs)
                   | TC_SEQ tcs => tcc_seq (map prop tcs)
                   | TC_PROJ (tc, i) => tcc_proj(prop tc, i)
                   | TC_SUM tcs => tcc_sum (map prop tcs)
                   | TC_FIX ((n,tc,ts), i) => 
                        tcc_fix((n, prop tc, map prop ts), i)
                   | TC_ABS tc => tcc_abs (prop tc)
                   | TC_BOX tc => tcc_box (prop tc)
                   | TC_TUPLE (rk, tcs) => tcc_tup (rk, map prop tcs)
                   | TC_ARROW (r, ts1, ts2) => 
                       tcc_arw (r, map prop ts1, map prop ts2)
                   | TC_PARROW (t1, t2) => tcc_parw (prop t1, prop t2)
                   | TC_CONT _ => bug "unexpected TC_CONT in tc_lzrd"
                   | TC_IND (tc, _) => h(tc, ol, nl, tenv)
                   | TC_ENV(tc, ol', nl', tenv') => 
                       if ol = 0 then h(tc, ol', nl+nl', tenv')
                       else h(g x, ol, nl, tenv))
            end (* function h *)
   in if tcp_norm(t) then t else g t
  end (* function tc_lzrd *)

(** utility function to read the top-level of an lty *)
and lt_lzrd t = 
  let fun g x = 
           (case lt_outX x
             of LT_IND (lt, _) => g lt
              | LT_ENV(lt, i, j, te) => 
                  let val nlt = g(h(lt, i, j, te))
                   in lty_upd(x, nlt); nlt
                  end
              | _ => x)

      and h (x, 0, 0, _) = g x
        | h (x, ol, nl, tenv) = 
            let fun prop z = ltc_env(z, ol, nl, tenv)
             in (case lt_outX x
                  of LT_TYC tc => ltc_tyc (tcc_env(tc, ol, nl, tenv))
                   | LT_STR ts => ltc_str (map prop ts)
                   | LT_PST its => ltc_pst (map (fn (i, t) => (i, prop t)) its)
                   | LT_FCT (ts1, ts2) => ltc_fct(map prop ts1, map prop ts2)
                   | LT_POLY (ks, ts) => 
                       let val tenv' = tcInsert(tenv, (NONE, nl))
                        in ltc_poly(ks, 
                             map (fn t => ltc_env(t, ol+1, nl+1, tenv')) ts)
                       end
                   | LT_CONT _ => bug "unexpected LT_CONT in lt_lzrd"
                   | LT_IND (t, _) => h(t, ol, nl, tenv)
                   | LT_ENV (lt, ol', nl', tenv') => 
                       if ol = 0 then h(lt, ol', nl+nl', tenv')
                       else h(g x, ol, nl, tenv))
            end (* function h *)
   in if ltp_norm(t) then t else g t
  end (* function lt_lzrd *)

(** taking out the TC_IND indirection *)
and stripInd t = (case tc_outX t of TC_IND (x,_) => stripInd x | _ => t)

(** normalizing an arbitrary tyc into a simple weak-head-normal-form *)
and tc_whnm t = if tcp_norm(t) then t else 
  let val nt = tc_lzrd t
   in case (tc_outX nt)
       of TC_APP(tc, tcs) =>
            (let val tc' = tc_whnm tc
              in case (tc_outX tc')
                  of TC_FN(ks, b) =>  
                       let fun base () = 
                             (b, 1, 0, tcInsert(initTycEnv,(SOME tcs, 0)))
                           val sp = 
                             (case tc_outX b
                               of TC_ENV(b', ol', nl', te') => 
                                    (case tcSplit te'
                                      of SOME((NONE, n), te) =>
                                           if (n = nl'-1) andalso (ol' > 0)
                                           then (b', ol', n, 
                                                 tcInsert(te, (SOME tcs, n)))
                                           else base()
                                       | _ => base())
                                | _ => base())
                           val res = tc_whnm(tcc_env sp)
                        in tyc_upd(nt, res); res
                       end
                   | ((TC_SEQ _) | (TC_TUPLE _) | (TC_ARROW _) | (TC_IND _)) =>
                       bug "unexpected tycs in tc_whnm-TC_APP"
                   | _ => let val xx = tcc_app(tc', tcs) 
                           in stripInd xx
                          end
(*
                       let fun h x = 
                             let val nx = tc_whnm x
                              in (case tc_outX nx
                                   of TC_BOX z => h z
                                    | TC_ABS z => h z
                                    | _ => nx)
                             end
                        in tcc_app(tc', map h tcs)
                       end
*)
             end)
        | TC_PROJ(tc, i) =>
            (let val tc' = tc_whnm tc
              in (case (tc_outX tc')
                   of (TC_SEQ tcs) => 
                        let val res = List.nth(tcs, i)
                                      handle _ => bug "TC_SEQ in tc_whnm"
                            val nres = tc_whnm res
                         in tyc_upd(nt, nres); nres
                        end
                    | ((TC_PRIM _) | (TC_NVAR _) | (TC_FIX _) | (TC_FN _) |
                       (TC_SUM _) | (TC_ARROW _) | (TC_ABS _) | (TC_BOX _) | 
                       (TC_IND _) | (TC_TUPLE _)) =>
                         bug "unexpected tycs in tc_whnm-TC_PROJ"
                    | _ => let val xx = tcc_proj(tc', i)
                            in stripInd xx
                           end)
             end)
        | TC_IND (tc, _) => tc_whnm tc
        | TC_ENV _ => bug "unexpected TC_ENV in tc_whnm"
        | _ => nt
  end (* function tc_whnm *)

(** normalizing an arbitrary lty into the simple weak-head-normal-form *)
and lt_whnm t = if ltp_norm(t) then t else 
  let val nt = lt_lzrd t
   in case (lt_outX nt)
       of LT_TYC tc => ltc_tyc(tc_whnm tc)
        | _ => nt
  end (* function lt_whnm *)

(** normalizing an arbitrary tyc into the standard normal form *)
fun tc_norm t = if (tcp_norm t) then t else
  let val nt = tc_whnm t
   in if (tcp_norm nt) then nt
      else
        (let val res = 
              (case (tc_outX nt)
                of TC_FN (ks, tc) => tcc_fn(ks, tc_norm tc)
                 | TC_APP (tc, tcs) => 
                     tcc_app(tc_norm tc, map tc_norm tcs)
                 | TC_SEQ tcs => tcc_seq(map tc_norm tcs)
                 | TC_PROJ (tc, i) => tcc_proj(tc_norm tc, i)
                 | TC_SUM tcs => tcc_sum (map tc_norm tcs)
                 | TC_FIX ((n,tc,ts), i) => 
                     tcc_fix((n, tc_norm tc, map tc_norm ts), i)
                 | TC_ABS tc => tcc_abs(tc_norm tc)
                 | TC_BOX tc => tcc_box(tc_norm tc)
                 | TC_TUPLE (rk, tcs) => tcc_tup(rk, map tc_norm tcs)
                 | TC_ARROW (r, ts1, ts2) => 
                     tcc_arw(r, map tc_norm ts1, map tc_norm ts2)
                 | TC_PARROW (t1, t2) => tcc_parw(tc_norm t1, tc_norm t2)
                 | TC_IND (tc, _) => tc_norm tc
                 | TC_ENV _ => bug "unexpected tycs in tc_norm"
                 | _ => nt)
          in tyc_upd(nt, res); res
         end)
  end (* function tc_norm *)

(** normalizing an arbitrary lty into the standard normal form *)
fun lt_norm t = if (ltp_norm t) then t else 
  let val nt = lt_lzrd t
   in if (ltp_norm nt) then nt
      else 
        (let val res = 
              (case lt_outX nt
                of LT_TYC tc => ltc_tyc(tc_norm tc)
                 | LT_STR ts => ltc_str(map lt_norm ts)
                 | LT_PST its => 
                     ltc_pst(map (fn (i, t) => (i, lt_norm t)) its)
                 | LT_FCT (ts1, ts2) => 
                     ltc_fct(map lt_norm ts1, map lt_norm ts2)
                 | LT_POLY (ks, ts) => ltc_poly (ks, map lt_norm ts)
                 | LT_IND (lt, _) => lt_norm lt
                 | _ => bug "unexpected ltys in lt_norm")
          in lty_upd(nt, res); res
         end)
  end (* function lt_norm *)

(***************************************************************************
 *         REBINDING THE INJECTION AND PROJECTION FUNCTIONS                *
 ***************************************************************************)
(** converting from the standard reps to the hash-consing reps *)
val tk_inj = tk_injX
val tc_inj = tc_injX
val lt_inj = lt_injX

(** converting from the hash-consing reps to the standard reps *)
val tk_out = tk_outX 
val tc_out = tc_outX o tc_whnm
val lt_out = lt_outX o lt_whnm

(***************************************************************************
 *         UTILITY FUNCTIONS ON TESTING EQUIVALENCE                        *
 ***************************************************************************)

(** testing the equality of values of tkind, tyc, lty *)
fun eqlist (p, x::xs, y::ys) = (p(x,y)) andalso (eqlist(p, xs, ys))
  | eqlist (p, [], []) = true
  | eqlist _ = false

(** testing the "pointer" equality on normalized tkind, tyc, and lty *)
fun tk_eq (x: tkind, y) = (x = y)
fun tc_eq (x: tyc, y) = (x = y)
fun lt_eq (x: lty, y) = (x = y)

(** testing the equivalence for arbitrary tkinds, tycs and ltys *)
val tk_eqv = tk_eq       (* all tkinds are normalized *)

(** tc_eqv_generator, invariant: t1 and t2 are in the wh-normal form *)
fun tc_eqv_gen (eqop1, eqop2, eqop3, eqop4) (t1, t2) = 
  (case (tc_outX t1, tc_outX t2)
    of (TC_FN(ks1, b1), TC_FN(ks2, b2)) =>
         (eqlist(tk_eqv, ks1, ks2)) andalso (eqop2(b1, b2))
     | (TC_APP(a1, b1), TC_APP(a2, b2)) =>
         (eqop1(a1, a2)) andalso (eqlist(eqop2, b1, b2))
     | (TC_SEQ ts1, TC_SEQ ts2) => eqlist(eqop1, ts1, ts2)
     | (TC_SUM ts1, TC_SUM ts2) => eqlist(eqop1, ts1, ts2)
     | (TC_TUPLE (_, ts1), TC_TUPLE (_, ts2)) => eqlist(eqop1, ts1, ts2)
     | (TC_ABS a, TC_ABS b) => eqop1(a, b)
     | (TC_ABS a, _) => eqop3(a, t2)
     | (_, TC_ABS b) => eqop3(t1, b)
     | (TC_BOX a, TC_BOX b) => eqop1(a, b)
     | (TC_BOX a, _) => eqop3(a, t2)
     | (_, TC_BOX b) => eqop3(t1, b)
     | (TC_PROJ(a1, i1), TC_PROJ(a2, i2)) =>
         (i1 = i2) andalso (eqop1(a1, a2))
     | (TC_ARROW(r1, a1, b1), TC_ARROW(r2, a2, b2)) => 
         (r1 = r2) andalso (eqop4(a1, a2)) andalso (eqop4(b1, b2))
     | (TC_PARROW(a1, b1), TC_PARROW(a2, b2)) => 
         (eqop1(a1, a2)) andalso (eqop1(b1, b2))
     | (TC_FIX((n1,tc1,ts1), i1), TC_FIX((n2,tc2,ts2), i2)) => 
         true  (* INCORRECT: this is temporary (ZHONG) *)
     | (TC_CONT ts1, TC_CONT ts2) => eqlist(eqop1, ts1, ts2)
     | _ => false)

fun tc_eqv (x as ref (_, _, AX_REG(true, _)), 
            y as ref (_, _, AX_REG(true, _))) = tc_eq(x,y)
  | tc_eqv (x, y) =
      let val t1 = tc_whnm x
          val t2 = tc_whnm y
       in if (tcp_norm t1) andalso (tcp_norm t2) then tc_eq(t1, t2)
          else tc_eqv_gen (tc_eqv, tc_eqv, fn _ => false,
                           fn (ts1, ts2) => eqlist(tc_eqv, ts1, ts2)) (t1, t2)
      end (* function tc_eqv *)

(** testing the equivalence of two tycs with relaxed constraints *)
fun tc_eqv_bx (x : tyc, y) =
  let val t1 = tc_whnm x
      val t2 = tc_whnm y
   in (if (tcp_norm t1) andalso (tcp_norm t2) then tc_eq(t1, t2)
      else false) orelse 
      (tc_eqv_gen (tc_eqv_bx, tc_eqv_sp, fn _ => false,
                   fn (ts1, ts2) => tc_eqv_bx(tc_autotuple ts1,
                                              tc_autotuple ts2)) (t1, t2))
  end (* function tc_eqv_bx *)

and tc_eqv_sp (x : tyc, y) = 
  let val t1 = tc_whnm x
      val t2 = tc_whnm y
   in (if (tcp_norm t1) andalso (tcp_norm t2) then tc_eq(t1, t2)
      else false) orelse
      (tc_eqv_gen (tc_eqv_sp, tc_eqv_sp, tc_eqv_sp,
                   fn (ts1, ts2) => tc_eqv_sp(tc_autotuple ts1,
                                              tc_autotuple ts2)) (t1, t2))
  end (* function tc_eqv_sp *)

(* 
 * all the complexity of lt_eqv comes from the partial-structure (or
 * partial record) type (the LT_PST type). If we can remove LT_PST
 * type, then the following can be considerabily simplified. (ZHONG)
 *)

(** lt_eqv_generator, invariant: x and y are in the wh-normal form *)
fun lt_eqv_gen (eqop1, eqop2) (x : lty, y) = 
  let fun sp (r, []) = true
        | sp (r, (i,t)::s) = 
            (if (eqop1(List.nth(r,i),t)) 
             then sp(r,s) else false) handle _ => false

      fun pp ([], _) = true
        | pp (_, []) = true
        | pp (a as ((i,t)::l), b as ((j,s)::r)) = 
            if i > j then pp(a,r) 
            else if i < j then pp(l,b) 
                 else if (eqop1(t,s)) then pp(l,r) else false 

      (* seq should be called if t1 and t2 are weak-head normal form *)
      fun seq (t1, t2) = 
        (case (lt_outX t1, lt_outX t2)
          of (LT_POLY(ks1, b1), LT_POLY(ks2, b2)) =>
               (eqlist(tk_eqv, ks1, ks2)) andalso (eqlist(eqop1, b1, b2))
           | (LT_FCT(as1, bs1), LT_FCT(as2, bs2)) => 
               (eqlist(eqop1, as1, as2)) andalso (eqlist(eqop1, bs1, bs2))
           | (LT_TYC a, LT_TYC b) => eqop2(a, b)
           | (LT_STR s1, LT_STR s2) => eqlist(eqop1, s1, s2)
           | (LT_PST s1, LT_PST s2) => pp(s1, s2)
           | (LT_PST s1, LT_STR s2) => sp(s2, s1)
           | (LT_STR s1, LT_PST s2) => sp(s1, s2)
           | (LT_CONT s1, LT_CONT s2) => eqlist(eqop1, s1, s2)
           | _ => false)
   in seq(x, y)
  end (* function lt_eqv_gen *)

fun lt_eqv(x : lty, y) = 
  let val seq = lt_eqv_gen (lt_eqv, tc_eqv) 
   in if (ltp_norm x) andalso (ltp_norm y) then 
        (lt_eq(x, y)) orelse (seq(x, y))  
      else (let val t1 = lt_whnm x
                val t2 = lt_whnm y
             in if (ltp_norm t1) andalso (ltp_norm t2) then 
                 (lt_eq(t1, t2)) orelse (seq(t1, t2))  
                else seq(t1, t2)
            end)
  end (* function lt_eqv *)

fun lt_eqv_bx (x : lty, y) = 
  let val seq = lt_eqv_gen (lt_eqv_bx, tc_eqv_bx) 
   in if (ltp_norm x) andalso (ltp_norm y) then 
        (lt_eq(x, y)) orelse (seq(x, y))  
      else (let val t1 = lt_whnm x
                val t2 = lt_whnm y
             in if (ltp_norm t1) andalso (ltp_norm t2) then 
                 (lt_eq(t1, t2)) orelse (seq(t1, t2))  
                else seq(t1, t2)
            end)
  end (* function lt_eqv_bx *)

(***************************************************************************
 *  UTILITY FUNCTIONS ON FINDING OUT THE DEPTH OF THE FREE TYC VARIABLES   *
 ***************************************************************************)
(** finding out the innermost binding depth for a tyc's free variables *)
fun tc_depth (x, d) =
  let val tvs = tc_vs (tc_norm x) 
      (* unfortunately we have to reduce everything to the normal form
         before we can talk about its list of free type variables.
       *)
   in case tvs
       of NONE => bug "unexpected case in tc_depth"
        | SOME [] => DI.top
        | SOME (a::_) => d + 1 - (#1(tvFromInt a))
  end

fun tcs_depth ([], d) = DI.top
  | tcs_depth (x::r, d) = Int.max(tc_depth(x, d), tcs_depth(r, d))

end (* toplevel local *)
end (* abstraction LtyKernel *)

