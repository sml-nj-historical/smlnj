(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)
(* ltykernel.sml *)

structure LtyKernel :> LTYKERNEL = 
struct 

fun bug s = ErrorMsg.impossible ("LtyKernel:" ^ s)

(***************************************************************************
 *                UTILITY FUNCTIONS FOR HASHCONSING BASICS                 *
 ***************************************************************************)

(** hashconsing implementation basics *)
local open SortedList
      val MVAL = 10000
      val BVAL = MVAL * 2 (* all index i start from 0 *)
in 

type enc_tvar = int
fun tvEncode (d, i) = d * MVAL + i
fun tvDecode x = ((x div MVAL), (x mod MVAL))

fun exitLevel xs = 
  let fun h ([], x) = rev x
        | h (a::r, x) = if a < BVAL then h(r, x) else h(r, (a-MVAL)::x)
   in h(xs, [])
  end
  
(* definitions of named tyc variables.
   for now, these share the same namespace with lvars. *)
type tvar = LambdaVar.lvar
val mkTvar = LambdaVar.mkLvar

(* for lists of free type variables, debruijn indices are collapsed
   into a single integer using tvEncode/tvDecode, named variables use
   the tvar as an integer.  The debruijn-indexed list is kept sorted,
   the named variables are in arbitrary order (for now) --league, 2 July 1998
 *)
datatype aux_info = 
    AX_REG of bool                      (* normalization flag *)
            * enc_tvar list             (* free debruijn-indexed type vars *)
            * tvar list                 (* free named type vars *)
  | AX_NO                               (* no aux_info available *)

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

(* an special extensible token key *)
type token = int      

datatype fflag                                 (* calling conventions *)
  = FF_VAR of bool * bool                      (* is it fixed ? *)
  | FF_FIXED                                   (* used after rep. analysis *)

datatype rflag = RF_TMP                        (* tuple kind: a template *)

(** definitions of lambda type constructors *)
datatype tycI
  = TC_VAR of DebIndex.index * int             (* tyc variables *)
  | TC_NVAR of tvar                            (* named tyc variables *)
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
  | TC_TOKEN of token * tyc                    (* extensible token tyc *)
  | TC_CONT of tyc list                        (* intern continuation tycon *)
  | TC_IND of tyc * tycI                       (* indirect tyc thunk *)
  | TC_ENV of tyc * int * int * tycEnv         (* tyc closure *)

withtype tyc = tycI hash_cell                  (* hash-consed tyc cell *)
     and tycEnv = tyc     (* 
                           * This really is (tyc list option * int) list,
                           * it is encoded using SEQ[(PROJ(SEQ tcs),i)]
                           * and SEQ[(PROJ(VOID, i))]. (ZHONG)
                           *)

(** definitions of lambda types *)
datatype ltyI          
  = LT_TYC of tyc                              (* monomorphic type *)
  | LT_STR of lty list                         (* structure record type *)
  | LT_FCT of lty list * lty list              (* functor arrow type *)
  | LT_POLY of tkind list * lty list           (* polymorphic type *)

  | LT_CONT of lty list                        (* internal cont type *)
  | LT_IND of lty * ltyI                       (* a lty thunk and its sig *)
  | LT_ENV of lty * int * int * tycEnv         (* lty closure *)

withtype lty = ltyI hash_cell                  (* hash-consed lty cell *)

(***************************************************************************
 *                   TOKEN TYC UTILITY FUNCTIONS                           *
 ***************************************************************************)

type token_info  
  = {name      : string, 
     abbrev    : string,
     reduce_one: token * tyc -> tyc,
     is_whnm   : tyc -> bool,
     is_known  : token * tyc -> bool}

local val token_key = ref 0
      val token_table_size = 10
      val default_token_info = 
        {name="TC_GARBAGE", 
         abbrev="GB",
         reduce_one=(fn _ => bug "token not implemented"),
         is_whnm=(fn _ => bug "token not implemented"),
         is_known=(fn _ => bug "token not implemented")}
      val token_array : token_info Array.array =
            Array.array(token_table_size,default_token_info)
      val token_validity_table = Array.array(token_table_size,false)
      fun get_next_token () = 
        let val n = !token_key
         in if n > token_table_size then bug "running out of tokens"
            else (token_key := n+1; n)
        end
      fun store_token_info (x, k) = Array.update(token_array, k, x)
      fun get_is_whnm k = #is_whnm (Array.sub(token_array, k))
      fun get_reduce_one (z as (k, t)) = 
            (#reduce_one (Array.sub(token_array, k))) z
      fun get_name k = #name (Array.sub(token_array, k))
      fun get_abbrev k = #abbrev (Array.sub(token_array, k))
      fun get_is_known (z as (k, t)) = 
            (#is_known (Array.sub(token_array, k))) z
      fun is_valid k = Array.sub(token_validity_table, k)
      fun set_valid k = Array.update(token_validity_table, k, true)
in 

val register_token: token_info -> token = 
  (fn x => let val k = get_next_token ()
            in store_token_info(x, k); set_valid k; k
           end)

val token_name    : token -> string = get_name 
val token_abbrev  : token -> string = get_abbrev
val token_whnm    : token -> tyc -> bool = get_is_whnm 
val token_reduce  : token * tyc -> tyc = get_reduce_one
val token_isKnown : token * tyc -> bool = get_is_known
val token_isvalid : token -> bool = is_valid
val token_eq      : token * token -> bool = fn (x,y) => (x=y)
val token_int     : token -> int = fn x => x
val token_key     : int -> token = fn x => x

end (* end of all token-related hacks *)

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
              | g (TC_NVAR v) = combine[15, v]
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
                     let fun h (FF_FIXED) = 10
                           | h (FF_VAR(true,b2)) = if b2 then 20 else 30
                           | h (FF_VAR(false,b2)) = if b2 then 40 else 50
                      in combine (12::(h rw)::(map getnum (ts1@ts2)))
                     end
              | g (TC_PARROW (t1,t2)) = combine [13, getnum t1, getnum t2]
              | g (TC_TOKEN (i, tc)) = combine [14, i, getnum tc]
              | g (TC_CONT ts) = combine (15::(map getnum ts))
              | g (TC_ENV(t,i,j,env)) = 
                     combine[16, getnum t, i, j, getnum env]
              | g (TC_IND _) = bug "unexpected TC_IND in tc_hash"

         in g tc
        end 

      fun lt_hash lt = 
        let fun g (LT_TYC t) = combine [1, getnum t]
              | g (LT_STR ts) = combine (2::(map getnum ts))
              | g (LT_FCT(ts1, ts2)) = 
                     combine (3::(map getnum (ts1@ts2)))
              | g (LT_POLY(ks, ts)) = 
                     combine (4::((map getnum ts)@(map getnum ks)))
              | g (LT_CONT ts) = combine (5::(map getnum ts))
              | g (LT_ENV(t,i,j,env)) = 
                     combine [6, getnum t, i, j, getnum env]
              | g (LT_IND _) = bug "unexpected LT_IND in tc_hash"
         in g lt
        end

      fun tkI_eq {new: tkindI, old} = (new = old)
      
      (* the 1st is one being mapped; the 2nd is in the hash table *)
      fun tcI_eq {new : tycI, old=TC_IND(_,s)} = tcI_eq {new=new, old=s}
        | tcI_eq {new, old} = (new=old)

      fun ltI_eq {new : ltyI, old=LT_IND(_,s)} = ltI_eq {new=new, old=s}
        | ltI_eq {new, old} = (new=old)

      val baseAux = AX_REG (true, [], [])

      fun getAux (ref(i : int, _, x)) = x

      fun mergeAux(AX_NO, _) = AX_NO
        | mergeAux(_, AX_NO) = AX_NO
        | mergeAux(AX_REG(b1,vs1,nvs1), AX_REG(b2,vs2,nvs2)) =
            AX_REG(b2 andalso b1, mergeTvs(vs1, vs2),
                   mergeTvs(nvs1, nvs2))

      fun fsmerge [] = baseAux
        | fsmerge [x] = getAux x
        | fsmerge xs = 
            let fun loop([], z) = z
                  | loop(_, AX_NO) = AX_NO
                  | loop(a::r, z) = loop(r, mergeAux(getAux a, z))
             in loop(xs, baseAux)
            end

      fun exitAux(AX_REG(b, vs, nvs)) = AX_REG(b, exitLevel vs, nvs)
        | exitAux x = x

      fun tc_aux tc = 
        let fun g (TC_VAR(d, i)) = AX_REG(true, [tvEncode(d, i)], [])
              | g (TC_NVAR v) = AX_REG(true, [], [v])
              | g (TC_PRIM pt) = baseAux
              | g (TC_APP(ref(_, TC_FN _, AX_NO), _)) = AX_NO
              | g (TC_PROJ(ref(_, TC_SEQ _, AX_NO), _)) = AX_NO
              | g (TC_APP(ref(_, TC_FN _, AX_REG(_,vs,nvs)), ts)) = 
                     mergeAux(AX_REG(false, vs, nvs), fsmerge ts) (* ? *)
              | g (TC_PROJ(ref(_, TC_SEQ _, AX_REG(_,vs,nvs)), _)) = 
                     AX_REG(false, vs, nvs) (* ? *)
              | g (TC_FN(ks, t)) = exitAux(getAux t)
              | g (TC_APP(t, ts)) = fsmerge (t::ts)
              | g (TC_SEQ ts) = fsmerge ts
              | g (TC_PROJ(t, _)) = getAux t
              | g (TC_SUM ts) = fsmerge ts
              | g (TC_FIX((_,t,ts), _)) = 
                     let val ax = getAux t
                      in case ax
                          of AX_REG(_,[],[]) => mergeAux(ax, fsmerge ts)
                           | AX_REG _ => bug "unexpected TC_FIX freevars in tc_aux"
                           | AX_NO => AX_NO
                     end
              | g (TC_ABS t) = getAux t
              | g (TC_BOX t) = getAux t
              | g (TC_TUPLE (_, ts)) = fsmerge ts
              | g (TC_ARROW(_, ts1, ts2)) = fsmerge (ts1@ts2)
              | g (TC_PARROW(t1, t2)) = fsmerge [t1, t2]
              | g (TC_TOKEN (k, (ref(_, t, AX_NO)))) = AX_NO
              | g (TC_TOKEN (k, (x as ref(_, t, AX_REG(b,vs,nvs))))) = 
                     AX_REG((token_whnm k x) andalso b, vs, nvs)
              | g (TC_CONT ts) = fsmerge ts
              | g (TC_IND _) = bug "unexpected TC_IND in tc_aux"
              | g (TC_ENV _) = AX_NO
         in g tc
        end 
        
      fun lt_aux lt = 
        let fun g (LT_TYC t) = getAux t
              | g (LT_STR ts) = fsmerge ts
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
(* ignores named vars for now.  --CALeague, 1 Jul 1998 *)
fun tc_vs (r as ref(_ : int, _ : tycI, AX_NO)) = NONE
  | tc_vs (r as ref(_ : int, _ : tycI, AX_REG (_,x,_))) = SOME x

fun lt_vs (r as ref(_ : int, _ : ltyI, AX_NO)) = NONE
  | lt_vs (r as ref(_ : int, _ : ltyI, AX_REG (_,x,_))) = SOME x

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
 *            UTILITY FUNCTIONS ON TKIND ENVIRONMENT                       *
 ***************************************************************************)
(** tkind environment: maps each tyvar, i.e., its debindex, to its kind *)
type tkindEnv = tkind list list

(** utility functions for manipulating the tkindEnv *)
exception tkUnbound
val initTkEnv : tkindEnv = []

fun tkLookup (kenv, i, j) = 
  let val ks = List.nth(kenv, i-1) handle _ => raise tkUnbound
   in List.nth(ks, j) handle _ => raise tkUnbound
  end

fun tkInsert (kenv, ks) = ks::kenv

(* strip any unused type variables out of a kenv, given a list of
 * [encoded] free type variables.  the result is a "parallel list" of
 * the kinds of those free type variables in the environment.
 * This is meant to use the same representation of a kind environment
 * as in ltybasic.
 * --CALeague
 *)
fun tkLookupFreeVars (kenv, tyc) =
    let
	fun g (kenv, d, []) = []
	  | g (kenv, d, ftv::ftvs) =
	    let val (d', i') = tvDecode ftv
		val kenv' = List.drop (kenv, d'-d)
		    handle _ => raise tkUnbound
		val k = List.nth (hd kenv', i')
		    handle _ => raise tkUnbound
		val rest = g (kenv', d', ftvs)
	    in
		k :: rest
	    end

        fun h ftvs = g (kenv, 1, ftvs)
    in
        Option.map h (tc_vs tyc)
    end

(***************************************************************************
 *            UTILITY FUNCTIONS ON TYC ENVIRONMENT                         *
 ***************************************************************************)

(** utility functions for manipulating the tycEnv *)
local val tc_void = tc_injX(TC_PRIM(PT.ptc_void))
      fun tc_cons (t, b) = tc_injX(TC_ARROW(FF_FIXED, [t],[b]))
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
fun tcp_norm ((t as ref (i, _, AX_REG(b,_,_))) : tyc) =  b
  | tcp_norm _ = false

fun ltp_norm ((t as ref (i, _, AX_REG(b,_,_))) : lty) =  b
  | ltp_norm _ = false


(** utility functions for tc_env and lt_env *)
local fun tcc_env_int(x, 0, 0, te) = x
        | tcc_env_int(x, i, j, te) = tc_injX(TC_ENV(x, i, j, te))

      fun ltc_env_int(x, 0, 0, te) = x
        | ltc_env_int(x, i, j, te) = lt_injX(LT_ENV(x, i, j, te))
 
      fun withEff ([], ol, nl, tenv) = false
        | withEff (a::r, ol, nl, tenv) = 
            let val (i, j) = tvDecode a
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
  | tyc_upd (tgt as ref(i : int, old : tycI, x as (AX_REG(false,_,_))), nt) = 
      (tgt := (i, TC_IND (nt, old), x))
  | tyc_upd _ = bug "unexpected tyc_upd on already normalized tyc"

fun lty_upd (tgt as ref(i : int, old : ltyI, AX_NO), nt) = 
      (tgt := (i, LT_IND (nt, old), AX_NO))
  | lty_upd (tgt as ref(i : int, old : ltyI, x as (AX_REG(false,_,_))), nt) = 
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
val tcc_box = tc_injX o TC_BOX
val tcc_real = tc_injX (TC_PRIM PT.ptc_real)
val ltc_tyc = lt_injX o LT_TYC
val ltc_str = lt_injX o LT_STR
val ltc_fct = lt_injX o LT_FCT
val ltc_poly = lt_injX o LT_POLY
val tcc_sum = tc_injX o TC_SUM
val tcc_token = tc_injX o TC_TOKEN

(* The following functions decide on how to flatten the arguments 
 * and results of an arbitrary FLINT function. The current threshold
 * is maintained by the "flatten_limit" parameter. This parameter
 * is designed as architecture independent, however, some implicit
 * constraints are:
 *     (1) flatten_limit <= numgpregs - numcalleesaves - 3
 *     (2) flatten_limit <= numfpregs - 2
 * Right now (2) is in general not true for x86; we inserted a 
 * special hack at cpstrans phase to deal with this case. In the
 * long term, if the spilling phase in the backend can offer more
 * supports on large-number of arguments, then we can make this
 * flattening more aggressive. (ZHONG)
 *) 
val flatten_limit = 9  

fun isKnown tc = 
  (case tc_outX(tc_whnm tc)
    of (TC_PRIM _ | TC_ARROW _ | TC_BOX _ | TC_ABS _ | TC_PARROW _) => true 
     | (TC_CONT _ | TC_FIX _ | TC_SUM _ | TC_TUPLE _) => true
     | TC_APP(tc, _) => isKnown tc
     | TC_PROJ(tc, _) => isKnown tc
     | TC_TOKEN(k, x) => token_isKnown(k, x)
     | _ => false)

and tc_autoflat tc = 
  let val ntc = tc_whnm tc 
   in (case tc_outX ntc
        of TC_TUPLE (_, [_]) => (* singleton record is not flattened to ensure
                              isomorphism btw plambdatype and flinttype *)
             (true, [ntc], false)
         | TC_TUPLE (_, []) =>  (* unit is not flattened to avoid coercions *)
             (true, [ntc], false)
         | TC_TUPLE (_, ts) => 
             if length ts <= flatten_limit then (true, ts, true)
             else (true, [ntc], false)  (* ZHONG added the magic number 10 *)
         | _ => if isKnown ntc then (true, [ntc], false)
                else (false, [ntc], false))
  end

and tc_autotuple [x] = x 
  | tc_autotuple xs = 
       if length xs <= flatten_limit then tcc_tup (RF_TMP, xs)
       else bug "fatal error with tc_autotuple"

and tcs_autoflat (flag, ts) = 
  if flag then (flag, ts) 
  else (case ts 
         of [tc] => (let val ntc = tc_whnm tc
                         val (nraw, ntcs, _) = tc_autoflat ntc
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
and tcc_arw (x as (FF_FIXED, _, _)) = tc_injX (TC_ARROW x)
  | tcc_arw (x as (FF_VAR (true, true), _, _)) = tc_injX (TC_ARROW x)
  | tcc_arw (b as (FF_VAR (b1, b2)), ts1, ts2) =
      let val (nb1, nts1) = tcs_autoflat (b1, ts1)
          val (nb2, nts2) = tcs_autoflat (b2, ts2)
       in tc_injX (TC_ARROW(FF_VAR(nb1, nb2),  nts1, nts2))
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
                   | TC_TOKEN (k, t) => tcc_token(k, prop t)
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
        | TC_TOKEN(k, tc)  =>
            (let val tc' = tc_whnm tc
              in if token_whnm k tc' 
                 then let val xx = tcc_token(k, tc') in stripInd xx end
                 else let val res = token_reduce(k, tc')
                          val nres = tc_whnm res
                       in tyc_upd(nt, nres); nres
                      end
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
                 | TC_TOKEN (k, t) => tcc_token(k, tc_norm t)
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
                 | LT_FCT (ts1, ts2) => 
                     ltc_fct(map lt_norm ts1, map lt_norm ts2)
                 | LT_POLY (ks, ts) => ltc_poly (ks, map lt_norm ts)
                 | LT_IND (lt, _) => lt_norm lt
                 | _ => bug "unexpected ltys in lt_norm")
          in lty_upd(nt, res); res
         end)
  end (* function lt_norm *)

(***************************************************************************
 *         REGISTER A NEW TOKEN TYC --- TC_WRAP                            *
 ***************************************************************************)

(** we add a new constructor named TC_RBOX through the token facility *)
local val name = "TC_WRAP"
      val abbrev = "WR"
      val is_known = fn _ => true      (* why is this ? *)
      fun tcc_tok k t = tcc_token(k, t)

      fun unknown tc = 
        (case tc_outX tc
          of (TC_VAR _ | TC_NVAR _) => true
           | (TC_APP(tc, _)) => unknown tc
           | (TC_PROJ(tc, _)) => unknown tc
           | _ => false)         

      fun flex_tuple ts = 
        let fun hhh(x::r, ukn, wfree) = 
                 let fun iswp tc =
                       (case tc_outX tc
                         of TC_TOKEN(k', t) => (* WARNING: need check k' *)
                              (case tc_outX t
                                of TC_PRIM pt => false
                                 | _ => true)
                          | _ => true)
                  in hhh(r, (unknown x) orelse ukn, (iswp x) andalso wfree)
                 end
              | hhh([], ukn, wfree) = ukn andalso wfree
         in hhh(ts, false, true)
        end

      fun is_whnm tc = 
        (case tc_outX tc
          of (TC_ARROW(FF_FIXED, [t], _)) => (unknown t) 
           | (TC_TUPLE(rf, ts)) => flex_tuple ts
           | (TC_PRIM pt) => PT.unboxed pt
           | _ => false)

      (* invariants: tc itself is in whnm but is_whnm tc = false *)
      fun reduce_one (k, tc) =  
        (case tc_outX tc
          of TC_TUPLE (rk, ts) => 
               let fun hhh (x::r, nts, ukn) = 
                         let val nx = tc_whnm x
                             val b1 = unknown nx
                             val nnx = 
                               (case tc_outX nx
                                 of TC_TOKEN(k', t) =>
                                      if token_eq(k, k') then
                                        (case tc_outX t 
                                          of TC_PRIM _ => t
                                           | _ => nx)
                                      else nx
                                  | _ => nx)
                          in hhh(r, nnx::nts, b1 orelse ukn)
                         end
                     | hhh ([], nts, ukn) = 
                         let val nt = tcc_tup(rk, rev nts)
                          in if ukn then tcc_token(k, nt) else nt
                         end
                in hhh(ts, [], false)
               end
           | TC_ARROW (FF_FIXED, [_,_], [_]) => tc
           | TC_ARROW (FF_FIXED, [t1], ts2 as [_]) => 
               let val nt1 = tc_whnm t1
                   fun ggg z = 
                     let val nz = tc_whnm z
                      in (case tc_outX nz
                           of TC_PRIM pt => 
                                if PT.unboxed pt then tcc_token(k, nz)
                                else nz
                            | _ => nz)
                     end
                   val (wp, nts1) =
                     (case tc_outX nt1
                       of TC_TUPLE(_, [x,y]) => (false, [ggg x, ggg y])
                        | TC_TOKEN(k', x) => 
                            if token_eq(k, k') then
                              (case (tc_outX x)
                                of TC_TUPLE(_, [y, z]) => 
                                    (false, [ggg y, ggg z])
                                 | _ => (false, [nt1]))
                            else (false, [nt1])
                        | _ => (unknown nt1, [nt1]))
                   val nt = tcc_arw(FF_FIXED, nts1, ts2)
                in if wp then tcc_token(k, nt) else nt
               end
           | TC_ARROW (FF_FIXED, _, _) => 
               bug "unexpected reduce_one on ill-formed FF_FIX arrow types"
           | TC_ARROW (FF_VAR(b1,b2), ts1, ts2) => 
               bug "calling reduce_one on FF_VAR arrow types"
           | TC_PRIM pt => 
               if PT.unboxed pt then 
                 bug "calling reduce_one on an already-reduced whnm"
               else tc
           | TC_TOKEN(k', t) =>
               if token_eq(k, k') then tc
               else bug "unexpected token in reduce_one"
           | (TC_BOX _ | TC_ABS _ | TC_PARROW _) => 
               bug "unexpected tc_box/abs/parrow in reduce_one"
           | TC_ENV _ => bug "unexpected TC_ENV in reduce_one"
           | TC_IND _ => bug "unexpected TC_IND in reduce_one"
           | _ => tc)

in

val wrap_token = 
  register_token {name=name, abbrev=abbrev, reduce_one=reduce_one,
                  is_whnm=is_whnm, is_known=is_known}

end (* end of creating the box token for "tcc_rbox" *)

(** testing if a tyc is a unknown constructor *)
fun tc_unknown tc = not (isKnown tc)

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
fun eqlist p (x::xs, y::ys) = (p(x,y)) andalso (eqlist p (xs, ys))
  | eqlist p ([], []) = true
  | eqlist _ _ = false

(** testing the "pointer" equality on normalized tkind, tyc, and lty *)
fun tk_eq (x: tkind, y) = (x = y)
fun tc_eq (x: tyc, y) = (x = y)
fun lt_eq (x: lty, y) = (x = y)

(** testing the equivalence for arbitrary tkinds, tycs and ltys *)
val tk_eqv = tk_eq       (* all tkinds are normalized *)

local (* tyc equivalence utilities *)
(* The efficiency of checking FIX equivalence could probably be
 * improved somewhat, but it doesn't seem so bad for my purposes right
 * now.  Anyway, somebody might eventually want to do some profiling
 * and improve this.  --league, 24 March 1998
 *)
    
(* Profiling code, temporary?? *)
structure Click =
struct
    local
        val s_unroll = Stats.makeStat "FIX unrolls"
    in
        fun unroll() = Stats.addStat s_unroll 1
    end
end (* Click *)

(** unrolling a fix, tyc -> tyc *)
fun tc_unroll_fix tyc =
    case tc_outX tyc of
        (TC_FIX((n,tc,ts),i)) => let
            fun genfix i = tcc_fix ((n,tc,ts),i)
            val fixes = List.tabulate(n, genfix)
            val mu = tc
            val mu = if null ts then mu
                     else tcc_app (mu,ts)
            val mu = tcc_app (mu, fixes)
            val mu = if n=1 then mu
                     else tcc_proj (mu, i)
        in
            Click.unroll();
            mu
        end
      | _ => bug "unexpected non-FIX in tc_unroll_fix"

(* In order to check equality of two FIXes, we need to be able to
 * unroll them once, and check equality on the unrolled version, with
 * an inductive assumption that they ARE equal.  The following code
 * supports making and checking these inductive assumptions.
 * Furthermore, we need to avoid unrolling any FIX more than once.
 *)
structure TcDict = BinaryMapFn
                       (struct
                           type ord_key = tyc
                           val compare = tc_cmp
                       end)
(* for each tyc in this dictionary, we store a dictionary containing
 * tycs that are assumed equivalent to it.
 *)
type eqclass = unit TcDict.map
type hyp = eqclass TcDict.map

(* the null hypothesis, no assumptions about equality *)
val empty_eqclass : eqclass = TcDict.empty
val null_hyp : hyp = TcDict.empty

(* add assumption t1=t2 to current hypothesis.  returns composite
 * hypothesis.
 *)
fun assume_eq' (hyp, t1, t1eqOpt, t2) = let
    val t1eq  = case t1eqOpt of SOME e => e | NONE => empty_eqclass
    val t1eq' = TcDict.insert (t1eq, t2, ())
    val hyp'  = TcDict.insert (hyp, t1, t1eq')
in
    hyp'
end

fun assume_eq (hyp, t1, t1eqOpt, t2, t2eqOpt) =
    assume_eq' (assume_eq' (hyp, t1, t1eqOpt, t2),
                t2, t2eqOpt, t1)

(* check whether t1=t2 according to the hypothesis *)
val eq_by_hyp : eqclass option * tyc -> bool
    = fn (NONE, t2) => false
       | (SOME eqclass, t2) =>
         isSome (TcDict.find (eqclass, t2))
    
(* have we made any assumptions about `t' already? *)
val visited : eqclass option -> bool 
  = isSome

(* testing if two recursive datatypes are equivalent *)
fun eq_fix (eqop1, hyp) (t1, t2) = 
  (case (tc_outX t1, tc_outX t2) 
    of (TC_FIX((n1,tc1,ts1),i1), TC_FIX((n2,tc2,ts2),i2)) => 
        if not (!Control.FLINT.checkDatatypes) then true 
        else let 
            val t1eqOpt = TcDict.find (hyp, t1)
        in
            (* first check the induction hypothesis.  we only ever
             * make hypotheses about FIX nodes, so this test is okay
             * here.  if assume_eq appears in other cases, this 
             * test should be lifted outside the switch.
             *)
            if eq_by_hyp (t1eqOpt, t2) then true
            (* next try structural eq on the components.  i'm not sure why
             * this part is necessary, but it does seem to be... --league,
             * 23 March 1998
             *)
            else
                (n1 = n2 andalso i1 = i2 andalso
                 eqop1 hyp (tc1, tc2) andalso 
                 eqlist (eqop1 hyp) (ts1, ts2)) orelse
                (* not equal by inspection; we have to unroll it.
                 * we prevent unrolling the same FIX twice by asking
                 * the `visited' function.
                 *)
                if visited t1eqOpt then false 
                else let
                    val t2eqOpt = TcDict.find (hyp, t2)
                in
                    if visited t2eqOpt then false 
                    else eqop1 (assume_eq (hyp, t1, t1eqOpt,
                                           t2, t2eqOpt))
                               (tc_unroll_fix t1, tc_unroll_fix t2)
                end
        end
     | _ => bug "unexpected types in eq_fix")


(* tc_eqv_generator, invariant: t1 and t2 are in the wh-normal form 
 *     eqop1 is the default equality to be used for tycs
 *     eqop2 is used for body of FN, arguments in APP,
 *     eqop3 is used for ABS and BOX.
 *     eqop4 is used for arrow arguments and results
 * Each of these first takes the set of hypotheses.
 *)
fun tc_eqv_gen (eqop1, eqop2, hyp) (t1, t2) = 
    case (tc_outX t1, tc_outX t2) of
        (TC_FIX _, TC_FIX _) => eqop2 (eqop1, hyp) (t1, t2)
      | (TC_FN(ks1, b1), TC_FN(ks2, b2)) =>
        eqlist tk_eqv (ks1, ks2) andalso eqop1 hyp (b1, b2)
      | (TC_APP(a1, b1), TC_APP(a2, b2)) =>
        eqop1 hyp (a1, a2) andalso eqlist (eqop1 hyp) (b1, b2)
      | (TC_SEQ ts1, TC_SEQ ts2) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | (TC_SUM ts1, TC_SUM ts2) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | (TC_TUPLE (_, ts1), TC_TUPLE (_, ts2)) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | (TC_ABS a, TC_ABS b) =>
        eqop1 hyp (a, b)
      | (TC_BOX a, TC_BOX b) =>
        eqop1 hyp (a, b)
      | (TC_TOKEN(k1,t1), TC_TOKEN(k2,t2)) => 
        token_eq(k1,k2) andalso eqop1 hyp (t1,t2)
      | (TC_PROJ(a1, i1), TC_PROJ(a2, i2)) =>
        i1 = i2 andalso eqop1 hyp (a1, a2)
      | (TC_ARROW(r1, a1, b1), TC_ARROW(r2, a2, b2)) => 
        r1 = r2 andalso eqlist (eqop1 hyp) (a1, a2) 
                andalso eqlist (eqop1 hyp) (b1, b2)
      | (TC_PARROW(a1, b1), TC_PARROW(a2, b2)) => 
        eqop1 hyp (a1, a2) andalso eqop1 hyp (b1, b2)
      | (TC_CONT ts1, TC_CONT ts2) =>
        eqlist (eqop1 hyp) (ts1, ts2)
      | _ => false

(** general equality for tycs *)
fun tc_eqv' hyp (x as ref (_, _, AX_REG(true, _, _)),
                 y as ref (_, _, AX_REG(true, _, _))) = tc_eq(x,y)
  | tc_eqv' hyp (x, y) = let
        val t1 = tc_whnm x
        val t2 = tc_whnm y
    in
        if tcp_norm t1 andalso tcp_norm t2 then tc_eq (t1, t2)
        else    
            tc_eqv_gen (tc_eqv', fn _ => tc_eq, hyp) (t1, t2)
    end (* tc_eqv' *)

(* slightly relaxed constraints (???) *)
fun tc_eqv_x' hyp (x, y) =
  let val t1 = tc_whnm x
      val t2 = tc_whnm y
   in (if (tcp_norm t1) andalso (tcp_norm t2) then tc_eq(t1, t2)
       else false) orelse
       (tc_eqv_gen (tc_eqv_x', eq_fix, hyp) (t1, t2))
  end (* function tc_eqv_x *)

in (* tyc equivalence utilities *)

val tc_eqv = tc_eqv' null_hyp
val tc_eqv_x = tc_eqv_x' null_hyp

end (* tyc equivalence utilities *)


(** lt_eqv_generator, invariant: x and y are in the wh-normal form *)
fun lt_eqv_gen (eqop1, eqop2) (x : lty, y) = 
  let (* seq should be called if t1 and t2 are weak-head normal form *)
      fun seq (t1, t2) = 
        (case (lt_outX t1, lt_outX t2)
          of (LT_POLY(ks1, b1), LT_POLY(ks2, b2)) =>
               (eqlist tk_eqv (ks1, ks2)) andalso (eqlist eqop1 (b1, b2))
           | (LT_FCT(as1, bs1), LT_FCT(as2, bs2)) => 
               (eqlist eqop1 (as1, as2)) andalso (eqlist eqop1 (bs1, bs2))
           | (LT_TYC a, LT_TYC b) => eqop2(a, b)
           | (LT_STR s1, LT_STR s2) => eqlist eqop1 (s1, s2)
           | (LT_CONT s1, LT_CONT s2) => eqlist eqop1 (s1, s2)
           | _ => false)
   in seq(x, y)
  end (* function lt_eqv_gen *)

fun lt_eqv(x : lty, y) = 
  let val seq = lt_eqv_gen (lt_eqv, tc_eqv) 
   in if ((ltp_norm x) andalso (ltp_norm y)) then lt_eq(x,y)
      else (let val t1 = lt_whnm x
                val t2 = lt_whnm y
             in if (ltp_norm t1) andalso (ltp_norm t2) then lt_eq(x, y)
                else seq(t1, t2)
            end)
  end (* function lt_eqv *)

fun lt_eqv_x(x : lty, y) = 
  let val seq = lt_eqv_gen (lt_eqv_x, tc_eqv_x) 
   in if ((ltp_norm x) andalso (ltp_norm y)) then 
           (lt_eq(x, y)) orelse (seq(x, y))
      else (let val t1 = lt_whnm x
                val t2 = lt_whnm y
             in if (ltp_norm t1) andalso (ltp_norm t2) then 
                  (lt_eq(t1, t2)) orelse (seq(t1, t2))
                else seq(t1, t2)
            end)
  end (* function lt_eqv *)

(** testing equivalence of fflags and rflags *)
val ff_eqv   : fflag * fflag -> bool = (op =)
val rf_eqv   : rflag * rflag -> bool = (op =)

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
        | SOME (a::_) => d + 1 - (#1(tvDecode a))
  end

fun tcs_depth ([], d) = DI.top
  | tcs_depth (x::r, d) = Int.max(tc_depth(x, d), tcs_depth(r, d))

(* these return the list of free NAMED tyvars *)
fun tc_nvars (tyc:tyc) =
    case getAux (tc_norm tyc) of
        AX_REG (_,_,tvs) => tvs
      | AX_NO => bug "unexpected case in tc_nvars"

fun lt_nvars (lty:lty) = 
    case getAux (lt_norm lty) of
        AX_REG (_,_,tvs) => tvs
      | AX_NO => bug "unexpected case in lt_nvars"


end (* toplevel local *)
end (* abstraction LtyKernel *)
