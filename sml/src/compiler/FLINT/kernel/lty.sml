(* lty.sml *)
(* COPYRIGHT (c) 1997 YALE FLINT PROJECT *)

structure Lty : LTY =
struct

structure PT = PrimTyc

fun bug s = ErrorMsg.impossible ("Lty:" ^ s)

(***************************************************************************
 *                UTILITY FUNCTIONS FOR HASHCONSING BASICS                 *
 ***************************************************************************)

(** hashconsing implementation basics *)
local (* open SortedList *)
  val MVAL = 10000
  val BVAL = MVAL * 2 (* all indexes i start from 0 *)
in 

(* encoded deBruijn indexes *)
(* Type lambda bindings (TC_FN) bind several variables at a time,
 * i.e. they are n-ary for some n, with each type variable given a kind.
 * A type variable is represented by a pair (d,i), where d is a
 * 1-based deBruijn index designating a lambda binder by its lambda
 * nesting level, counting inside out, and i is a 0-based index
 * into a list of the type variables bound by the corresponding binder.
 * These (d,i) pairs are encoded into a single integer by tvEncode,
 * and the pair can be recovered from its encoding by tvDecode. *)

type enc_tvar = int 
fun tvEncode (d, i) = d * MVAL + i
fun tvDecode x = ((x div MVAL), (x mod MVAL))

(* enc_tvars < BVAL are bound by the innermost TC_FN binder.
 * exitLevel takes a list of enc_tvars and eliminates those bound
 * by the intermost binder, and decrements the d-level of the remainder *)
fun exitLevel (xs: enc_tvar list) : enc_tvar list =
    let fun h ([], x) = rev x
          | h (a::r, x) = if a < BVAL then h(r, x) else h(r, (a-MVAL)::x)
    in h(xs, [])
    end
  
(* definitions of named tyc variables.
   for now, these share the same namespace with lvars. *)
(* [KM ???] Are these used at all? *)
type tvar = LambdaVar.lvar
val mkTvar = LambdaVar.mkLvar

(* for lists of free type variables, debruijn indices are collapsed
   into a single integer using tvEncode/tvDecode, named variables use
   the tvar as an integer.  The deBruijn-indexed list is kept sorted,
   the named variables are in arbitrary order (for now) --league, 2 July 1998
 *)
datatype aux_info
  = AX_REG of bool                      (* normalization flag *)
            * enc_tvar list             (* free debruijn-indexed type vars *)
            * tvar list                 (* free named type vars *)
  | AX_NO                               (* no aux_info available *)

(* these two are originally from SortedList -- which I wanted to get
 * rid off.  -- Matthias  11/2000 *)
fun mergeTvs (l : tvar list, []) = l
  | mergeTvs ([], l) = l
  | mergeTvs (l as (h :: t), l' as (h' :: t')) =
      if h < h' then h :: mergeTvs (t, l')
      else if h = h' then h :: mergeTvs (t, t')
      else h' :: mergeTvs (l, t')

fun fmergeTvs [] = []
  | fmergeTvs (h :: t) = 
    let fun loop ([], a) = a
	  | loop (h :: t, a) = loop (t, mergeTvs (h, a))
    in
	loop (t, h)
    end

(*
val mergeTvs = SortedList.merge
val fmergeTvs = SortedList.foldmerge
*)

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
  | TK_FUN of tkind list * tkind               (* n-ary tycon function *)

withtype tkind = tkindI hash_cell              (* hash-consing-impl of tkind *)

(* an special extensible token key *)
type token = int      

datatype fflag                                 (* calling conventions *)
  = FF_VAR of bool * bool                      (* is it fixed ? *)
  | FF_FIXED                                   (* used after rep. analysis *)

datatype rflag = RF_TMP                        (* tuple kind: a template *)
 (* [dbm] only one rflag value, so doesn't discriminate anything, 
  * therefore probably redundant *)

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
  | TC_FIX of (int * tyc * tyc list) * int     (* (mutually-)recursive tyc 
	                                        * int # of family members  
						* tyc of rec-type generator
						* tyc list is freetycs 
						* int index of dcon in dt 
						*  built in 
                                                * trans/transtypes.sml*)

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
      val NNdec = itow (N*N) - 0w1
      val P = 0w509 (* was 0w1019, a prime < 1024 so that N*N*P < maxint *)

      val tk_table : tkind Weak.weak list Array.array = Array.array(N,nil)
      val tc_table : tyc Weak.weak list Array.array = Array.array(N,nil)
      val lt_table : lty Weak.weak list Array.array = Array.array(N,nil)

      fun vector2list v = Vector.foldr (op ::) [] v

      fun revcat(a::rest,b) = revcat(rest,a::b)
        | revcat(nil,b) = b

      fun combine [x] = itow x
        | combine (a::rest) = 
            andb(itow a +(combine rest)*P, NNdec)
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

      fun tk_hash (TK_MONO) = 0w1
        | tk_hash (TK_BOX) = 0w2
        | tk_hash (TK_SEQ ks) = combine (3::map getnum ks)
        | tk_hash (TK_FUN(ks, k)) = combine (4::getnum k::(map getnum ks))

      fun tc_hash tc = 
        case tc
         of (TC_VAR(d, i)) => combine [1, (DI.di_key d)*10, i]
          | (TC_NVAR v) => combine[15, v]
          | (TC_PRIM pt) => combine [2, PT.pt_toint pt]
          | (TC_FN(ks, t)) => combine (3::(getnum t)::(map getnum ks))
          | (TC_APP(t, ts)) => combine (4::(getnum t)::(map getnum ts))
          | (TC_SEQ ts) => combine (5::(map getnum ts))
          | (TC_PROJ(t, i)) => combine [6, (getnum t), i]
          | (TC_SUM ts) => combine (7::(map getnum ts))
          | (TC_FIX((n, t, ts), i)) => 
              combine (8::n::i::(getnum t)::(map getnum ts))
          | (TC_ABS t) => combine [9, getnum t]
          | (TC_BOX t) => combine [10, getnum t]
          | (TC_TUPLE (_, ts)) => combine (11::(map getnum ts))
          | (TC_ARROW(rw, ts1, ts2)) => 
              let fun h (FF_FIXED) = 10
                    | h (FF_VAR(true,b2)) = if b2 then 20 else 30
                    | h (FF_VAR(false,b2)) = if b2 then 40 else 50
              in combine (12::(h rw)::(map getnum (ts1@ts2)))
              end
          | (TC_PARROW (t1,t2)) => combine [13, getnum t1, getnum t2]
          | (TC_TOKEN (i, tc)) => combine [14, i, getnum tc]
          | (TC_CONT ts) => combine (15::(map getnum ts))
          | (TC_ENV(t,i,j,env)) => 
              combine[16, getnum t, i, j, getnum env]
          | (TC_IND _) => bug "unexpected TC_IND in tc_hash"

      fun lt_hash lt = 
        case lt
         of (LT_TYC t) => combine [1, getnum t]
          | (LT_STR ts) => combine (2::(map getnum ts))
          | (LT_FCT(ts1, ts2)) =>
              combine (3::(map getnum (ts1@ts2)))
          | (LT_POLY(ks, ts)) =>
              combine (4::((map getnum ts)@(map getnum ks)))
          | (LT_CONT ts) => combine (5::(map getnum ts))
          | (LT_ENV(t,i,j,env)) => 
              combine [6, getnum t, i, j, getnum env]
          | (LT_IND _) => bug "unexpected LT_IND in lt_hash"

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
          case tc
           of (TC_VAR(d, i)) => AX_REG(true, [tvEncode(d, i)], [])
            | (TC_NVAR v) => AX_REG(true, [], [v])
            | (TC_PRIM pt) => baseAux
            | (TC_APP(ref(_, TC_FN _, AX_NO), _)) => AX_NO
            | (TC_PROJ(ref(_, TC_SEQ _, AX_NO), _)) => AX_NO
            | (TC_APP(ref(_, TC_FN _, AX_REG(_,vs,nvs)), ts)) => 
                mergeAux(AX_REG(false, vs, nvs), fsmerge ts) (* ? *)
            | (TC_PROJ(ref(_, TC_SEQ _, AX_REG(_,vs,nvs)), _)) => 
                AX_REG(false, vs, nvs) (* ? *)
            | (TC_FN(ks, t)) => exitAux(getAux t)
            | (TC_APP(t, ts)) => fsmerge (t::ts)
            | (TC_SEQ ts) => fsmerge ts
            | (TC_PROJ(t, _)) => getAux t
            | (TC_SUM ts) => fsmerge ts
            | (TC_FIX((_,t,ts), _)) => 
                let val ax = getAux t
                in case ax
                    of AX_REG(_,[],[]) => mergeAux(ax, fsmerge ts)
                     | AX_REG _ => bug "unexpected TC_FIX freevars in tc_aux"
                     | AX_NO => AX_NO
                end
            | (TC_ABS t) => getAux t
            | (TC_BOX t) => getAux t
            | (TC_TUPLE (_, ts)) => fsmerge ts
            | (TC_ARROW(_, ts1, ts2)) => fsmerge (ts1@ts2)
            | (TC_PARROW(t1, t2)) => fsmerge [t1, t2]
            | (TC_TOKEN (k, (ref(_, t, AX_NO)))) => AX_NO
            | (TC_TOKEN (k, (x as ref(_, t, AX_REG(b,vs,nvs))))) => 
                AX_REG((token_whnm k x) andalso b, vs, nvs)
            | (TC_CONT ts) => fsmerge ts
            | (TC_IND _) => bug "unexpected TC_IND in tc_aux"
            | (TC_ENV _) => AX_NO
        
      fun lt_aux (LT_TYC t) = getAux t
        | lt_aux (LT_STR ts) = fsmerge ts
        | lt_aux (LT_FCT(ts1, ts2)) = fsmerge (ts1@ts2)
        | lt_aux (LT_POLY(ks, ts)) = exitAux(fsmerge ts)
        | lt_aux (LT_CONT ts) = fsmerge ts
        | lt_aux (LT_IND _) = bug "unexpected LT_IND in lt_aux"
        | lt_aux (LT_ENV _) = AX_NO

      fun tk_mk (i : int, k: tkindI) = ref (i, k, AX_NO)
      fun tc_mk (i : int, tc : tycI) = ref (i, tc, tc_aux tc)
      fun lt_mk (i : int, lt : ltyI) = ref (i, lt, lt_aux lt)
in 

(** a temporary hack on getting the list of free tyvars *)
(* ignores named vars for now.  --CALeague, 1 Jul 1998 *)
fun tc_vs (ref(_ : int, _ : tycI, AX_NO)) = NONE
  | tc_vs (ref(_ : int, _ : tycI, AX_REG (_,x,_))) = SOME x

fun lt_vs (ref(_ : int, _ : ltyI, AX_NO)) = NONE
  | lt_vs (ref(_ : int, _ : ltyI, AX_REG (_,x,_))) = SOME x

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

(** get the hash key of each lty, only used by reps/coerce.sml; a hack **)
fun lt_key (ref (h : int, _ : ltyI, _ : aux_info)) = h

(** checking if a tyc or an lty is in the normal form *)
fun tcp_norm ((ref(_, _, AX_REG(b,_,_))) : tyc) =  b
  | tcp_norm _ = false

fun ltp_norm ((ref(_, _, AX_REG(b,_,_))) : lty) =  b
  | ltp_norm _ = false

(** accessing free named tyvars *)
fun tc_nvars (tyc:tyc) =
    case getAux tyc
     of AX_REG (_,_,tvs) => tvs
      | AX_NO => bug "unexpected case in tc_nvars"

fun lt_nvars (lty:lty) =
    case getAux lty
     of AX_REG (_,_,tvs) => tvs
      | AX_NO => bug "unexpected case in lt_nvars"

end (* local -- hask consing *)

(***************************************************************************
 *            UTILITY FUNCTIONS ON TYC ENVIRONMENT                         *
 ***************************************************************************)

(* tycEnvs are represented by an encoding as tycs. The abstract representation
 * of tycEnvs would be given by:
 *
 *   datatype teBinder
 *     = Beta of int * tyc list * tkind list
 *     | Lamb of int * tkind list
 *
 *   type tycEnv = teBinder list
 *
 * Invariant: a tycEnv cannot terminate with a Lamb, i.e. the last binder
 *   in a tycEnv must be a Beta. tycEnvs are created when a closure is created
 *   when reducing a beta-redex (rule r1), and they are always initially of
 *   of the form Beta(0,args,ks)::nil.
 *)
             
datatype teBinder
  = Beta of int * tyc list * tkind list
      (* Beta(j,args,ks):
         created when reducing a beta redex (r1);
         j: the embedding level of the original redex -- 0 if the redex was
            created by r1, or the nesting level of the new closure if by r12;
         args: the tycs bound by the n-ary beta reduction, i.e. the arguments;
         ks: the operator domain kinds *)
  | Lamb of int * tkind list
      (* Lamb(j,ks):
         created when pushing a closure (Env) through a lambda (r10);
         j: the nesting level of the closure just before r10 is applied,
            i.e. the nesting level of the abstraction relative to the
            point where the closure was originally created;
         ks: the kinds of the abstraction parameters *)

val teEmpty : tycEnv = tc_injX(TC_SUM[])

(** utility functions for manipulating tycEnvs and teBinders **)

(* encoding teBinders as tycs:
 * Beta(j,args,ks) <=> TC_FN(ks,TC_PROJ(TC_SEQ args, j))
 * Lamb(j,ks) <=> TC_PROJ(TC_FN(ks,TC_SUM[]), j)
 *)
fun teEncodeBinder (Beta(j,args,ks)) : tyc =
      tc_injX(TC_FN(ks,tc_injX(TC_PROJ(tc_injX(TC_SEQ args), j))))
  | teEncodeBinder (Lamb(j,ks)) =
      tc_injX(TC_PROJ(tc_injX(TC_FN(ks,tc_injX(TC_SUM[]))), j))

fun teDecodeBinder (tyc : tyc) : teBinder =
    case tc_outX(tyc)
     of TC_FN(ks,tyc') =>
          (case tc_outX tyc'
             of TC_PROJ(tyc'',j) =>
                  (case tc_outX tyc''
                     of TC_SEQ(args) => Beta(j,args,ks)
                      | _ => bug "teDecodeBinder")
              | _ => bug "teDecodeBinder")
      | TC_PROJ(tyc',j) =>
          (case tc_outX tyc'
             of TC_FN(ks,_) => Lamb(j, ks)
              | _ => bug "teDecodeBinder")
      | _ => bug "teDecodeBinder"

fun teCons (b: teBinder, tenv: tycEnv) : tycEnv =
    tc_injX(TC_PARROW(teEncodeBinder b, tenv))

fun teDest (tenv: tycEnv) : (teBinder * tycEnv) option =
    case tc_outX tenv
     of TC_PARROW(b,tenv) => SOME(teDecodeBinder b, tenv)
      | TC_SUM [] => NONE
      | _ => bug "teDest"

fun teToBinders (tenv: tycEnv) =
    case teDest tenv 
     of NONE => []
      | SOME(binder, tenvRest) => binder::(teToBinders tenvRest)

(* TeUnbound -- raised when first element of a deBruijn index is 
 * out of bounds *)
exception TeUnbound

(* 1-based index lookup: assume i >= 1 *)
fun teLookup(tenv : tycEnv, i: int) : teBinder option =
      (case teDest tenv
        of SOME(binder, tenv') =>
             if i > 1 then teLookup(tenv',i-1)
             else if i = 1 then SOME binder
             else bug "index 0 in tycEnvLookup"
         | NONE => NONE)


(***************************************************************************
 *            UTILITY FUNCTIONS ON TKIND ENVIRONMENT                       *
 ***************************************************************************)
(** tkind environment: maps each tyvar, i.e., its debindex, to its kind *)
type tkindEnv = tkind list list

(** utility functions for manipulating the tkindEnv *)
exception tkUnbound
val initTkEnv : tkindEnv = []

fun tkLookup (kenv, i, j) = 
  let val ks = List.nth(kenv, i-1) handle Subscript => raise tkUnbound
   in List.nth(ks, j) handle Subscript => raise tkUnbound
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
    let fun g (kenv, d, []) = []
	  | g (kenv, d, ftv::ftvs) =
	    let val (d', i') = tvDecode ftv
		val kenv' = List.drop (kenv, d'-d)
		    handle Subscript => raise tkUnbound
		val k = List.nth (hd kenv', i')
		    handle Subscript => raise tkUnbound
	    in
		k :: g (kenv', d', ftvs)
	    end
        fun h ftvs = g (kenv, 1, ftvs)
    in Option.map h (tc_vs tyc)
    end


(** testing the "pointer" equality on normalized tkind, tyc, and lty *)
fun tk_eq (x: tkind, y) = (x = y)
fun tc_eq (x: tyc, y) = (x = y)
fun lt_eq (x: lty, y) = (x = y)


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


(********************************************************************
 *                      KIND-CHECKING ROUTINES                      *
 ********************************************************************)
exception TkTycChk of string
exception LtyAppChk

(* tkSubkind returns true if k1 is a subkind of k2, or if they are 
 * equivalent kinds.  it is NOT commutative.  tksSubkind is the same
 * thing, component-wise on lists of kinds.
 *)
fun tksSubkind (ks1, ks2) =
    ListPair.all tkSubkind (ks1, ks2)   (* component-wise *)
and tkSubkind (k1, k2) = 
    tk_eq (k1, k2) orelse              (* reflexive *)
    case (tk_outX k1, tk_outX k2) of
        (TK_BOX, TK_MONO) => true (* ground kinds (base case) *)
      (* this next case is WRONG, but necessary until the
       * infrastructure is there to give proper boxed kinds to
       * certain tycons (e.g., ref : Omega -> Omega_b)
       *)
      | (TK_MONO, TK_BOX) => true
      | (TK_SEQ ks1, TK_SEQ ks2) =>     
          tksSubkind (ks1, ks2)
      | (TK_FUN (ks1, k1'), TK_FUN (ks2, k2')) => 
          tksSubkind (ks2, ks1) andalso (* contravariant *)
          tkSubkind (k1', k2')
      | _ => false

local 

(* TODO 
 * There must be a better factoring of the dependencies 
 * These functions are in either ltydefs or ltybasic *)
    (** tkind constructors *)
  val tkc_mono   : tkind = tk_injX (TK_MONO)
  val tkc_box    : tkind = tk_injX (TK_BOX)
  val tkc_seq    : tkind list -> tkind = tk_injX o TK_SEQ
  val tkc_fun    : tkind list * tkind -> tkind = tk_injX o TK_FUN

(** utility functions for constructing tkinds *)
fun tkc_arg n = 
  let fun h (n, r) = if n < 1 then r else h(n-1, tkc_mono::r)
   in h(n, [])
  end

val tkc_fn1 = tkc_fun(tkc_arg 1, tkc_mono)
val tkc_fn2 = tkc_fun(tkc_arg 2, tkc_mono)
val tkc_fn3 = tkc_fun(tkc_arg 3, tkc_mono)

fun tkc_int 0 = tkc_mono
  | tkc_int 1 = tkc_fn1
  | tkc_int 2 = tkc_fn2
  | tkc_int 3 = tkc_fn3
  | tkc_int i = tkc_fun(tkc_arg i, tkc_mono)
in
(* is a kind monomorphic? *)
fun tkIsMono k = tkSubkind (k, tkc_mono)

(* assert that k1 is a subkind of k2 *)
fun tkAssertSubkind (k1, k2) =
    if tkSubkind (k1, k2) then ()
    else raise TkTycChk "Subkind assertion failed!"

(* assert that a kind is monomorphic *)
fun tkAssertIsMono k =
    if tkIsMono k then ()
    else raise TkTycChk "Mono assertion failed!"

(* select the ith element from a kind sequence *)
fun tkSel (tk, i) = 
  (case (tk_outX tk)
    of (TK_SEQ ks) => 
       (List.nth(ks, i)
        handle Subscript => raise TkTycChk "Invalid TC_SEQ index")
     | _ => raise TkTycChk "Projecting out of non-tyc sequence")

fun tks_eqv (ks1, ks2) = tk_eq(tkc_seq ks1, tkc_seq ks2)

fun tkApp (tk, tks) = 
  (case (tk_outX tk)
    of TK_FUN(a, b) =>
       if tks_eqv(a, tks) then b
       else raise TkTycChk "Param/Arg Tyc Kind mismatch"
     | _ => raise TkTycChk "Application of non-TK_FUN")

(* check the application of tycs of kinds `tks' to a type function of
 * kind `tk'.
 *)
fun tkApp (tk, tks) = 
  (case (tk_outX tk)
    of TK_FUN(a, b) =>
       if tksSubkind(tks, a) then b
       else raise TkTycChk "Param/Arg Tyc Kind mismatch"
     | _ => raise TkTycChk "Application of non-TK_FUN") 

(* Kind-checking naturally requires traversing type graphs.  to avoid
 * re-traversing bits of the dag, we use a dictionary to memoize the
 * kind of each tyc we process.
 *
 * The problem is that a tyc can have different kinds, depending on
 * the valuations of its free variables.  So this dictionary maps a
 * tyc to an association list that maps the kinds of the free
 * variables in the tyc (represented as a TK_SEQ) to the tyc's kind.
 *)
(* structure TcDict = BinaryMapFn
                     (struct
                        type ord_key = tyc
                        val compare = tc_cmp
		      end) *)
                       
structure Memo :> sig
  type dict 
  val newDict         : unit -> dict
  val recallOrCompute : dict * tkindEnv * tyc * (unit -> tkind) -> tkind
end =
struct
    structure TcDict = RedBlackMapFn
                           (struct
                               type ord_key = tyc
                               val compare = tc_cmp
                           end)

    type dict = (tkind * tkind) list TcDict.map ref
    val newDict : unit -> dict = ref o (fn () => TcDict.empty)

    fun recallOrCompute (dict, kenv, tyc, doit) =
        (* what are the valuations of tyc's free variables
         * in kenv? *)
        (* (might not be available for some tycs) *)
        case tkLookupFreeVars (kenv, tyc) of
            SOME ks_fvs => let
                (* encode those as a kind sequence *)
                val k_fvs = tkc_seq ks_fvs
                (* query the dictionary *)
                val kci = case TcDict.find(!dict, tyc) of
                    SOME kci => kci
                  | NONE => []
                (* look for an equivalent environment *)
                fun sameEnv (k_fvs',_) = tk_eq(k_fvs, k_fvs')
            in
                case List.find sameEnv kci of
                    SOME (_,k) => k     (* HIT! *)
                  | NONE => let
                        (* not in the list.  we will compute
                         * the answer and cache it
                         *)
                        val k = doit()
                        val kci' = (k_fvs, k) :: kci
                    in
                        dict := TcDict.insert(!dict, tyc, kci');
                        k
                    end
            end
          | NONE =>
            (* freevars were not available.  we'll have to
             * recompute and cannot cache the result.
             *)
            doit()

end (* Memo *)

(* return the kind of a given tyc in the given kind environment *)
fun tkTycGen'() = let
    val dict = Memo.newDict()

    fun tkTyc (kenv : tkindEnv) t = let
        (* default recursive invocation *)    
        val g = tkTyc kenv
        (* how to compute the kind of a tyc *)
	fun mkI tycI =
            case tycI of
                TC_VAR (i, j) =>
                tkLookup (kenv, i, j)
              | TC_NVAR _ => 
                bug "TC_NVAR not supported yet in tkTyc"
              | TC_PRIM pt =>
                tkc_int (PrimTyc.pt_arity pt)
              | TC_FN(ks, tc) =>
                tkc_fun(ks, tkTyc (tkInsert (kenv,ks)) tc)
              | TC_APP (tc, tcs) =>
                tkApp (g tc, map g tcs)
              | TC_SEQ tcs =>
                tkc_seq (map g tcs)
              | TC_PROJ(tc, i) =>
                tkSel(g tc, i)
              | TC_SUM tcs =>
                (List.app (tkAssertIsMono o g) tcs;
                 tkc_mono)
              | TC_FIX ((n, tc, ts), i) =>
                let (* Kind check generator tyc *)
		    val k = g tc
		    (* Kind check freetycs *)
                    val nk =
                        case ts
                          of [] => k 
                           | _ => tkApp(k, map g ts)
                in
                    case (tk_outX nk) of
                        TK_FUN(a, b) => 
                        let val arg =
                                case a
                                  of [x] => x
                                   | _ => tkc_seq a
                        in
			    (* Kind check recursive tyc app ??*)
                            if tkSubkind(arg, b) then (* order? *)
                                (if n = 1 then b else tkSel(arg, i))
                            else raise TkTycChk "Recursive app mismatch"
                        end
                      | _ => raise TkTycChk "FIX with no generator"
                end
              | TC_ABS tc =>
                (tkAssertIsMono (g tc);
                 tkc_mono)
              | TC_BOX tc =>
                (tkAssertIsMono (g tc);
                 tkc_mono)
              | TC_TUPLE (_,tcs) =>
                (List.app (tkAssertIsMono o g) tcs;
                 tkc_mono)
              | TC_ARROW (_, ts1, ts2) =>
                (List.app (tkAssertIsMono o g) ts1;
                 List.app (tkAssertIsMono o g) ts2;
                 tkc_mono)
              | TC_TOKEN(_, tc) =>
                (tkAssertIsMono (g tc);
                 tkc_mono)
              | TC_PARROW _ => bug "unexpected TC_PARROW in tkTyc"
           (* | TC_ENV _ => bug "unexpected TC_ENV in tkTyc" *)
	      | TC_ENV(body, 0, j, teEmpty) => 
		  (tkTyc (List.drop(kenv,j)) body 
		   handle Subscript => 
			  bug "[Env]: dropping too many frames")
	      | TC_ENV(body, i, j, env) =>
		  (let val kenv' = 
			   List.drop(kenv, j)
			   handle Subscript => 
				  bug "[Env]: dropping too many frames"
		       fun bindToKinds(Lamb(_,ks)) = ks
			 | bindToKinds(Beta(_,_,ks)) = ks
		       fun addBindToKEnv(b,ke) = 
			   bindToKinds b :: ke
		       val bodyKenv = 
			   foldr addBindToKEnv kenv' (teToBinders env)
		   in chkKindEnv(env,j,kenv);
		      tkTyc bodyKenv body
		   end) 
            (*  | TC_IND _ =>  bug "unexpected TC_IND in tkTyc" *)
	      | TC_IND(newtyc, oldtycI) =>
		  let val newtycknd = g newtyc
		  in   
		      if tk_eq(newtycknd, mkI oldtycI) 
		      then newtycknd
		      else bug "tkTyc[IND]: new tyc and old tycI kind mismatch"
		  end 
              | TC_CONT _ => bug "unexpected TC_CONT in tkTyc"
        fun mk () =
	    mkI (tc_outX t)
    in
        Memo.recallOrCompute (dict, kenv, t, mk)
    end
    and chkKindEnv(env : tycEnv,j,kenv : tkindEnv) : unit =
	let 
	    fun chkBinder(Lamb _) = ()
	      | chkBinder(Beta(j',args,ks)) = 
		let 
		    val kenv' = List.drop(kenv, j-j')
		    val argks = map (fn t => tkTyc kenv' t) args
		in if tksSubkind(ks, argks)
		   then ()
		   else bug "chkKindEnv: Beta binder kinds mismatch"
		end
		handle Subscript => 
		       bug "tkTyc[Env]: dropping too many frames"
	in app chkBinder (teToBinders env)
	end (* function chkKindEnv *)
in
    (tkTyc, chkKindEnv)
end (* function tkTycGen' *)

fun tkTycGen() = 
    case tkTycGen'() 
     of (tkTyc, _) => tkTyc
      
 
(* assert that the kind of `tc' is a subkind of `k' in `kenv' *)
fun tkChkGen() =
    let val tkTyc = tkTycGen()
        fun tkChk kenv (k, tc) =
            tkAssertSubkind (tkTyc kenv tc, k)
    in tkChk
    end (* function tkChkGen *)


fun ltyChk (lty : lty) =
    let val (tkChk, chkKindEnv) = tkTycGen'()
	fun ltyIChk (kenv : tkindEnv) (ltyI : ltyI) =
	    (case ltyI 
	      of LT_TYC(tyc) => 
		   (tkAssertIsMono (tkChk kenv tyc); tkc_mono)
	       | LT_STR(ltys) => tkc_seq(map (ltyChk' kenv) ltys)
	       | LT_FCT(paramLtys, rngLtys) => 
		   let val paramks = map (ltyChk' kenv) paramLtys
		       val tenv' = paramks :: kenv
		   in 
		       tkc_fun(paramks,
			      tkc_seq(map (ltyChk' tenv') rngLtys))
		   end
	       | LT_POLY(ks, ltys) => 
		   tkc_seq(map (ltyChk' (ks::kenv)) ltys)
		   (* ??? *)
	       | LT_CONT(ltys) => 
		   tkc_seq(map (ltyChk' kenv) ltys)
	       | LT_IND(newLty, oldLtyI) =>
		   let val newLtyKnd = (ltyChk' kenv) newLty
		   in if tk_eq(newLtyKnd, ltyIChk kenv oldLtyI)
		      then newLtyKnd
		      else bug "ltyChk[IND]: kind mismatch"
		   end
	       | LT_ENV(body, i, j, env) =>
		   (* Should be the same as checking TC_ENV and 
		    * therefore the two cases should probably just
		    * call the same helper function *)
		   (let val kenv' = 
			    List.drop(kenv, j)
			    handle Subscript => 
				   bug "[Env]: dropping too many frames"
			fun bindToKinds(Lamb(_,ks)) = ks
			  | bindToKinds(Beta(_,_,ks)) = ks
			fun addBindToKEnv(b,ke) = 
			    bindToKinds b :: ke
			val bodyKenv = 
			    foldr addBindToKEnv kenv' (teToBinders env)
		    in chkKindEnv(env,j,kenv);
		       ltyChk' bodyKenv body
		    end))
	and ltyChk' kenv lty = ltyIChk kenv (lt_outX lty)
    in ltyChk' [] lty
    end (* function ltyChk *)	   
end (* local *)
		   
end (* structure Lty *)
