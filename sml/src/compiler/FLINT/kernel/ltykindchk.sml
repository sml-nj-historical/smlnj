(* ltykindchk.sml *)

(* Kind checker *)

signature LTYKINDCHK =
sig

  exception KindChk of string

  (* assert that k1 is a subkind of k2 *)
  val tkAssertSubkind : Lty.tkind * Lty.tkind -> unit

  (* assert that a kind is monomorphic *)
  val tkAssertIsMono : Lty.tkind -> unit

  (* select the ith element (0 based) from a kind sequence *)
  val tkSel : Lty.tkind * int -> Lty.tkind

  val tks_eqv : Lty.tkind list * Lty.tkind list -> bool

  (* tkApp: tkind * tkind list
   * tkApp(tk,tks): check the validity of an application of a
   * type function of kind `tk' to a list of arguments of kinds `tks'.
   * Returns the result kind if valid, raises KindChk otherwise.
   *)
  val tkApp : Lty.tkind * Lty.tkind list -> Lty.tkind

  val tcKindCheckGen :   unit -> (Lty.tkindEnv -> Lty.tyc -> Lty.tkind)
  val tcKindVerifyGen :  unit -> (Lty.tkindEnv -> (Lty.tkind * Lty.tyc) -> unit)
  val ltKindCheckGen :   unit -> (Lty.tkindEnv -> Lty.lty -> Lty.tkind)
  val tcteKindCheckGen : unit -> (Lty.tkindEnv -> Lty.tyc -> Lty.tkind) *
                                 (Lty.tycEnv * int * Lty.tkindEnv -> unit)

end (* signature LTYKINDCHK *)

structure LtyKindChk : LTYKINDCHK =
struct

structure PP = PrettyPrintNew
structure PU = PPUtilNew
open Lty

fun bug s = ErrorMsg.impossible ("Lty:" ^ s)

(********************************************************************
 *                      KIND-CHECKING ROUTINES                      *
 ********************************************************************)
exception KindChk of string

(* assert that k1 is a subkind of k2 *)
fun tkAssertSubkind (k1, k2) =
    if tkSubkind (k1, k2) then ()
    else raise KindChk "Subkind assertion failed!"

(* assert that a kind is monomorphic *)
fun tkAssertIsMono k =
    if tkIsMono k then ()
    else raise KindChk "Mono assertion failed!"

(* select the ith element (0 based) from a kind sequence *)
fun tkSel (tk, i) = 
  (case (tk_outX tk)
    of (TK_SEQ ks) => 
       (List.nth(ks, i)
        handle Subscript => raise KindChk "Invalid TC_SEQ index")
     | _ => raise KindChk "Projecting out of non-tyc sequence")

fun tks_eqv (ks1, ks2) = tk_eq(tkc_seq ks1, tkc_seq ks2)

(* tkApp: tkind * tkind list
 * check the application of a type function of
 * kind `tk' to a list of arguments of kinds `tks'
 *)
fun tkApp (tk, tks) = 
  (case (tk_outX tk)
    of TK_FUN(a, b) =>
         if tksSubkind(tks, a) then b
         else raise KindChk "Param/Arg Tyc Kind mismatch"
     | _ => raise KindChk "Application of non-TK_FUN") 


(* Kind checking **************************************************)

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
                       
(* strip any unused type variables out of a kenv, given a list of
 * [encoded] free type variables.  the result is a "parallel list" of
 * the kinds of those free type variables in the environment.
 * This is meant to use the same representation of a kind environment
 * as in ltybasic.
 * --CALeague
 *)
fun tkLookupFreeVars (kenv, tyc) : tkind list option =
    (* invariant for g: kenv starts with the d(th) frame of the original
     * kenv passed to tkLookupFreeVars *)
    let fun g (kenv, d, []) = []
	  | g (kenv, d, ftv::ftvs) =
	    let val (d', k') = tvDecode ftv
		val kenv' = List.drop (kenv, d'-d)
		            handle Subscript =>
                              (print "### tkLookupFreeVars:1\n";
                               raise tkUnbound)
                (* kenv' should start with the d'(th) frame *)
		val k = case kenv'
                          of nil => (print "### tkLookupFreeVars:2\n";
                                     raise tkUnbound)
                           | ks :: _ =>  (* ks is d'(th) frame *)
                             (List.nth (ks, k')
		              handle Subscript =>
                                     (print "### tkLookupFreeVars:3\n";
                                      PP.with_default_pp
                                        (fn ppstrm =>
                                            (PP.string ppstrm "tyc: ";
                                             PP.newline ppstrm;
                                             PPLty.ppTyc 20 ppstrm tyc;
                                             PP.newline ppstrm;
                                             PP.string ppstrm "length ks: ";
                                             PP.string ppstrm
                                               (Int.toString(length ks));
                                             PP.newline ppstrm;
                                             PP.string ppstrm
                                               ("k': "^Int.toString k');
                                             PP.newline ppstrm));
                                      raise tkUnbound))
	    in
		k :: g (kenv', d', ftvs)
	    end
        fun h ftvs = g (kenv, 1, ftvs)
    in Option.map h (tc_vs tyc)
       (* assumes that tc_vs returns free variable codes sorted in
        * ascending numerical order, which means lexicographical order
        * on the decoded pairs *)
    end

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
        case tkLookupFreeVars (kenv, tyc)
          of SOME ks_fvs =>
             let
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
fun tcteKindCheckGen() = let
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
                              (* "sequencize" the domain to make it comparable
                               * to b *)
                        in
			    (* Kind check recursive tyc app ??*)
                            (* [KM ???] seems bogus if arg is a proper subkind,
                             * but probably ok if tkSubkind is really equivalence *)
                            if tkSubkind(arg, b) then (* order? *)
                                (if n = 1 then b else tkSel(arg, i))
                            else raise KindChk "Recursive app mismatch"
                        end
                      | _ => raise KindChk "FIX with no generator"
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
        handle tkUnbound => raise KindChk "tkUnbound"
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
end (* function tcteKindCheckGen *)

fun tcKindCheckGen() = 
    case tcteKindCheckGen() 
     of (tcKindChk, _) => tcKindChk
      
 
(* assert that the kind of `tc' is a subkind of `k' in `kenv' *)
fun tcKindVerifyGen() =
    let val tkTyc = tcKindCheckGen()
        fun tkChk kenv (k, tc) =
            tkAssertSubkind (tkTyc kenv tc, k)
    in tkChk
    end (* function tkChkGen *)

(* ltKindCheckGen : unit -> tkindEnv -> lty -> tkind *)
fun ltKindCheckGen () = 
let val (tkChk, chkKindEnv) = tcteKindCheckGen()
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
 in ltyChk'
end (* function ltKindCheckGen *)	   

end (* structure LtyKindChk *)