(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* pickmod.sml *)

signature PICKMOD =
sig
  val pickleEnv : 
        CMStaticEnv.staticEnv * StaticEnv.staticEnv 
                  -> {hash: PersStamps.persstamp,
                      pickle: Word8Vector.vector, 
	              exportLvars: Access.lvar list,
	              exportPid: PersStamps.persstamp option}
  val pickleFLINT:
        CompBasic.flint option ->
            {hash: PersStamps.persstamp, pickle: Word8Vector.vector}

  val pickle2hash: Word8Vector.vector -> PersStamps.persstamp

  val dontPickle : 
        StaticEnv.staticEnv * int
                  -> StaticEnv.staticEnv * PersStamps.persstamp *
	             Access.lvar list * PersStamps.persstamp option

  val debugging : bool ref
  val debuggingSW : bool ref

end (* signature PICKMOD *)

structure PickMod : PICKMOD = 
struct

local structure A  = Access
      structure B  = Bindings
      structure DI = DebIndex
      structure EP = EntPath
      structure ED = EntPath.EvDict
      structure II = InlInfo
      structure IP = InvPath
      structure F  = FLINT
      structure LK = LtyKernel  (* structure LT = LtyDef *)
      (** pickmod must look under the abstract lty representation *)
      structure M  = Modules 
      structure MI = ModuleId
      structure P  = PrimOp
      structure PS = PersStamps
      structure PT = PrimTyc
      structure S  = Symbol
      structure SP = SymPath 
      structure T  = Types 
      structure TU = TypesUtil
      structure V  = VarCon 
      structure LtyKey : ORD_KEY = 
        struct type ord_key = LK.lty * DI.depth
               fun compare((t,d),(t',d')) =
                   case LK.lt_cmp(t,t') of EQUAL => DI.cmp(d,d')
                                         | x => x
        end
      structure LtyDict = BinaryMapFn(LtyKey)
in 

val say = Control.Print.say
val debugging = ref true
fun debugmsg (msg: string) =
  if !debugging then (say msg; say "\n") else ()
fun bug msg = ErrorMsg.impossible ("PickMod: " ^ msg)

fun isGlobalStamp(Stamps.STAMP{scope=Stamps.GLOBAL _,...}) = true
  | isGlobalStamp _ = false

val addPickles = Stats.addStat(Stats.makeStat "Pickle Bytes")

(**************************************************************************
 *                      UTILITY FUNCTIONS                                 *
 **************************************************************************)
datatype key 
  = MIkey of MI.modId
  | LTkey of LK.lty
  | TCkey of LK.tyc
  | TKkey of LK.tkind
  | DTkey of Stamps.stamp
  | MBkey of Stamps.stamp
  | EPkey of EP.entPath

structure Key =
  struct
    type ord_key = key

    fun cmpKey(MIkey a', MIkey b') = MI.cmp(a',b')
      | cmpKey(MIkey _, _) = GREATER
      | cmpKey(_, MIkey _) = LESS 
      | cmpKey(LTkey a', LTkey b') = LK.lt_cmp(a',b')
      | cmpKey(LTkey _, _) = GREATER
      | cmpKey(_, LTkey _) = LESS 
      | cmpKey(TCkey a', TCkey b') = LK.tc_cmp(a',b')
      | cmpKey(TCkey _, _) = GREATER
      | cmpKey(_, TCkey _) = LESS 
      | cmpKey(TKkey a', TKkey b') = LK.tk_cmp(a',b')
      | cmpKey(TKkey _, _) = GREATER
      | cmpKey(_, TKkey _) = LESS 
      | cmpKey(DTkey a', DTkey b') = Stamps.cmp(a',b')
      | cmpKey(DTkey _, _) = GREATER
      | cmpKey(_, DTkey _) = LESS 
      | cmpKey(MBkey a', MBkey b') = Stamps.cmp(a',b')
      | cmpKey(MBkey _, _) = GREATER
      | cmpKey(_, MBkey _) = LESS 
      | cmpKey(EPkey a', EPkey b') = EP.cmpEntPath(a', b')
(*    
      | cmpKey(EPkey _, _) = GREATER
      | cmpKey(_, EPkey _) = LESS 
*)

    val compare = cmpKey

  end (* structure Key *)

structure W = ShareWrite(Key)

val debuggingSW = W.debugging
(*
val cnt = ref 0
fun getcnt () = if !cnt = 10 then (cnt := 0; true) else  (cnt := (!cnt)+1; false)
fun dmsg x = (say x; if getcnt() then say "\n" else ())

val $ = fn (s,z) => (dmsg (s^"  "); W.$(s,z))
*)
val $ = W.$
infix $ 

val nilEncode = "Na" $ []

fun list alpha nil () = nilEncode
  | list alpha [a] () = "1a" $ [alpha a]
  | list alpha [a,b] () = "2a" $ [alpha a, alpha b]
  | list alpha [a,b,c] () = "3a" $ [alpha a, alpha b, alpha c]
  | list alpha [a,b,c,d] () = "4a" $ [alpha a, alpha b, alpha c, alpha d]
  | list alpha [a,b,c,d,e] () =
      "5a" $ [alpha a, alpha b, alpha c, alpha d, alpha e]
  | list alpha (a::b::c::d::e::rest) () =
      "Ma" $ [alpha a, alpha b, alpha c, alpha d, alpha e, list alpha rest]

fun tuple2 (alpha,beta) (x,y) () = "Tb" $ [alpha x, beta y]

fun option alpha (SOME x) () = "Sc" $ [alpha x]
  | option alpha NONE () = "Nc" $ []

fun bool true () = "Td" $ []
  | bool false() = "Fd" $ []

fun persstamp x = W.w8vector (PS.toBytes x)

fun symbol s () = 
  let val code = 
        case S.nameSpace s 
	 of S.VALspace => "Ap"
	  | S.TYCspace => "Bp"
	  | S.SIGspace => "Cp"
	  | S.STRspace => "Dp"
	  | S.FCTspace => "Ep"
	  | S.FIXspace => "Fp"
	  | S.LABspace => "Gp"
	  | S.TYVspace => "Hp"
	  | S.FSIGspace=> "Ip"
   in code $ [W.string(S.name s)]
  end

val int = W.int
val word = W.string o Word.toString
val word32 = W.string o Word32.toString
val int32 = W.string o Int32.toString

(* val apath = int list *)

fun numkind (P.INT i)   () = "Ig" $ [int i]
  | numkind (P.UINT i)  () = "Ug" $ [int i]
  | numkind (P.FLOAT i) () = "Fg" $ [int i]

fun arithop P.+ () = "ah" $ []
  | arithop P.- () = "bh" $ []
  | arithop P.* () = "ch" $ []
  | arithop P./ () = "dh" $ []
  | arithop P.~ () = "eh" $ []
  | arithop P.ABS () = "fh" $ []
  | arithop P.LSHIFT () = "gh" $ []
  | arithop P.RSHIFT () = "hh" $ []
  | arithop P.RSHIFTL () = "ih" $ []
  | arithop P.ANDB () = "jh" $ []
  | arithop P.ORB () = "kh" $ []
  | arithop P.XORB () = "lh" $ []
  | arithop P.NOTB () = "mh" $ []

fun cmpop P.> () = "ai" $ []
  | cmpop P.>= () = "bi" $ []
  | cmpop P.< () = "ci" $ []
  | cmpop P.<= () = "di" $ []
  | cmpop P.LEU () = "ei" $ []
  | cmpop P.LTU () = "fi" $ []
  | cmpop P.GEU () = "gi" $ []
  | cmpop P.GTU () = "hi" $ []
  | cmpop P.EQL () = "ii" $ []
  | cmpop P.NEQ () = "ji" $ []

fun primop (P.ARITH{oper=p,overflow=v,kind=k}) () = 
      "Aj" $ [arithop p, bool v, numkind k]
  | primop (P.CMP{oper=p,kind=k}) () = 
      "Cj" $ [cmpop p, numkind k]

  | primop (P.TEST(from,to)) () = "Gj" $ [int from, int to]
  | primop (P.TESTU(from,to)) () = "Hj" $ [int from, int to]
  | primop (P.TRUNC(from,to)) () = "Ij" $ [int from, int to]
  | primop (P.EXTEND(from,to)) () = "Jj" $ [int from, int to]
  | primop (P.COPY(from,to)) () = "Kj" $ [int from, int to]

  | primop (P.INLLSHIFT k) () = "<j" $ [numkind k]
  | primop (P.INLRSHIFT k) () = ">j" $ [numkind k]
  | primop (P.INLRSHIFTL k) () = "Lj" $ [numkind k] 

  | primop (P.ROUND{floor=f,fromkind=k,tokind=t}) () =
      "Rj" $ [bool f, numkind k, numkind t]
  | primop (P.REAL{fromkind=k,tokind=t}) () =
      "Fj" $ [numkind k, numkind t]
  | primop (P.NUMSUBSCRIPT{kind=k,checked=c,immutable=i}) () =
      "Sj" $ [numkind k, bool c, bool i]
  | primop (P.NUMUPDATE{kind=k,checked=c}) () =
      "Uj" $ [numkind k, bool c]
  | primop (P.INL_MONOARRAY k) () = "Mj" $ [numkind k]
  | primop (P.INL_MONOVECTOR k) () = "Vj" $ [numkind k]

  | primop (P.MKETAG) () = "Xj" $ []
  | primop (P.WRAP) () = "Yj" $ []
  | primop (P.UNWRAP) () = "Zj" $ []

  | primop P.SUBSCRIPT () =  nestprimop "ak"
  | primop P.SUBSCRIPTV () = nestprimop "bk" 
  | primop P.INLSUBSCRIPT () = nestprimop "ck" 
  | primop P.INLSUBSCRIPTV () = nestprimop "dk" 
  | primop P.INLMKARRAY () = nestprimop "~k" 

  | primop P.PTREQL () = nestprimop "ek" 
  | primop P.PTRNEQ () = nestprimop "fk" 
  | primop P.POLYEQL () = nestprimop "gk" 
  | primop P.POLYNEQ () = nestprimop "hk" 
  | primop P.BOXED () = nestprimop "ik" 
  | primop P.UNBOXED () = nestprimop "jk" 
  | primop P.LENGTH () = nestprimop "kk" 
  | primop P.OBJLENGTH () = nestprimop "lk" 
  | primop P.CAST () = nestprimop "mk" 
  | primop P.GETRUNVEC () = nestprimop "nk" 
  | primop P.MARKEXN () = nestprimop "[k" 
  | primop P.GETHDLR () = nestprimop "ok" 
  | primop P.SETHDLR () = nestprimop "pk" 
  | primop P.GETVAR () = nestprimop "qk" 
  | primop P.SETVAR () = nestprimop "rk" 
  | primop P.GETPSEUDO () = nestprimop "sk" 
  | primop P.SETPSEUDO () = nestprimop "tk" 
  | primop P.SETMARK () = nestprimop "uk" 
  | primop P.DISPOSE () = nestprimop "vk" 
  | primop P.MAKEREF () = nestprimop "wk" 
  | primop P.CALLCC () = nestprimop "xk" 
  | primop P.CAPTURE () = nestprimop "yk" 
  | primop P.THROW () = nestprimop "zk" 
  | primop P.DEREF () = nestprimop "1k" 
  | primop P.ASSIGN () = nestprimop "2k" 
    (* NOTE: P.UNBOXEDASSIGN is defined below *)
  | primop P.UPDATE () = nestprimop "3k" 
  | primop P.INLUPDATE () = nestprimop "4k" 
  | primop P.BOXEDUPDATE () = nestprimop "5k" 
  | primop P.UNBOXEDUPDATE () = nestprimop "6k" 

  | primop P.GETTAG () = nestprimop "7k" 
  | primop P.MKSPECIAL () = nestprimop "8k" 
  | primop P.SETSPECIAL () = nestprimop "9k" 
  | primop P.GETSPECIAL () = nestprimop "0k" 
  | primop P.USELVAR () = nestprimop "!k" 
  | primop P.DEFLVAR () = nestprimop "@k" 
  | primop P.INLDIV () = nestprimop "#k" 
  | primop P.INLMOD () = nestprimop "$k"
  | primop P.INLREM () = nestprimop "%k" 
  | primop P.INLMIN () = nestprimop "^k" 
  | primop P.INLMAX () = nestprimop "&k" 
  | primop P.INLABS () = nestprimop "*k" 
  | primop P.INLNOT () = nestprimop "(k" 
  | primop P.INLCOMPOSE () = nestprimop ")k" 
  | primop P.INLBEFORE () = nestprimop ",k" 
  | primop P.INL_ARRAY () = nestprimop ".k" 
  | primop P.INL_VECTOR () = nestprimop "/k" 
  | primop P.ISOLATE () = nestprimop ":k" 
  | primop P.WCAST () = nestprimop ";k" 
  | primop P.NEW_ARRAY0 () = nestprimop "Ak" 
  | primop P.GET_SEQ_DATA () = nestprimop "Bk" 
  | primop P.SUBSCRIPT_REC () = nestprimop "Ck" 
  | primop P.SUBSCRIPT_RAW64 () = nestprimop "Dk" 
  | primop P.UNBOXEDASSIGN () = nestprimop "Ek"

and nestprimop s = "aj" $ [fn () => s $ []]

fun consig (A.CSIG(i,j)) () = "S8" $ [W.int i, W.int j]
  | consig (A.CNIL) () = "N8" $ []

fun mkAccess var = 
  let fun access (A.LVAR i)      () = "Ll" $ [var i]
        | access (A.EXTERN p)    () = "El" $ [persstamp p]
        | access (A.PATH(a,i))   () = "Pl" $ [int i, access a]
        | access (A.NO_ACCESS)   () = "Nl" $ []

      fun conrep (A.UNTAGGED) () = "Um" $ []
	| conrep (A.TAGGED i) () = "Tm" $ [int i]
	| conrep (A.TRANSPARENT) () = "Bm" $ []
	| conrep (A.CONSTANT i) () = "Cm" $ [int i]
	| conrep (A.REF) () = "Rm" $ []
	| conrep (A.EXN a) () = "Vm" $ [access a]
	| conrep (A.LISTCONS) () = "Lm" $ []
	| conrep (A.LISTNIL) () = "Nm" $ []
        | conrep (A.SUSP NONE) () = "Sm" $ []
        | conrep (A.SUSP (SOME (a,b))) () = "Xm" $ [access a, access b]

  in {access=access,conrep=conrep}
 end

fun alphaConverter () = 
  let exception AlphaCvt
      val m: int Intmap.intmap = Intmap.new (32, AlphaCvt)
      val alphacount = ref 0
      fun alphaConvert i = 
        (Intmap.map m i
         handle AlphaCvt => (let val j = !alphacount
			      in alphacount := j+1;
			         Intmap.add m (i,j);
			         j
		             end))
   in alphaConvert
  end

fun mkStamp alphaConvert =
  let fun stamp (Stamps.STAMP{scope=Stamps.LOCAL, count=i}) () =
	    "Le" $ [W.int(alphaConvert i)]
        | stamp (Stamps.STAMP{scope=Stamps.GLOBAL pid, count=i}) () =
	    "Ge" $ [persstamp pid, W.int i]
        | stamp (Stamps.STAMP{scope=Stamps.SPECIAL s, count=i}) () =
	    "Se" $ [W.string s, W.int i]
   in stamp
  end

(** NOTE: the CRC functions really ought to work on Word8Vector.vectors **)
fun pickle2hash pickle =
  PS.fromBytes(Byte.stringToBytes(CRC.toString(
                    CRC.fromString(Byte.bytesToString pickle))))


(**************************************************************************
 *                  PICKLING A LAMBDA EXPRESSIONS                         *
 **************************************************************************)

fun mkPickleLty (stamp,tvar) =
    let fun ltyI x () =
	  (case LK.lt_out x
	    of LK.LT_TYC tc   => "An" $ [tyc tc]
	     | LK.LT_STR l    => "Bn" $ [list lty l]
	     | LK.LT_FCT (ts1,ts2) => "Cn" $ [list lty ts1, list lty ts2]
	     | LK.LT_POLY(ks,ts)  => "Dn" $ [list tkind ks, list lty ts]
             | LK.LT_IND _ => 
                 bug "unexpected LT_IND in mkPickeLty"
             | LK.LT_ENV (lt,ol,nl,te) => 
                 bug "unexpected LT_ENV in mkPickeLty"
             | LK.LT_CONT _ => 
                 bug "unexpected LT_CONT in mkPickeLty")

        and lty x () =
	  if (LK.ltp_norm x) then
            (W.identify (LTkey x) (fn () => ltyI x ()))
          else ltyI x ()
            (* bug "unexpected complex lambda type in mkPickleLty" *)

        and tycI x () = 
	  (case LK.tc_out x
	    of LK.TC_VAR(db,i) => "A6" $ [int(DI.di_toint db), int i]
             | LK.TC_NVAR(n, dp, i) => 
                 "B6" $ [int n, int(DI.dp_toint dp), int i]
	     | LK.TC_PRIM t => "C6" $ [int(PT.pt_toint t)]
             | LK.TC_FN(ks,tc) => "D6" $ [list tkind ks, tyc tc]
             | LK.TC_APP(tc,l) => "E6" $ [tyc tc, list tyc l]
             | LK.TC_SEQ l => "F6" $ [list tyc l]
             | LK.TC_PROJ(tc,i) => "G6" $ [tyc tc, int i]
             | LK.TC_SUM l => "H6" $ [list tyc l]
             | LK.TC_FIX((n,tc,ts),i) => 
                 "I6" $ [int n, tyc tc, list tyc ts, int i]
             | LK.TC_ABS tc => "J6" $ [tyc tc]
             | LK.TC_BOX tc => "K6" $ [tyc tc]
             | LK.TC_TUPLE (_,l) => "L6" $ [list tyc l]
             | LK.TC_ARROW (LK.FF_VAR(b1,b2),ts1,ts2) => 
                 "M6" $ [bool b1, bool b2, list tyc ts1, list tyc ts2]
             | LK.TC_ARROW (LK.FF_FIXED,ts1,ts2) => 
                 "N6" $ [list tyc ts1, list tyc ts2]
             | LK.TC_PARROW _ =>
                 bug "unexpected TC_PARROW in mkPickleLty"
             | LK.TC_TOKEN(k, t) => 
                 "O6" $ [int (LK.token_int k), tyc t]
             | LK.TC_IND _ => 
                 bug "unexpected TC_IND in mkPickleLty"
             | LK.TC_ENV (tc, ol, nl, te) => 
                 bug "unexpected TC_ENV in mkPickleLty"
             | LK.TC_CONT _ => 
                 bug "unexpected TC_CONT in mkPickleLty")

        and tyc x () =
          if (LK.tcp_norm x) then
	    (W.identify (TCkey x) (fn () => tycI x ()))
          else tycI x ()
            (* bug "unexpected complex lambda tyc in mkPickleLty" *)

        and tkind x () =
	  W.identify (TKkey x)
	    (fn ()=>
	     case LK.tk_out x
              of LK.TK_MONO => "A7" $ []
               | LK.TK_BOX => "B7" $ []
               | LK.TK_SEQ ks => "C7" $ [list tkind ks]
               | LK.TK_FUN(ks,k) => "D7" $ [list tkind ks, tkind k])

     in {lty=lty,tyc=tyc,tkind=tkind}
    end
 
fun pickleFLINT fdecOp =
  let val alphaConvert = alphaConverter()
      val stamp = mkStamp alphaConvert
      val lvar = int o alphaConvert
      val tvar = lvar
      val {access,conrep} = mkAccess lvar
      val {lty,tyc,tkind} = mkPickleLty(stamp,tvar)
	
      fun con (F.DATAcon (dc, ts, v), e) () =
	    ".5" $ [dcon (dc, ts), lvar v, lexp e]
        | con (F.INTcon i, e) ()           = ",5" $ [int i, lexp e]
        | con (F.INT32con i32, e) ()       = "=5" $ [int32 i32, lexp e]
        | con (F.WORDcon w, e) ()          = "?5" $ [word w, lexp e]
        | con (F.WORD32con w32, e) ()      = ">5" $ [word32 w32, lexp e]
        | con (F.REALcon s, e) ()          = "<5" $ [W.string s, lexp e]
        | con (F.STRINGcon s, e) ()        = "'5" $ [W.string s, lexp e]
        | con (F.VLENcon i, e) ()          = ";5" $ [int i, lexp e]

      and dcon ((s, cr, t), ts) () = 
            "^5" $ [symbol s, conrep cr, lty t, list tyc ts]

      and dict {default=v, table=tbls} () = 
            "%5" $ [lvar v, list (tuple2 (list tyc, lvar)) tbls]

      and value (F.VAR v) ()               = "a5" $ [lvar v]
	| value (F.INT i) ()               = "b5" $ [int i]
	| value (F.INT32 i32) ()           = "c5" $ [int32 i32]
	| value (F.WORD w) ()              = "d5" $ [word w]
	| value (F.WORD32 w32) ()          = "e5" $ [word32 w32]
	| value (F.REAL s) ()              = "f5" $ [W.string s]
	| value (F.STRING s) ()            = "g5" $ [W.string s]

      and fprim (NONE, p, t, ts) () = 
            "h5" $ [primop p, lty t, list tyc ts]
        | fprim (SOME dt, p, t, ts) () = 
            "i5" $ [dict dt, primop p, lty t, list tyc ts]

      and lexp (F.RET vs) () = "j5" $ [list value vs]
        | lexp (F.LET(vs, e1, e2)) () =  
            "k5" $ [list lvar vs, lexp e1, lexp e2]
	| lexp (F.FIX (fdecs, e)) () = "l5" $ [list fundec fdecs, lexp e]
	| lexp (F.APP (v, vs)) () = "m5" $ [value v, list value vs]
	| lexp (F.TFN(tfdec, e)) () = 
            "n5" $ [tfundec tfdec, lexp e]
	| lexp (F.TAPP(v, ts)) () = 
            "o5" $ [value v, list tyc ts]
	| lexp (F.SWITCH (v, crl, cel, eo)) () =
	    "p5" $ [value v, consig crl, list con cel, option lexp eo]
	| lexp (F.CON (dc, ts, u, v, e)) () =
            "q5" $ [dcon(dc, ts), value u, lvar v, lexp e]
	| lexp (F.RECORD(rk, vl, v, e)) () =
            "r5" $ [rkind rk, list value vl, lvar v, lexp e]
        | lexp (F.SELECT (u, i, v, e)) () = 
            "s5" $ [value u, int i, lvar v, lexp e]
	| lexp (F.RAISE (u, ts)) () = "t5" $ [value u, list lty ts]
	| lexp (F.HANDLE (e, u)) () = "u5" $ [lexp e, value u]
	| lexp (F.BRANCH (p, vs, e1, e2)) () = 
            "v5" $ [fprim p, list value vs, lexp e1, lexp e2]
        | lexp (F.PRIMOP (p, vs, v, e)) () = 
            "w5" $ [fprim p, list value vs, lvar v, lexp e]

      and fundec (fk, v, vts, e) () = 
            "05" $ [fkind fk, lvar v, list (tuple2(lvar, lty)) vts, lexp e]

      and tfundec (v, tvks, e) () = 
            "15" $ [lvar v, list (tuple2(tvar, tkind)) tvks, lexp e]

      and fkind (F.FK_FCT) () = "25" $ []
        | fkind (F.FK_FUN {isrec, fixed=LK.FF_VAR(b1, b2), 
                           known, inline}) () = 
            "35" $ [option (list lty) isrec, bool b1, bool b2, bool known,
                    bool inline]
        | fkind (F.FK_FUN {isrec, fixed=LK.FF_FIXED, known, inline}) () = 
            "45" $ [option (list lty) isrec, bool known, bool inline]

      and rkind (F.RK_VECTOR tc) () = "55" $ [tyc tc]
        | rkind (F.RK_STRUCT) () = "65" $ []
        | rkind (F.RK_TUPLE _) () = "75" $ []

      val prog = fundec
      val pickle = W.pickle (option prog fdecOp)
      val hash = pickle2hash pickle
   in {pickle = pickle, hash = hash}
  end


(**************************************************************************
 *                    PICKLING AN ENVIRONMENT                             *
 **************************************************************************)

fun pickleEnv(context0, e0: B.binding Env.env) =
let val alphaConvert = alphaConverter ()
    val stamp = mkStamp alphaConvert
    val entVar = stamp
    val entPath = list entVar

    fun modId (MI.STRid{rlzn=a,sign=b}) () = "Bf" $ [stamp a, stamp b]
      | modId (MI.SIGid s) () = "Cf" $ [stamp s]
      | modId (MI.FCTid{rlzn=a,sign=b}) () = "Ef" $ [stamp a, modId b]
      | modId (MI.FSIGid{paramsig=a,bodysig=b}) () = "Ff" $ [stamp a, stamp b]
      | modId (MI.TYCid a) () = "Gf" $ [stamp a]
      | modId (MI.EENVid s) () = "Vf" $ [stamp s]

    val lvcount = ref 0
    val lvlist = ref ([]: Access.lvar list)

    fun anotherLvar v =
      let val j = !lvcount
       in lvlist := v :: !lvlist;
	  lvcount := j+1; 
	  j
      end

    val {access,conrep} = mkAccess (int o anotherLvar)
    val {lty,tkind,...} = mkPickleLty(stamp, int o alphaConvert)

    (* SP.path and IP.path are both treated as symbol lists *)
    fun spath (SP.SPATH p) = list symbol p
    fun ipath (IP.IPATH p) = list symbol p

    val label = symbol

    fun eqprop T.YES () = "Yq" $ []
      | eqprop T.NO  () = "Nq" $ []
      | eqprop T.IND () = "Iq" $ []
      | eqprop T.OBJ () = "Oq" $ []
      | eqprop T.DATA() = "Dq" $ []
      | eqprop T.ABS () = "Aq" $ []
      | eqprop T.UNDEF()= "Uq" $ []

    fun datacon (T.DATACON{name=n,const=c,typ=t,rep=r,sign=s,lazyp=l}) () =
	  "Dr" $ [symbol n, bool c, ty t, conrep r, consig s, bool l]

    and tyckind (T.PRIMITIVE pt) () = "Ps" $ [int (PT.pt_toint pt)] 
      | tyckind (T.DATATYPE{index=i, family, stamps=ss, root, freetycs}) () = 
	 "Ds" $ [W.int i, option entVar root,
                 dtypeInfo (ss, family, freetycs)]
	 
      | tyckind (T.ABSTRACT tyc) () = "As" $ [tycon tyc]
      | tyckind (T.FLEXTYC tps) () = 
          (*** tycpath should never be pickled; the only way it can be
               pickled is when pickling the domains of a mutually 
               recursive datatypes; right now the mutually recursive
               datatypes are not assigned accurate domains ... (ZHONG)
               the following code is just a temporary gross hack. 
           ***)
           "Fs" $ [] (* "Ss" $ [tycpath tps] *)
      | tyckind (T.FORMAL) () = "Fs" $ []
      | tyckind (T.TEMP) () = "Ts" $ []

    and dtypeInfo (ss, family, freetycs) () = 
          W.identify (DTkey (Vector.sub(ss,0)))
             (fn () => "Zs" $ [list stamp (Vector.foldr (op ::) nil ss),
                               dtFamily family, list tycon freetycs])

    and dtFamily {mkey=s, members=v, lambdatyc} () = 
          W.identify (MBkey s)
             (fn () => "Us" $ [stamp s, 
                    (list dtmember (Vector.foldr (op ::) nil v))])

    and tycpath _ () = bug "unexpected tycpath during the pickling"

    and dtmember {tycname=n,dcons=d,arity=i,eq=ref e,lazyp=l,sign=sn} () =
          "Tt" $ [symbol n, list nameRepDomain d, int i, eqprop e, bool l,
		  consig sn]

    and nameRepDomain {name=n,rep=r,domain=t} () =
	  "Nu" $ [symbol n, conrep r, option ty t]

    and tycon (T.GENtyc{stamp=s, arity=a, eq=ref e, kind=k, path=p}) () =
	  let val id = MI.TYCid s
	   in W.identify(MIkey id)
		(fn()=> case CMStaticEnv.lookTYC context0 id
			 of SOME _ => "Xv" $ [modId id]
			  | NONE => "Gv" $ [stamp s, int a, eqprop e, 
                                            tyckind k, ipath p])
	  end

      | tycon (T.DEFtyc{stamp=x, tyfun=T.TYFUN{arity=r,body=b},
			strict=s, path=p}) () =
        W.identify(MIkey(MI.TYCid x))
	  (fn()=> "Dw" $ [stamp x, int r, ty b, list bool s, ipath p])

      | tycon (T.PATHtyc{arity=a, entPath=e, path=p}) () =
          "Pw" $ [int a, ipath p, entPath e]
(*
	  W.identify(EPkey e)
	  (fn()=>"Pw" $ [int a, entPath e, ipath p])
*)

      | tycon (T.RECORDtyc l) () = "Rw" $ [list label l]
      | tycon (T.RECtyc i) () = "Cw" $ [int i]
      | tycon (T.FREEtyc i) () = "Hw" $ [int i]
      | tycon T.ERRORtyc () = "Ew" $ []

    and ty (T.VARty(ref(T.INSTANTIATED t))) () = ty t ()
      | ty (T.VARty(ref(T.OPEN _))) () = (* "Vx" $ [tyvar v] *)
  	  bug "uninstatiated VARty in pickmod" 
      | ty (T.CONty (c,[])) () = "Nx" $ [tycon c]
      | ty (T.CONty (c,l)) () = "Cx" $ [tycon c, list ty l]
      | ty (T.IBOUND i) () = "Ix" $ [int i]
      | ty T.WILDCARDty () = "Wx" $ []
      | ty (T.POLYty{sign=s,tyfun=T.TYFUN{arity=r,body=b}}) () = 
	  "Px" $ [list bool s, int r, ty b]
      | ty T.UNDEFty () = "Ux" $ []
      | ty _ () = bug "unexpected types in pickmod-ty"

    fun inl_info (II.INL_PRIM(p, t)) () = "Py" $ [primop p, option ty t]
      | inl_info (II.INL_STR sl) () = "Sy" $ [list inl_info sl]
      | inl_info (II.INL_NO) () = "Ny" $ []
      | inl_info _ () = bug "unexpected inl_info in pickmod"

    fun var (V.VALvar{access=a, info=z, path=p, typ=ref t}) () =
	  "Vz" $ [access a, inl_info z, spath p, ty t]
      | var (V.OVLDvar{name=n, options=ref p, 
                       scheme=T.TYFUN{arity=r,body=b}}) () =
          "Oz" $ [symbol n, list overld p, int r, ty b]
      | var  V.ERRORvar () = "Ez" $ []

    and overld {indicator=i,variant=v} () = "OA" $ [ty i, var v]

    fun fsigId(M.FSIG{kind,
		      paramsig=p as M.SIG{stamp=ps,...},
		      paramvar=q,
		      paramsym=s,
		      bodysig=b as M.SIG{stamp=bs,...}}) =
	  MI.FSIGid{paramsig=ps,bodysig=bs}
      | fsigId _ = bug "unexpected functor signatures in fsigId"


    fun strDef(M.CONSTstrDef s) () = "CE" $ [Structure s]
      | strDef(M.VARstrDef(s,p)) () = "VE" $ [Signature s,entPath p]

    (* 
     * boundeps is not pickled right now, but it really should
     * be pickled in the future.
     *)
    and Signature (M.SIG{name=k, closed=c, fctflag=f, 
                         stamp=m, symbols=l, elements=e, 
                         boundeps=ref b, lambdaty=_, typsharing=t, 
                         strsharing=s}) () =
	 let val id = MI.SIGid m
	  in W.identify (MIkey id)
	      (fn () => 
                case (CMStaticEnv.lookSIG context0 id)
		 of SOME _ => "XE" $ [modId id]
		  | NONE => "SE" $ [option symbol k, bool c, bool f,
                                    stamp m, list symbol l,
				    list (tuple2 (symbol,spec)) e,

(* this is currently turned off ...
 *                              option (list (tuple2 (entPath, tkind))) b, 
 *)
                                option (list (tuple2 (entPath, tkind))) NONE,
			        list (list spath) t, 
				list (list spath) s])
	 end

      | Signature M.ERRORsig () = "EE" $ []

    and fctSig (fs as M.FSIG{kind=k, paramsig=p, paramvar=q, 
			     paramsym=s, bodysig=b}) () =
	  let val id = fsigId fs
	   in W.identify (MIkey id)
		    (fn () => 
                      case CMStaticEnv.lookFSIG context0 id
		       of SOME _ => "XF" $ [modId id]
		        | NONE => "FF" $ [option symbol k, Signature p, 
                                          entVar q, option symbol s, 
                                          Signature b])
	  end
      | fctSig M.ERRORfsig () = "EF" $ []

    and spec (M.TYCspec{spec=t, entVar=v, repl=r, scope=s}) () =
	  "TG" $ [tycon t,entVar v,bool r,int s]
      | spec (M.STRspec{sign=s, slot=d, def=e, entVar=v}) () =
	  "SG" $ [Signature s, int d, option (tuple2 (strDef, int)) e, entVar v]
      | spec (M.FCTspec{sign=s, slot=d, entVar=v}) () =
	  "FG" $ [fctSig s, int d, entVar v]
      | spec (M.VALspec{spec=t, slot=d}) () = "PH" $ [ty t, int d]
      | spec (M.CONspec{spec=c, slot=i}) () = "QH" $ [datacon c, option int i]

    and entity (M.TYCent t) () = "LI" $ [tycEntity t]
      | entity (M.STRent t) () = "SI" $ [strEntity t]
      | entity (M.FCTent t) () = "FI" $ [fctEntity t]
      | entity M.ERRORent () = "EI" $ []

    and fctClosure (M.CLOSURE{param=p,body=s,env=e}) () =
	  "FJ" $ [entVar p, strExp s, entityEnv e]

    and Structure (m as M.STR{sign=s as M.SIG{stamp=g,...},
			      rlzn=r as {stamp=st,...},
			      access=a, info=z}) () = 
	  let val id = MI.STRid{rlzn=st,sign=g}
	   in W.identify (MIkey id)
	      (fn () => 
                case CMStaticEnv.lookSTR context0 id
		  of NONE  => 
		      ((* if isGlobalStamp st andalso isGlobalStamp g
		       then say (String.concat["#pickmod: missed global structure ",
					       MI.idToString id, "\n"])
		       else (); *)
		       "SK" $ [Signature s, strEntity r, 
			       access a, inl_info z])
                   | SOME _ => "XK" $ [modId id, access a])
	  end
      | Structure (M.STRSIG{sign=s,entPath=p}) () = 
          "GK" $ [Signature s, entPath p]
      | Structure M.ERRORstr () = "EK" $ []
      | Structure _ () = bug "unexpected structure in Structure"

    and Functor (M.FCT{sign=s, rlzn=r as {stamp=m,...}, 
                       access=a, info=z}) () = 
	  let val sigid = fsigId s
	      val id = MI.FCTid{rlzn=m, sign=sigid}
	   in W.identify (MIkey id)
	      (fn () =>
                case CMStaticEnv.lookFCT context0 id
		  of NONE =>
		      ((* if isGlobalStamp m andalso 
			  (case sigid
			     of MI.FSIGid{paramsig,bodysig} => 
				 isGlobalStamp paramsig andalso
				 isGlobalStamp bodysig
                              | _ => (say "#pickmod: funny functor sig id\n";
				      false))
		       then say (String.concat["#pickmod: missed global functor ",
					       MI.idToString id, "\n"])
		       else (); *)
		       "FL" $ [fctSig s, fctEntity r, 
			       access a, inl_info z])
                   | SOME _ => "XL" $ [modId id, access a])
	  end
      | Functor M.ERRORfct () = "EL" $ []

    and stampExp (M.CONST s) () = "CM" $ [stamp s]
      | stampExp (M.GETSTAMP s) () = "GM" $ [strExp s]
      | stampExp M.NEW () = "NM" $ []

    and tycExp (M.CONSTtyc t) () = "CN" $ [tycon t]
      | tycExp (M.FORMtyc t) () = "DN" $ [tycon t]
      | tycExp (M.VARtyc s) () = "VN" $ [entPath s]

    and strExp (M.VARstr s) () = "VO" $ [entPath s]
      | strExp (M.CONSTstr s) () = "CO" $ [strEntity s]
      | strExp (M.STRUCTURE{stamp=s,entDec=e}) () = 
	  "SO" $ [stampExp s, entityDec e]
      | strExp (M.APPLY(f,s)) () = "AO" $ [fctExp f, strExp s]
      | strExp (M.LETstr(e,s)) () = "LO" $ [entityDec e, strExp s]
      | strExp (M.ABSstr(s,e)) () = "BO" $ [Signature s, strExp e]
      | strExp (M.CONSTRAINstr{boundvar,raw,coercion}) () =
	  "RO" $ [entVar boundvar, strExp raw, strExp coercion] 
      | strExp (M.FORMstr fs) () = "FO" $ [fctSig fs]

    and fctExp (M.VARfct s) () = "VP" $ [entPath s]
      | fctExp (M.CONSTfct e) () = "CP" $ [fctEntity e]
      | fctExp (M.LAMBDA{param=p,body=b}) () = "LP" $ [entVar p, strExp b]
      | fctExp (M.LAMBDA_TP{param=p, body=b, sign=fs}) () = 
                 "PP" $ [entVar p, strExp b, fctSig fs]
      | fctExp (M.LETfct(e,f)) () = "TP" $ [entityDec e, fctExp f]

    and entityExp (M.TYCexp t) () = "TQ" $ [tycExp t]
      | entityExp (M.STRexp t) () = "SQ" $ [strExp t]
      | entityExp (M.FCTexp t) () = "FQ" $ [fctExp t]
      | entityExp (M.ERRORexp) () = "EQ" $ []
      | entityExp (M.DUMMYexp) () = "DQ" $ []

    and entityDec (M.TYCdec(s,x)) () = "TR" $ [entVar s, tycExp x]
      | entityDec (M.STRdec(s,x,n)) () = "SR" $ [entVar s, strExp x, symbol n]
      | entityDec (M.FCTdec(s,x)) () = "FR" $ [entVar s, fctExp x]
      | entityDec (M.SEQdec e) () = "QR" $ [list entityDec e]
      | entityDec (M.LOCALdec(a,b)) () = "LR" $ [entityDec a, entityDec b]
      | entityDec M.ERRORdec () = "ER" $ []
      | entityDec M.EMPTYdec () = "MR" $ []

    and entityEnv (M.MARKeenv(s,r)) () = 
	  let val id = MI.EENVid s
	   in W.identify(MIkey id)
	      (fn() => case CMStaticEnv.lookEENV context0 id
		        of SOME _ => "X4" $ [modId id]
		         | NONE => "M4" $ [stamp s, entityEnv r])
          end
      | entityEnv (M.BINDeenv(d, r)) () = 
	  "B4" $ [list (tuple2(entVar, entity)) (ED.listItemsi d), entityEnv r]
      | entityEnv M.NILeenv () = "N4" $ []
      | entityEnv M.ERReenv () = "E4" $ []

    and strEntity {stamp=s, entities=e, lambdaty=_, rpath=r} () =
	  "SS" $ [stamp s, entityEnv e, ipath r]

    and fctEntity {stamp=s, closure=c, lambdaty=_,
                   tycpath=_, rpath=r} () =
  	  "FT" $ [stamp s, fctClosure c, ipath r] 
(*      | fctEntity {stamp=s, closure=c, lambdaty=ref t, 
                   tycpath=SOME _, rpath=r} () =
          bug "unexpected fctEntity in pickmod"
*)

    and tycEntity x () = tycon x ()

    fun fixity Fixity.NONfix () = "NW" $ [] 
      | fixity (Fixity.INfix(i,j)) () = "IW" $ [int i, int j]

    fun binding (B.VALbind x) () = "V2" $ [var x]
      | binding (B.CONbind x) () = "C2" $ [datacon x]
      | binding (B.TYCbind x) () = "T2" $ [tycon x]
      | binding (B.SIGbind x) () = "G2" $ [Signature x]
      | binding (B.STRbind x) () = "S2" $ [Structure x]
      | binding (B.FSGbind x) () = "I2" $ [fctSig x]
      | binding (B.FCTbind x) () = "F2" $ [Functor x]
      | binding (B.FIXbind x) () = "X2" $ [fixity x]

    fun env alpha e () = 
	let fun uniq (a::b::rest) = if S.eq(a,b) then uniq(b::rest)
				    else a::uniq(b::rest)
	      | uniq l = l
	    val syms = uniq(ListMergeSort.sort S.symbolGt (Env.symbols e))
	    val pairs = map (fn s => (s, Env.look(e,s))) syms
	 in "E3" $ [list (tuple2(symbol,alpha)) pairs]
	end

    val pickle = W.pickle (env binding e0)

    val hash = pickle2hash pickle

    val exportLvars = rev(!lvlist)
    val exportPid = case exportLvars of [] => NONE
                                      | _ => SOME hash

 in addPickles (Word8Vector.length pickle);
    {hash = hash,
     pickle = pickle,
     exportLvars = exportLvars,
     exportPid = exportPid}
end (* fun pickleEnv *)

fun dontPickle (senv : StaticEnv.staticEnv, count) =
    let val hash =
	    let val toByte = Word8.fromLargeWord o Word32.toLargeWord
		val >> = Word32.>>
		infix >>
		val w = Word32.fromInt count
	    in
	      PersStamps.fromBytes(
	      Word8Vector.fromList
	       [0w0,0w0,0w0,toByte(w >> 0w24),0w0,0w0,0w0,toByte(w >> 0w16),
		0w0,0w0,0w0,toByte(w >> 0w8),0w0,0w0,0w0,toByte(w)])
	    end
	fun uniq (a::b::rest) = if S.eq(a,b) then uniq(b::rest)
				else a::uniq(b::rest)
	  | uniq l = l
        (* next two lines are alternative to using Env.consolidate *)
	val syms = uniq(ListMergeSort.sort S.symbolGt (Env.symbols senv))
	fun newAccess i = A.PATH (A.EXTERN hash, i)
	fun mapbinding(sym,(i,env,lvars)) =
	    case Env.look(senv,sym)
	      of B.VALbind(V.VALvar{access=a, info=z, path=p, typ=ref t}) =>
		  (case a
		    of A.LVAR k => 
			(i+1,
			 Env.bind(sym,B.VALbind(V.VALvar{access=newAccess i,
							 info=z, path=p,
							 typ=ref t}),
				  env),
			 k :: lvars)
		     | _ => (say(A.prAcc a ^ "\n"); bug "dontPickle 1"))
	       | B.STRbind(M.STR{sign=s, rlzn=r, access=a, info=z}) =>
		  (case a
		    of A.LVAR k => 
			(i+1,
			 Env.bind(sym,B.STRbind(M.STR{access=newAccess i,
						      sign=s,rlzn=r,info=z}),
				  env),
			 k :: lvars)
		     | _ => (say(A.prAcc a ^ "\n"); bug "dontPickle 2"))
	       | B.FCTbind(M.FCT{sign=s, rlzn=r, access=a, info=z}) =>
		  (case a
		    of A.LVAR k => 
			(i+1,
			 Env.bind(sym,B.FCTbind(M.FCT{access=newAccess i,
						      sign=s,rlzn=r,info=z}),
				  env),
			 k :: lvars)
		     | _ => (say(A.prAcc a ^ "\n"); bug "dontPickle 3"))
	       | B.CONbind(T.DATACON{name=n,const=c,typ=t,sign=s,lazyp=false,
				     rep as (A.EXN a)}) =>
		   let val newrep = A.EXN (newAccess i)
                    in case a
			 of A.LVAR k =>
			     (i+1,
			      Env.bind(sym,B.CONbind
				       (T.DATACON{rep=newrep, name=n, lazyp=false,
						  const=c, typ=t, sign=s}),
				       env),
			      k :: lvars)
			  | _ => (say(A.prAcc a ^ "\n"); bug "dontPickle 4")
		   end
	       | binding => 
		   (i, Env.bind(sym,binding,env), lvars)
	val (_,newenv,lvars) = foldl mapbinding (0,StaticEnv.empty,nil) syms
	val exportPid = case lvars
			  of [] => NONE
			   | _ => SOME hash
     in (newenv,hash,rev(lvars),exportPid)
    end

end (* toplevel local *)
end (* structure PickMod *)



