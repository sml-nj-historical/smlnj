(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* unpickmod.sml *)

signature UNPICKMOD =
sig

  val unpickleEnv:
	SCStaticEnv.staticEnv *
	{hash: PersStamps.persstamp, pickle: Word8Vector.vector}
	-> StaticEnv.staticEnv

  val unpickleFLINT:
        {hash: PersStamps.persstamp, pickle: Word8Vector.vector}
	-> CompBasic.flint option

end (* signature UNPICKMOD *)

structure UnpickMod : UNPICKMOD =
struct

local structure A  = Access
      structure B  = Bindings
      structure DI = DebIndex
      structure EP = EntPath
      structure ED = EntPath.EvDict
      structure II = InlInfo
      structure IP = InvPath
      structure F  = FLINT
      structure LV = LambdaVar
      structure LT = LtyDef  (* structure LK = LtyKernel *)
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
in

datatype universal
  = UFunctor of M.Functor
  | USignature of M.Signature
  | UStructure of M.Structure
  | UfctSig of M.fctSig
  | Uaccess of A.access
  | UaccessList of A.access list
  | Uarithop of P.arithop
  | Ubind of S.symbol * B.binding
  | UbindList of (S.symbol * B.binding) list
  | Ubinding of B.binding
  | Ubool of bool
  | UboolList of bool list
  | UboundepsElem of EP.entPath * LT.tkind
  | UboundepsList of (EP.entPath * LT.tkind) list
  | UboundepsOption of (EP.entPath * LT.tkind) list option
  | Ucmpop of P.cmpop
  | Uconrep of A.conrep
  | Uconsig of A.consig
  | Udatacon of V.datacon
  | Uelement of S.symbol * M.spec
  | Uelements of M.elements
  | Uentity of M.entity
  | UentityDec of M.entityDec
  | UentityDecList of M.entityDec list
  | UentityEnv of M.entityEnv
  | UentVarOption of EP.entVar option
  | UentVElist of (EP.entVar * M.entity) list
  | UentVETuple of EP.entVar * M.entity
  | UentityExp of M.entityExp
  | UentPath of EP.entPath
  | Uenv of B.binding Env.env
  | Ueqprop of T.eqprop
  | UfctClosure of M.fctClosure
  | UfctEntity of M.fctEntity
  | UfctExp of M.fctExp
  | Ufixity of Fixity.fixity
  | Uinl_info of II.inl_info
  | Uinl_infoList of II.inl_info list
  | Ulty of LT.lty
  | UltyList of LT.lty list
  | UltyListOption of LT.lty list option
  | UldTuple of LT.lty * DI.depth
  | UldOption of (LT.lty * DI.depth) option
  | UldOptionList of (LT.lty * DI.depth) option list
  | UintltyList of (int * LT.lty) list
  | UintltyTuple of (int * LT.lty)
  | Utyc of LT.tyc
  | UtycList of LT.tyc list
  | Utkind of LT.tkind
  | UtkindList of LT.tkind list
  | UtkindtycTuple of LT.tkind * LT.tyc
  | UtkindtycList of (LT.tkind * LT.tyc) list
  | UmodId of MI.modId
  | UnameRepDomain of {name:S.symbol, rep:A.conrep, domain:T.ty option}
  | UnameRepDomainList of {name:S.symbol, rep:A.conrep,
                           domain:T.ty option} list
  | UdtFamily of T.dtypeFamily
  | Udtmember of T.dtmember
  | UdtmemberList of T.dtmember list
  | UdtypeInfo of Stamps.stamp Vector.vector * T.dtypeFamily * T.tycon list

  | Unumkind of P.numkind
  | Uoverld of {indicator: T.ty, variant: V.var}
  | UoverldList of {indicator: T.ty, variant: V.var} list
  | Uprimop of P.primop
  | UstrDef of M.strDef
  | UstrDefIntTuple of M.strDef * int
  | UstrDefIntOption of (M.strDef * int) option
  | UspathList of SP.path list
  | UspathListList of SP.path list list
  | Uspec of M.spec
  | Ustamp of Stamps.stamp
  | UstampExp of M.stampExp
  | UstrEntity of M.strEntity
  | UstrExp of M.strExp
  | Usymbol of S.symbol
  | UsymbolOption of S.symbol option
  | UsymbolList of S.symbol list
  | Uty of T.ty
  | UtyList of T.ty list
  | UtyOption of T.ty option
  | UtycExp of M.tycExp
  | Utyckind of T.tyckind
  | Utycon of T.tycon
  | UtyconList of T.tycon list
  | Uvar of V.var
  | Ulexp of F.lexp
  | UlexpOption of F.lexp option
  | Ufundec of F.fundec
  | UfundecList of F.fundec list
  | UfundecOption of F.fundec option
  | Utfundec of F.tfundec
  | Ufkind of F.fkind
  | Urkind of F.rkind
  | UintOption of int option
  | UlexpList of F.lexp list
  | Ufprim of F.primop
  | Udict of F.dict
  | Ugenop of F.dict * F.primop
  | Uvalue of F.value
  | UvalueList of F.value list
  | Ucon of F.con * F.lexp
  | Udcon of F.dcon * LT.tyc list
  | UconList of (F.con * F.lexp) list
  | Ulvar of LV.lvar
  | UlvarList of LV.lvar list
  | UtycsLvarPair of LT.tyc list * LV.lvar
  | UtycsLvarPairList of (LT.tyc list * LV.lvar) list
  | UlvarLtyPair of LV.lvar * LT.lty
  | UlvarLtyPairList of (LV.lvar * LT.lty) list
  | UtvarTkPair of LV.lvar * LT.tkind
  | UtvarTkPairList of (LV.lvar * LT.tkind) list

(**************************************************************************
 *                      UTILITY FUNCTIONS                                 *
 **************************************************************************)

structure R = ShareRead(type universal = universal)

val ? = R.?
val % = R.%

fun bool #"T" = %Ubool true
  | bool #"F" = %Ubool false
  | bool _ = raise Fail "    | bool"

fun list (alpha,alphaproj,alphalistproj,alphalistinj) =
  let fun f #"N" = %alphalistinj nil
        | f #"1" = alpha(fn a => %alphalistinj[alphaproj a])
        | f #"2" = alpha(fn a => alpha(fn b =>
                     %alphalistinj[alphaproj a, alphaproj b]))
        | f #"3" = alpha(fn a => alpha(fn b => alpha(fn c =>
                     %alphalistinj[alphaproj a, alphaproj b, alphaproj c])))
        | f #"4" = alpha(fn a => alpha(fn b => alpha(fn c => alpha(fn d=>
                     %alphalistinj[alphaproj a, alphaproj b, alphaproj c,
                     alphaproj d]))))
        | f #"5" = alpha(fn a => alpha(fn b => alpha(fn c => 
                     alpha(fn d=> alpha(fn e =>
                     %alphalistinj[alphaproj a, alphaproj b, alphaproj c,
                     alphaproj d, alphaproj e])))))
        | f #"M" = alpha(fn a => alpha(fn b => alpha(fn c => 
                     alpha(fn d=> alpha(fn e => ?f(fn r =>
                     %alphalistinj(alphaproj a :: alphaproj b :: alphaproj c ::
                     alphaproj d :: alphaproj e ::
                     alphalistproj r)))))))
	| f _ = raise Fail "    | list"

  in f
 end

val boolList = list(?bool, fn Ubool t => t, fn UboolList t => t, UboolList)

fun lvar #"x" = R.int (%Ulvar)
  | lvar _ = raise Fail "    | lvar"

val lvarList = list (?lvar, fn Ulvar v => v, fn UlvarList l => l, UlvarList)

fun numkind #"I" = R.int(fn i => %Unumkind(P.INT i))
  | numkind #"U" = R.int(fn i => %Unumkind(P.UINT i))
  | numkind #"F" = R.int(fn i => %Unumkind(P.FLOAT i))
  | numkind _ = raise Fail "    | numkind"

fun arithop #"a" = %Uarithop P.+
  | arithop #"b" = %Uarithop P.-
  | arithop #"c" = %Uarithop P.*
  | arithop #"d" = %Uarithop P./
  | arithop #"e" = %Uarithop P.~
  | arithop #"f" = %Uarithop P.ABS
  | arithop #"g" = %Uarithop P.LSHIFT
  | arithop #"h" = %Uarithop P.RSHIFT
  | arithop #"i" = %Uarithop P.RSHIFTL
  | arithop #"j" = %Uarithop P.ANDB
  | arithop #"k" = %Uarithop P.ORB
  | arithop #"l" = %Uarithop P.XORB
  | arithop #"m" = %Uarithop P.NOTB
  | arithop _ = raise Fail "    | arithop"

fun cmpop #"a" = %Ucmpop P.>
  | cmpop #"b" = %Ucmpop P.>=
  | cmpop #"c" = %Ucmpop P.<
  | cmpop #"d" = %Ucmpop P.<=
  | cmpop #"e" = %Ucmpop P.LEU
  | cmpop #"f" = %Ucmpop P.LTU
  | cmpop #"g" = %Ucmpop P.GEU
  | cmpop #"h" = %Ucmpop P.GTU
  | cmpop #"i" = %Ucmpop P.EQL
  | cmpop #"j" = %Ucmpop P.NEQ
  | cmpop _ = raise Fail "    | cmpop"

fun primop #"A" = ?arithop(fn Uarithop p => 
         ?bool(fn Ubool v => ?numkind(fn Unumkind k =>
         %Uprimop(P.ARITH{oper=p,overflow=v,kind=k}))))
  | primop #"<"  = ?numkind(fn Unumkind k => %Uprimop(P.INLLSHIFT k))
  | primop #">"  = ?numkind(fn Unumkind k => %Uprimop(P.INLRSHIFT k))
  | primop #"L"  = ?numkind(fn Unumkind k => %Uprimop(P.INLRSHIFTL k))
  | primop #"C" = ?cmpop(fn Ucmpop p =>
         ?numkind(fn Unumkind k =>
         %Uprimop(P.CMP{oper=p,kind=k})))

  | primop #"G" = 
         R.int (fn from => R.int (fn to => %Uprimop(P.TEST(from,to))))
  | primop #"H" = 
         R.int (fn from => R.int (fn to => %Uprimop(P.TESTU(from,to))))
  | primop #"I" = 
	 R.int (fn from => R.int (fn to => %Uprimop(P.TRUNC(from,to))))
  | primop #"J" = 
	 R.int (fn from => R.int (fn to => %Uprimop(P.EXTEND(from,to))))
  | primop #"K" =
	 R.int (fn from => R.int (fn to => %Uprimop(P.COPY(from,to))))

  | primop #"R" = ?bool(fn Ubool f =>
         ?numkind(fn Unumkind k =>
         ?numkind(fn Unumkind t =>
         %Uprimop(P.ROUND{floor=f,fromkind=k,tokind=t}))))
  | primop #"F" = ?numkind(fn Unumkind k =>
         ?numkind(fn Unumkind t =>
         %Uprimop(P.REAL{fromkind=k,tokind=t})))
  | primop #"S" = ?numkind(fn Unumkind k =>
         ?bool(fn Ubool c =>
         ?bool(fn Ubool i =>
         %Uprimop(P.NUMSUBSCRIPT{kind=k,checked=c,immutable=i}))))
  | primop #"U" = ?numkind(fn Unumkind k =>
         ?bool(fn Ubool c =>
         %Uprimop(P.NUMUPDATE{kind=k,checked=c})))
  | primop #"M" = ?numkind(fn Unumkind k =>
         %Uprimop(P.INL_MONOARRAY k))
  | primop #"V" = ?numkind(fn Unumkind k =>
         %Uprimop(P.INL_MONOVECTOR k))

  | primop #"X" = %Uprimop(P.MKETAG)
  | primop #"Y" = %Uprimop(P.WRAP)
  | primop #"Z" = %Uprimop(P.UNWRAP)

  | primop x = %Uprimop(
       case x
        of #"a" => P.SUBSCRIPT
         | #"b" => P.SUBSCRIPTV
         | #"c" => P.INLSUBSCRIPT
         | #"d" => P.INLSUBSCRIPTV
         | #"~" => P.INLMKARRAY
         | #"e" => P.PTREQL
         | #"f" => P.PTRNEQ
         | #"g" => P.POLYEQL
         | #"h" => P.POLYNEQ
         | #"i" => P.BOXED
         | #"j" => P.UNBOXED
         | #"k" => P.LENGTH
         | #"l" => P.OBJLENGTH
         | #"m" => P.CAST
         | #"n" => P.GETRUNVEC
	 | #"[" => P.MARKEXN
         | #"o" => P.GETHDLR
         | #"p" => P.SETHDLR
         | #"q" => P.GETVAR
         | #"r" => P.SETVAR
         | #"s" => P.GETPSEUDO
         | #"t" => P.SETPSEUDO
         | #"u" => P.SETMARK
         | #"v" => P.DISPOSE
         | #"w" => P.MAKEREF
         | #"x" => P.CALLCC
         | #"y" => P.CAPTURE
         | #"z" => P.THROW
         | #"1" => P.DEREF
         | #"2" => P.ASSIGN
         | #"3" => P.UPDATE
         | #"4" => P.INLUPDATE
         | #"5" => P.BOXEDUPDATE
         | #"6" => P.UNBOXEDUPDATE
         | #"7" => P.GETTAG
         | #"8" => P.MKSPECIAL
         | #"9" => P.SETSPECIAL
         | #"0" => P.GETSPECIAL
         | #"!" => P.USELVAR
         | #"@" => P.DEFLVAR
         | #"#" => P.INLDIV
         | #"$" => P.INLMOD
         | #"%" => P.INLREM
         | #"^" => P.INLMIN
         | #"&" => P.INLMAX
         | #"*" => P.INLABS
         | #"(" => P.INLNOT
         | #")" => P.INLCOMPOSE
         | #"," => P.INLBEFORE
         | #"." => P.INL_ARRAY
         | #"/" => P.INL_VECTOR
         | #":" => P.ISOLATE
	 | _ => raise Fail "    | primop")


(*
 * TODO: primtyc is still not implemented yet.
 *)
fun primtyc x = raise Fail " primtyc unimplemented !"

fun stripOpt (SOME x) = x
  | stripOpt _ = raise Fail "    | stripOpt"

fun word t = R.string (t o stripOpt o Word.fromString)
fun word32 t = R.string (t o stripOpt o Word32.fromString)
fun int32 t = R.string (t o stripOpt o Int32.fromString)

fun symbol c = 
  R.string(fn name => %Usymbol(
    case c 
     of #"A" => S.varSymbol name 
      | #"B" => S.tycSymbol name 
      | #"C" => S.sigSymbol name 
      | #"D" => S.strSymbol name 
      | #"E" => S.fctSymbol name 
      | #"F" => S.fixSymbol name 
      | #"G" => S.labSymbol name 
      | #"H" => S.tyvSymbol name 
      | #"I" => S.fsigSymbol name 
      | _ => raise Fail "    | symbol"))

val symbolList =
      list(?symbol,fn Usymbol t => t, fn UsymbolList t => t, UsymbolList)

fun spath x = symbolList x
fun ipath x = symbolList x

fun consig #"S" = R.int(fn i => R.int (fn j => %Uconsig(A.CSIG(i,j))))
  | consig #"N" = %Uconsig (A.CNIL)
  | consig _ = raise Fail "    | consig"

fun mkAccess (mkvar,stamp) = 
let fun access #"L" = R.int(fn i => %Uaccess (mkvar i))
      | access #"E" = R.w8vector(fn v =>
		       %Uaccess(A.EXTERN(PS.fromBytes v)))
      | access #"P" = R.int(fn i =>
		       ?access(fn Uaccess a => 
			%Uaccess(A.PATH(a,i))))
      | access #"N" = %Uaccess A.NO_ACCESS
      | access _ = raise Fail "    | access"

    fun conrep #"U" = %Uconrep A.UNTAGGED
      | conrep #"T" = R.int (fn i => %Uconrep(A.TAGGED i))
      | conrep #"B" = %Uconrep(A.TRANSPARENT)
      | conrep #"C" = R.int(fn i => %Uconrep(A.CONSTANT i))
      | conrep #"R" = %Uconrep(A.REF)
      | conrep #"V" = ?access(fn Uaccess a => %Uconrep(A.EXN a))
      | conrep #"L" = %Uconrep(A.LISTCONS)
      | conrep #"N" = %Uconrep(A.LISTNIL)
      | conrep #"S" = %Uconrep(A.SUSP NONE)
      | conrep #"X" = ?access(fn Uaccess a => 
                       ?access(fn Uaccess b => 
                         %Uconrep(A.SUSP (SOME (a,b)))))
      | conrep _ = raise Fail "    | conrep"

    fun lty #"A" = ?tyc (fn Utyc tc => %Ulty(LT.ltc_tyc tc))
      | lty #"B" = ?ltyList (fn UltyList l => %Ulty(LT.ltc_str l))
      | lty #"C" = 
          ?intltyList (fn UintltyList l => %Ulty(LT.ltc_pst l))
      | lty #"D" = 
          ?ltyList (fn UltyList ts1 => 
           ?ltyList (fn UltyList ts2 => 
		 %Ulty(LT.ltc_fct(ts1,ts2))))
      | lty #"E" = ?tkindList (fn UtkindList ks =>
 		     ?ltyList (fn UltyList ts => 
                         %Ulty(LT.ltc_poly(ks,ts))))
      | lty _ = raise Fail "    | lty"

    and ldTuple #"T" = ?lty (fn Ulty t => 
                         R.int (fn i => %UldTuple(t, i)))
      | ldTuple _ = raise Fail "   | ldTuple"

    and ldOption #"S" = ?ldTuple(fn UldTuple t => %UldOption (SOME t))
      | ldOption #"N" = %UldOption NONE
      | ldOption _ = raise Fail "    | ltyOption"

    and ltyList x = list (?lty,fn Ulty t => t, fn UltyList t => t, UltyList) x

    and ltyListOption #"S" = 
          ?ltyList (fn UltyList ts => %UltyListOption (SOME ts))
      | ltyListOption #"N" = %UltyListOption (NONE)
      | ltyListOption _ = raise Fail "   | ltyListOption"

    and intltyList x = list (?intltyTuple, fn UintltyTuple t => t,
			     fn UintltyList t => t, UintltyList) x

    and intltyTuple #"T" = R.int(fn i =>
			    ?lty(fn Ulty t => %UintltyTuple(i,t)))
      | intltyTuple _ = raise Fail "    | intltyTuple"

    and tyc #"A" = R.int (fn i => R.int (fn j => 
                      %Utyc (LT.tcc_var (DI.di_fromint i, j))))
      | tyc #"B" = R.int (fn v => 
                     R.int (fn d => 
                       R.int (fn i => 
                         %Utyc (LT.tcc_nvar(v, DI.di_fromint d, i)))))
      | tyc #"C" = R.int (fn k => %Utyc (LT.tcc_prim (PT.pt_fromint k)))
      | tyc #"D" = ?tkindList (fn UtkindList ks => 
                      ?tyc (fn Utyc tc => %Utyc(LT.tcc_fn(ks, tc))))
      | tyc #"E" = ?tyc (fn Utyc tc => 
                      ?tycList (fn UtycList ts => %Utyc(LT.tcc_app(tc, ts))))
      | tyc #"F" = ?tycList (fn UtycList ts => %Utyc(LT.tcc_seq ts))
      | tyc #"G" = ?tyc (fn Utyc tc => R.int (fn i =>
                            %Utyc(LT.tcc_proj(tc, i))))
      | tyc #"H" = ?tycList (fn UtycList ts => %Utyc(LT.tcc_sum ts))
      | tyc #"I" = R.int (fn n => 
                    ?tyc (fn Utyc tc => 
                      ?tycList (fn UtycList ts => 
                         R.int (fn i => 
                            %Utyc(LT.tcc_fix((n,tc,ts), i))))))
      | tyc #"J" = ?tyc (fn Utyc tc => %Utyc(LT.tcc_abs tc))
      | tyc #"K" = ?tyc (fn Utyc tc => %Utyc(LT.tcc_box tc))
      | tyc #"L" = ?tycList (fn UtycList ts => %Utyc(LT.tcc_tuple ts))
      | tyc #"M" = ?bool(fn Ubool b1 =>
                    ?bool(fn Ubool b2 =>
                      ?tycList (fn UtycList ts1 => 
                      ?tycList (fn UtycList ts2 =>  
                         %Utyc(LT.tcc_arrow((b1,b2),ts1, ts2))))))
      | tyc _ = raise Fail "    | tyc"

    and tycList x = list (?tyc, fn Utyc t => t, fn UtycList t => t, UtycList) x

    and tkind #"A" = %Utkind (LT.tkc_mono)
      | tkind #"B" = %Utkind (LT.tkc_box)
      | tkind #"C" = ?tkindList (fn UtkindList ks => 
                        %Utkind (LT.tkc_seq ks))
      | tkind #"D" = ?tkindList (fn UtkindList ks =>
                       ?tkind (fn Utkind k => 
                        %Utkind (LT.tkc_fun(ks, k))))
      | tkind _ = raise Fail "    | tkind"

    and tkindList x = 
          list (?tkind, fn Utkind t => t, fn UtkindList t => t, UtkindList) x

    and tkindtycTuple #"T" = ?tkind (fn Utkind k =>
                              ?tyc (fn Utyc t => %UtkindtycTuple(k, t)))
      | tkindtycTuple _ = raise Fail "   | tkindtycTuple"

    and tkindtycList x = 
          list (?tkindtycTuple, fn UtkindtycTuple t => t, 
                fn UtkindtycList t => t, UtkindtycList) x

    fun tycsLvarPair #"T" = ?tycList (fn UtycList ts => 
                               R.int (fn v => %UtycsLvarPair (ts, v)))
      | tycsLvarPair _ = raise Fail "   | tycsLvarPair"

    fun tycsLvarPairList x = 
          list (?tycsLvarPair, fn UtycsLvarPair t => t,
                fn UtycsLvarPairList t => t, UtycsLvarPairList) x

    fun con #"." = ?dcon (fn Udcon (dc, ts) =>
		    ?lvar (fn Ulvar v  => 
		     ?lexp (fn Ulexp e =>
                      %Ucon (F.DATAcon (dc, ts, v), e))))
      | con #"," = R.int (fn i => 
                    ?lexp (fn Ulexp e => %Ucon (F.INTcon i, e)))
      | con #"=" = int32 (fn i32 => 
		    ?lexp (fn Ulexp e =>
		     %Ucon (F.INT32con i32, e)))
      | con #"?" = word (fn w =>
		    ?lexp (fn Ulexp e =>
		     %Ucon (F.WORDcon w, e)))
      | con #">" = word32 (fn w32 =>
		    ?lexp (fn Ulexp e =>
		     %Ucon (F.WORD32con w32, e)))
      | con #"<" = R.string (fn s =>
		    ?lexp (fn Ulexp e =>
		     %Ucon (F.REALcon s, e)))
      | con #"'" = R.string (fn s =>
		    ?lexp (fn Ulexp e =>
		     %Ucon (F.STRINGcon s, e)))
      | con #";" = R.int (fn i =>  
                    ?lexp (fn Ulexp e => %Ucon (F.VLENcon i, e)))
      | con _ = raise Fail "    | con"

    and conList x =
	list (?con, fn Ucon c => c, fn UconList l => l, UconList) x

    and dcon #"^" = ?symbol(fn Usymbol s =>
                     ?conrep (fn Uconrep cr =>
                      ?lty (fn Ulty t =>
                       ?tycList (fn UtycList ts => 
                        %Udcon((s, cr, t), ts)))))
      | dcon _ = raise Fail "    | dcon"

    and dict #"%" = R.int (fn v => 
                      ?tycsLvarPairList (fn UtycsLvarPairList tbls => 
                            %Udict {default=v, table=tbls}))
      | dict _ = raise Fail "    | dict"

    and value #"a" = R.int (fn v => %Uvalue (F.VAR v))
      | value #"b" = R.int (fn i => %Uvalue (F.INT i))
      | value #"c" = int32 (fn i32 => %Uvalue (F.INT32 i32))
      | value #"d" = word (fn w => %Uvalue (F.WORD w))
      | value #"e" = word32 (fn w32 => %Uvalue (F.WORD32 w32))
      | value #"f" = R.string (fn s => %Uvalue (F.REAL s))
      | value #"g" = R.string (fn s => %Uvalue (F.STRING s))
      | value _ = raise Fail "    | value"

    and fprim #"h" = ?primop (fn Uprimop p =>
		      ?lty (fn Ulty t =>
                       ?tycList (fn UtycList ts => 
  		        %Ufprim (NONE, p, t, ts))))

      | fprim #"i" = ?dict (fn Udict nd => 
                      ?primop (fn Uprimop p =>
		       ?lty (fn Ulty t =>
                        ?tycList (fn UtycList ts => 
  		         %Ufprim (SOME nd, p, t, ts)))))

      | fprim _ = raise Fail "    | fprim"

    and valueList x = 
      list (?value, fn Uvalue v => v, fn UvalueList l => l, UvalueList) x
  
    and lexp #"j" = ?valueList (fn UvalueList vs => %Ulexp (F.RET vs))
      | lexp #"k" = ?lvarList (fn UlvarList vs =>
                     ?lexp (fn Ulexp e1 =>
                      ?lexp (fn Ulexp e2 => %Ulexp (F.LET(vs, e1, e2)))))
      | lexp #"l" = ?fundecList (fn UfundecList fdecs =>
                     ?lexp (fn Ulexp e =>
                        %Ulexp  (F.FIX(fdecs, e))))
      | lexp #"m" = ?value (fn Uvalue u =>
		     ?valueList (fn UvalueList vs =>
		      %Ulexp (F.APP (u, vs))))
      | lexp #"n" = ?tfundec (fn Utfundec tfdec =>
                     ?lexp (fn Ulexp e =>
                      %Ulexp (F.TFN (tfdec, e))))
      | lexp #"o" = ?value (fn Uvalue u =>
                      ?tycList (fn UtycList ts =>
                       %Ulexp (F.TAPP (u, ts))))
      | lexp #"p" = ?value (fn Uvalue v =>
		     ?consig (fn Uconsig crl =>
		      ?conList (fn UconList cel =>
		       ?lexpOption (fn UlexpOption eo =>
			%Ulexp (F.SWITCH (v, crl, cel, eo))))))
      | lexp #"q" = ?dcon (fn Udcon (c, ts) =>
	             ?value (fn Uvalue u =>
                      ?lvar (fn Ulvar v => 
                       ?lexp (fn Ulexp e =>
			%Ulexp (F.CON (c, ts, u, v,e ))))))
      | lexp #"r" = ?rkind (fn Urkind rk =>
                     ?valueList (fn UvalueList vl => 
                      ?lvar (fn Ulvar v =>
                       ?lexp (fn Ulexp e =>
                        %Ulexp (F.RECORD(rk, vl, v, e))))))
      | lexp #"s" = ?value (fn Uvalue u =>
                      R.int (fn i => 
                       ?lvar (fn Ulvar v =>
                        ?lexp (fn Ulexp e =>
                         %Ulexp (F.SELECT(u, i, v, e))))))
      | lexp #"t" = ?value (fn Uvalue v =>
		     ?ltyList (fn UltyList ts =>
		      %Ulexp (F.RAISE (v, ts))))
      | lexp #"u" = ?lexp (fn Ulexp e =>
		     ?value (fn Uvalue u =>
		      %Ulexp (F.HANDLE (e, u))))
      | lexp #"v" = ?fprim (fn Ufprim p => 
                     ?valueList (fn UvalueList vs =>
                      ?lexp (fn Ulexp e1 =>
                       ?lexp (fn Ulexp e2 =>
                        %Ulexp (F.BRANCH(p, vs, e1, e2))))))
      | lexp #"w" = ?fprim (fn Ufprim p => 
                     ?valueList (fn UvalueList vs =>
                      ?lvar (fn Ulvar v =>
                       ?lexp (fn Ulexp e =>
                        %Ulexp (F.PRIMOP(p, vs, v, e))))))
      | lexp _ = raise Fail "    | lexp"


    and lexpList x =
	list (?lexp, fn Ulexp e => e, fn UlexpList l => l, UlexpList) x

    and lexpOption #"S" = ?lexp (fn Ulexp e => %UlexpOption (SOME e))
      | lexpOption #"N" = %UlexpOption NONE
      | lexpOption _ = raise Fail "    | lexpOption"

    and fundec #"0" = ?fkind (fn Ufkind fk =>
                       ?lvar (fn Ulvar v =>
                        ?lvarLtyPairList (fn UlvarLtyPairList vts =>
                         ?lexp (fn Ulexp e =>
                           %Ufundec (fk, v, vts, e)))))
      | fundec _ = raise Fail "    | fundec"

    and fundecOption #"S" = ?fundec (fn Ufundec f => %UfundecOption (SOME f))
      | fundecOption #"N" = %UfundecOption NONE
      | fundecOption _ = raise Fail "    | fundecOption"

    and fundecList x = 
        list (?fundec, fn Ufundec x => x, fn UfundecList l => l,
              UfundecList) x

    and lvarLtyPair #"T" = ?lvar (fn Ulvar v => 
                            ?lty (fn Ulty t => %UlvarLtyPair (v, t)))
      | lvarLtyPair _ = raise Fail "   | lvarLtyPair"

    and lvarLtyPairList x = 
        list (?lvarLtyPair, fn UlvarLtyPair x => x, fn UlvarLtyPairList l => l,
              UlvarLtyPairList) x

    and tvarTkPair #"T" = ?lvar (fn Ulvar tv =>
                           ?tkind (fn Utkind tk => %UtvarTkPair (tv, tk)))
      | tvarTkPair _ = raise Fail "   | tvarTkPair"

    and tvarTkPairList x = 
        list (?tvarTkPair, fn UtvarTkPair x => x, fn UtvarTkPairList l => l,
              UtvarTkPairList) x

    and tfundec #"0" = ?lvar (fn Ulvar v =>
                        ?tvarTkPairList (fn UtvarTkPairList tvks =>
                         ?lexp (fn Ulexp e =>
                           %Utfundec (v, tvks, e))))
      | tfundec _ = raise Fail "    | tfundec"

    and fkind #"2" = %Ufkind (F.FK_FCT)
      | fkind #"3" = ?ltyListOption (fn UltyListOption isrec =>
                      ?bool (fn Ubool b1 =>
                       ?bool (fn Ubool b2 =>
                        ?bool (fn Ubool known =>
                         ?bool (fn Ubool inline =>
                          %Ufkind (F.FK_FUN{isrec=isrec, fixed=(b1, b2), 
                                            known=known, inline=inline}))))))
      | fkind _ = raise Fail "    | fkind"

    and rkind #"4" = ?tyc (fn Utyc tc => %Urkind (F.RK_VECTOR tc))
      | rkind #"5" = %Urkind (F.RK_STRUCT)
      | rkind #"6" = %Urkind (FlintUtil.rk_tuple)
      | rkind _ = raise Fail "    | rkind"

    fun ldOptionList x =
	list (?ldOption, fn UldOption to => to,
	      fn UldOptionList tol => tol, UldOptionList) x

 in {access=access, lexp=lexp, conrep=conrep, 
     tkind=tkind, fundecOption=fundecOption, ldOptionList=ldOptionList}
end

fun mkStamp globalPid =
  let fun stamp #"L" =
            R.int(fn j =>
	       %Ustamp(Stamps.STAMP{scope=Stamps.GLOBAL globalPid, count=j}))
	| stamp #"G" =
	    R.w8vector(fn s =>
	     R.int(fn j =>
	     %Ustamp(Stamps.STAMP{scope=Stamps.GLOBAL(PS.fromBytes s),
				  count=j})))
        | stamp #"S" =
	    R.string(fn s => R.int(fn j =>
	    %Ustamp(Stamps.STAMP{scope=Stamps.SPECIAL s, count=j})))

        | stamp _ = raise Fail "    | stamp"
   in stamp
  end

fun unpickleFLINT({hash: PS.persstamp, pickle: Word8Vector.vector}) = 
  let val stamp = mkStamp hash     (* ZHONG? *)
      val {fundecOption, ...} = mkAccess(A.LVAR,stamp)
      val UfundecOption result = R.root(pickle, fundecOption)
   in result
  end

(**************************************************************************
 *                   UNPICKLING AN ENVIRONMENT                            *
 **************************************************************************)

fun unpickleEnv (context0, pickle) =
  let val {hash=globalPid, pickle=p0: Word8Vector.vector} = pickle

      fun import i = A.PATH (A.EXTERN globalPid, i)
      val stamp = mkStamp globalPid
      val {access,lexp,conrep,tkind,ldOptionList,fundecOption} = 
             mkAccess(import,stamp)


      val entVar = stamp
      val entPath = list(?entVar, fn Ustamp t => t, 
                         fn UentPath t => t, UentPath)

      fun modId #"B" = ?stamp(fn Ustamp a => ?stamp(fn Ustamp b => 
			 %UmodId(MI.STRid{rlzn=a,sign=b})))
        | modId #"C" = ?stamp(fn Ustamp s => %UmodId(MI.SIGid s))
        | modId #"E" = ?stamp(fn Ustamp a => ?modId(fn UmodId b =>
			 %UmodId(MI.FCTid{rlzn=a,sign=b})))
        | modId #"F" = ?stamp(fn Ustamp a => ?stamp(fn Ustamp b => 
 			 %UmodId(MI.FSIGid{paramsig=a,bodysig=b})))
        | modId #"G" = ?stamp(fn Ustamp s => 
  		         %UmodId(MI.TYCid s))
        | modId #"V" = ?stamp(fn Ustamp s => %UmodId(MI.EENVid s))

        | modId _ = raise Fail "    | modId"

      val label = symbol

      fun eqprop c = 
        %Ueqprop(case c
	  of #"Y" => T.YES 
	   | #"N" => T.NO  
	   | #"I" => T.IND 
	   | #"O" => T.OBJ 
	   | #"D" => T.DATA
	   | #"A" => T.ABS
	   | #"U" => T.UNDEF
           | _ => raise Fail "    | eqprop")


      fun datacon #"D" = 
            ?symbol(fn Usymbol n =>
  		?bool(fn Ubool c =>
		   ?ty(fn Uty t =>
		       ?conrep(fn Uconrep r => 
			  ?consig(fn Uconsig s =>
			   %Udatacon(T.DATACON{name=n,const=c,typ=t,
					       rep=r,sign=s}))))))
        | datacon _ = raise Fail "    | datacon"


      and tyOption #"S" = ?ty(fn Uty t => %UtyOption (SOME t))
        | tyOption #"N" = %UtyOption NONE
        | tyOption _ = raise Fail "    | tyOption"

      and tyList x = list(?ty, fn Uty t => t, fn UtyList t => t, UtyList) x

      and tyckind #"P" = 
            R.int(fn k => %Utyckind(T.PRIMITIVE (PT.pt_fromint k)))
        | tyckind #"D" = 
            R.int(fn i =>
              ?entVarOption(fn UentVarOption root =>
              ?dtypeInfo (fn UdtypeInfo (ss, family, freetycs) =>
                   %Utyckind(T.DATATYPE {index=i,root=root,family=family,
                                         stamps=ss, freetycs=freetycs}))))
        | tyckind #"A" =
            ?tycon (fn Utycon tc => %Utyckind(T.ABSTRACT tc))
        | tyckind #"S" = raise Fail "     | tyckind-tycpath"
        | tyckind #"F" = %Utyckind T.FORMAL
        | tyckind #"T" = %Utyckind T.TEMP
        | tyckind _ = raise Fail "    | tyckind"

      and dtypeInfo #"Z" = 
            ?stampList (fn UentPath ss =>
               ?dtFamily (fn UdtFamily ff =>
                 ?tyconList (fn UtyconList tt =>
                    %UdtypeInfo(Vector.fromList ss, ff, tt))))
        | dtypeInfo _ = raise Fail "   | dtypeInfo"

      and dtFamily #"U" = 
            ?stamp (fn Ustamp s => 
               ?dtmemberList (fn UdtmemberList ds =>
                   %UdtFamily ({mkey=s, members=Vector.fromList ds,
                                lambdatyc = ref NONE})))
        | dtFamily _ = raise Fail "    | dtFamily"

      and stampList x = entPath x

      and dtmemberList x =
        list(?dtmember, fn Udtmember t => t,
	     fn UdtmemberList t => t, UdtmemberList) x

      and dtmember #"T" = 
  	?symbol(fn Usymbol n =>
	    ?nameRepDomainList(fn UnameRepDomainList l =>
              R.int(fn i =>
	        ?eqprop(fn Ueqprop e =>
		  ?consig(fn Uconsig sn =>
		    %Udtmember{tycname=n,dcons=l,arity=i,
			       eq=ref e,sign=sn})))))
        | dtmember _ = raise Fail "    | dtmember"

      and nameRepDomainList x =
        list(?nameRepDomain,fn UnameRepDomain t => t,
	    fn UnameRepDomainList t => t, UnameRepDomainList) x

      and nameRepDomain #"N" = 
        ?symbol(fn Usymbol n =>
	    ?conrep(fn Uconrep r =>
	      ?tyOption(fn UtyOption t =>
	         %UnameRepDomain{name=n,rep=r,domain=t})))
        | nameRepDomain _ = raise Fail "    | nameRepDomain"

      and tycon #"X" = ?modId(fn UmodId id => 
		      case SCStaticEnv.lookTYC context0 id
                       of SOME t => %Utycon t)
        | tycon #"G" = ?stamp(fn Ustamp s =>
		     R.int(fn a =>
		      ?eqprop(fn Ueqprop e =>
		       ?tyckind(fn Utyckind k =>
			?ipath(fn UsymbolList p =>
			 %Utycon(T.GENtyc{stamp=s,arity=a,eq=ref e,kind=k,
					  path=IP.IPATH p}))))))
        | tycon #"D" = ?stamp(fn Ustamp x =>
		     R.int(fn r =>
		      ?ty(fn Uty b =>
		       ?boolList(fn UboolList s => 
			?ipath(fn UsymbolList p =>
			 %Utycon(T.DEFtyc{stamp=x,
                                          tyfun=T.TYFUN{arity=r,body=b},
					  strict=s,path=IP.IPATH p}))))))

        | tycon #"P" = R.int(fn a =>
                        ?ipath(fn UsymbolList p =>
                         ?entPath(fn UentPath e =>
                           %Utycon(T.PATHtyc{arity=a, entPath=e,
                                             path=IP.IPATH p}))))

        | tycon #"R" = ?symbolList(fn UsymbolList l =>
		      %Utycon(T.RECORDtyc l))
        | tycon #"C" = R.int(fn i => %Utycon(T.RECtyc i))
        | tycon #"H" = R.int(fn i => %Utycon(T.FREEtyc i))
        | tycon #"E" = %Utycon(T.ERRORtyc)
        | tycon _ = raise Fail "    | tycon"

      and tyconList x = list(?tycon, fn Utycon t => t, 
                           fn UtyconList t => t, UtyconList) x


      and symbolOption #"S" = ?symbol(fn Usymbol s => 
		           %UsymbolOption(SOME s))
        | symbolOption #"N" = %UsymbolOption NONE
        | symbolOption _ = raise Fail "    | symbolOption"

      and intOption #"S" = R.int (fn s => %UintOption(SOME s))
        | intOption #"N" = %UintOption NONE
        | intOption _ = raise Fail "    | intOption"

      and spathList x =
        list(?spath,fn UsymbolList t => SP.SPATH t, fn UspathList t => t,
  	     UspathList) x

      and spathListList x =
        list(?spathList,fn UspathList l => l, fn UspathListList t => t,
  	     UspathListList) x

      and ty #"C" = ?tycon(fn Utycon c => ?tyList(fn UtyList l => 
		   %Uty(T.CONty(c,l))))
        | ty #"N" = ?tycon(fn Utycon c => %Uty(T.CONty(c,nil)))
        | ty #"I" = R.int(fn i => %Uty(T.IBOUND i))
        | ty #"W" = %Uty T.WILDCARDty
        | ty #"P" = ?boolList(fn UboolList s =>
  		      R.int(fn r => 
  		      ?ty(fn Uty b =>
		      %Uty(T.POLYty{sign=s, tyfun=T.TYFUN{arity=r,body=b}}))))
        | ty #"U" = %Uty(T.UNDEFty)
        | ty _ = raise Fail "     | ty"

      and inl_info #"P" = ?primop(fn Uprimop p => 
                            ?tyOption(fn UtyOption t => 
                              %Uinl_info(II.INL_PRIM(p, t))))

        | inl_info #"S" = ?inl_infoList(fn Uinl_infoList sl =>
                              %Uinl_info(II.INL_STR sl))

        | inl_info #"N" = %Uinl_info(II.INL_NO)

        | inl_info #"L" = raise Fail "INL_LEXP not implemented"

        | inl_info #"A" = ?access(fn Uaccess a =>
                            ?tyOption(fn UtyOption t => 
                              %Uinl_info(II.INL_PATH(a, t))))


      and inl_infoList s = list(?inl_info, (fn Uinl_info x => x),
                                (fn Uinl_infoList x => x), Uinl_infoList) s

      and var #"V" = ?access(fn Uaccess a => 
                      ?inl_info(fn Uinl_info z => 
   		       ?spath(fn UsymbolList p =>
		         ?ty(fn Uty t =>
		            %Uvar(V.VALvar{access=a, info=z, 
                                           path=SP.SPATH p, typ=ref t})))))

        | var #"O" = ?symbol(fn Usymbol n =>
  		        ?overldList(fn UoverldList p =>
		           R.int(fn r=> 
		             ?ty(fn Uty b =>
		                %Uvar(V.OVLDvar{name=n,options=ref p,
				       scheme=T.TYFUN{arity=r,body=b}})))))

        | var #"E" = %Uvar(V.ERRORvar)
        | var _ = raise Fail "     | var"

      and overld #"O" = ?ty(fn Uty i => ?var(fn Uvar v => 
			   %Uoverld{indicator=i,variant=v}))
   
      and overldList x = list(?overld, fn Uoverld t => t,
			      fn UoverldList t => t, UoverldList) x


      and strDef #"C" = 
            ?Structure(fn UStructure s => %UstrDef(M.CONSTstrDef s))
        | strDef #"V" = 
            ?Signature(fn USignature s =>
  	     ?entPath(fn UentPath a =>
	      %UstrDef(M.VARstrDef(s,a))))
        | strDef _ = raise Fail "     | strDef"

      and strDefIntTuple #"T" =
	    ?strDef(fn UstrDef s =>
	     R.int(fn i =>
	      %UstrDefIntTuple(s,i)))
        | strDefIntTuple _ = raise Fail "   | strDefIntTuple"

      and strDefIntOption #"S" =
	   ?strDefIntTuple(fn UstrDefIntTuple d =>
	    %UstrDefIntOption(SOME d))
        | strDefIntOption #"N" = %UstrDefIntOption NONE
        | strDefIntOption _ = raise Fail "    | strDefIntOption"

      and elements x = 
          list (?element,fn Uelement t => t, fn Uelements t => t, Uelements) x

      and element #"T" =
	    ?symbol(fn Usymbol s =>
	     ?spec(fn Uspec c =>
	      %Uelement(s,c)))
        | element _ = raise Fail "    | element"


      and boundepsElem #"T" = ?entPath(fn UentPath a => 
                              ?tkind(fn Utkind tk => %UboundepsElem(a, tk)))
        | boundepsElem _ = raise Fail "    | boundepsElem"

      and boundepsList x = 
            list(?boundepsElem, fn UboundepsElem t => t,
		  fn UboundepsList t => t,  UboundepsList) x

      and boundepsOption #"S" = ?boundepsList(fn UboundepsList x => 
  		           %UboundepsOption(SOME x))
        | boundepsOption #"N" = %UboundepsOption NONE
        | boundepsOption _ = raise Fail "    | boundepsOption"

      and Signature #"X" = ?modId(fn UmodId id =>
	    case SCStaticEnv.lookSIG context0 id
             of SOME t => %USignature t)

        | Signature #"S" =
	    ?symbolOption(fn UsymbolOption k =>
	     ?bool(fn Ubool c =>
	     ?bool(fn Ubool f =>
	      ?stamp(fn Ustamp m =>
	       ?symbolList(fn UsymbolList l =>
		?elements(fn Uelements e =>
		 ?boundepsOption(fn UboundepsOption b =>
		   ?spathListList(fn UspathListList t =>
		    ?spathListList(fn UspathListList s =>
		     %USignature(M.SIG{name=k,closed=c,fctflag=f,
                                       stamp=m, symbols=l,
				       elements=e, boundeps=ref b,
				       lambdaty=ref NONE,
				       typsharing=t,strsharing=s}))))))))))
        | Signature #"E" = %USignature M.ERRORsig
        | Signature _ = raise Fail "     | Signature"

      and fctSig #"X" = ?modId(fn UmodId id =>
		      case SCStaticEnv.lookFSIG context0 id
                       of SOME t => %UfctSig t)
        | fctSig #"F" = 
             ?symbolOption(fn UsymbolOption k =>
              ?Signature(fn USignature p =>
	       ?entVar(fn Ustamp q =>
	        ?symbolOption(fn UsymbolOption s =>
	         ?Signature(fn USignature b =>
                   %UfctSig(M.FSIG{kind=k,paramsig=p,paramvar=q,paramsym=s,
					  bodysig=b}))))))
        | fctSig #"E" = %UfctSig M.ERRORfsig
        | fctSig _ = raise Fail "    | fctSig"

      and spec #"T" = ?tycon(fn Utycon t => 
		       ?entVar(fn Ustamp v => 
			R.int(fn s =>
		         %Uspec(M.TYCspec{spec=t, entVar=v, scope=s}))))
        | spec #"S" = ?Signature (fn USignature s =>
  		       R.int (fn d =>
		        ?strDefIntOption(fn UstrDefIntOption e =>
		         ?entVar (fn Ustamp v =>
		          %Uspec(M.STRspec{sign=s, slot=d, def=e, entVar=v})))))
        | spec #"F" = ?fctSig (fn UfctSig s =>
		    R.int (fn d =>
		     ?entVar (fn Ustamp v =>
		      %Uspec(M.FCTspec{sign=s, slot=d, entVar=v}))))
        | spec #"P" = ?ty (fn Uty t => R.int(fn d => 
                    %Uspec(M.VALspec{spec=t,slot=d})))
        | spec #"Q" = ?datacon (fn Udatacon c => 
                    ?intOption (fn UintOption d => 
                     %Uspec(M.CONspec{spec=c,slot=d})))
        | spec _ = raise Fail "    | spec"

      and entity #"L" = ?tycEntity(fn Utycon t => %Uentity(M.TYCent t))
        | entity #"S" = ?strEntity(fn UstrEntity t => %Uentity(M.STRent t))
        | entity #"F" = ?fctEntity(fn UfctEntity t => %Uentity(M.FCTent t))
        | entity #"E" = %Uentity(M.ERRORent)
        | entity _ = raise Fail "    | entity"

      and fctClosure #"F" = 
            ?entVar(fn Ustamp p =>
  		?strExp(fn UstrExp s =>
		   ?entityEnv(fn UentityEnv e =>
		    %UfctClosure(M.CLOSURE{param=p,body=s,env=e}))))
        | fctClosure _ = raise Fail "    | fctClosure"

      and Structure #"X" = ?modId(fn UmodId id =>
			    ?access(fn Uaccess a =>
			         case SCStaticEnv.lookSTR context0 id
				   of SOME(M.STR{sign=s,rlzn=r,access=_,info=z})
				       => %UStructure(M.STR{sign=s,rlzn=r,
							    access=a,info=z})
				    | NONE => 
				       raise Fail "missing external Structure"))
        | Structure #"S" = 
              ?Signature (fn USignature s =>
		 ?strEntity (fn UstrEntity r =>
		   ?access (fn Uaccess a =>
                     ?inl_info (fn Uinl_info z =>
			   %UStructure(M.STR{sign=s, rlzn=r, access=a, 
                                             info=z})))))

        | Structure #"G" = ?Signature (fn USignature s =>
			 ?entPath (fn UentPath a =>
			  %UStructure(M.STRSIG{sign=s,entPath=a})))
        | Structure #"E" = %UStructure M.ERRORstr

        | Structure _ = raise Fail "    | Structure"

      and Functor #"X" = ?modId(fn UmodId id =>
			  ?access(fn Uaccess a =>
		               case SCStaticEnv.lookFCT context0 id
				 of SOME(M.FCT{sign=s,rlzn=r,access=_,info=z}) => 
				       %UFunctor(M.FCT{sign=s,rlzn=r,access=a,info=z})
				  | NONE =>
				     raise Fail "missing external Functor"))
        | Functor #"F" = 
             ?fctSig(fn UfctSig s =>
	        ?fctEntity(fn UfctEntity r =>
		   ?access(fn Uaccess a =>
                      ?inl_info(fn Uinl_info z => 
			%UFunctor(M.FCT{sign=s, rlzn=r, access=a,
                                        info=z})))))
        | Functor #"E" = %UFunctor M.ERRORfct

        | Functor _ = raise Fail "    | Functor"

      and stampExp #"C" = ?stamp(fn Ustamp s => %UstampExp(M.CONST s))
        | stampExp #"G" = ?strExp(fn UstrExp s => %UstampExp(M.GETSTAMP s))
        | stampExp #"N" = %UstampExp M.NEW
        | stampExp _ = raise Fail "    | stampExp"

      and entVarOption #"S" = ?entVar(fn Ustamp x => 
     		                  %UentVarOption(SOME x))
        | entVarOption #"N" = %UentVarOption NONE
        | entVarOption _ = raise Fail "    | entVarOption"

      and tycExp #"C" = ?tycon(fn Utycon t => %UtycExp(M.CONSTtyc t))
        | tycExp #"D" = ?tycon(fn Utycon t => %UtycExp(M.FORMtyc t))
        | tycExp #"V" = ?entPath(fn UentPath s => %UtycExp(M.VARtyc s))
        | tycExp _ = raise Fail "    | tycExp"

      and strExp #"V" = ?entPath(fn UentPath s => %UstrExp(M.VARstr s))
        | strExp #"C" = ?strEntity(fn UstrEntity s => %UstrExp(M.CONSTstr s))
        | strExp #"S" = ?stampExp(fn UstampExp s =>
		     ?entityDec(fn UentityDec e=>
		      %UstrExp(M.STRUCTURE{stamp=s,entDec=e})))
        | strExp #"A" = ?fctExp(fn UfctExp f =>
		     ?strExp(fn UstrExp s =>
		      %UstrExp(M.APPLY(f,s))))
        | strExp #"L" = ?entityDec(fn UentityDec e =>
		     ?strExp(fn UstrExp s =>
		      %UstrExp(M.LETstr(e,s))))
        | strExp #"B" = ?Signature(fn USignature s => 
                     ?strExp(fn UstrExp e => 
                      %UstrExp(M.ABSstr(s,e))))
        | strExp #"R" = ?entVar(fn Ustamp s =>
			 ?strExp(fn UstrExp e1 =>
			  ?strExp(fn UstrExp e2 =>
			   %UstrExp(M.CONSTRAINstr{boundvar=s,raw=e1,coercion=e2}))))
        | strExp #"F" = ?fctSig(fn UfctSig x => 
                           %UstrExp(M.FORMstr x))
        | strExp _ = raise Fail "    | strExp"

      and fctExp #"V" = ?entPath(fn UentPath s => %UfctExp(M.VARfct s))
        | fctExp #"C" = ?fctEntity(fn UfctEntity s => %UfctExp(M.CONSTfct s))
        | fctExp #"L" = ?entVar(fn Ustamp p => 
 		          ?strExp(fn UstrExp b =>
		             %UfctExp(M.LAMBDA{param=p, body=b})))
        | fctExp #"P" = ?entVar(fn Ustamp p => 
 		          ?strExp(fn UstrExp b =>
                            ?fctSig(fn UfctSig x => 
 		              %UfctExp(M.LAMBDA_TP{param=p, body=b,
                                                   sign=x}))))
        | fctExp #"T" = ?entityDec(fn UentityDec e =>
		          ?fctExp (fn UfctExp f =>
		             %UfctExp(M.LETfct(e,f))))
        | fctExp _ = raise Fail "    | fctExp"

      and entityExp #"T" = ?tycExp(fn UtycExp t => %UentityExp(M.TYCexp t))
        | entityExp #"S" = ?strExp(fn UstrExp t => %UentityExp(M.STRexp t))
        | entityExp #"F" = ?fctExp(fn UfctExp t => %UentityExp(M.FCTexp t))
        | entityExp #"D" = %UentityExp(M.DUMMYexp)
        | entityExp #"E" = %UentityExp(M.ERRORexp)
        | entityExp _ = raise Fail "    | entityExp"

      and entityDec #"T" = ?entVar(fn Ustamp s => ?tycExp(fn UtycExp x =>
			  %UentityDec(M.TYCdec(s,x))))
        | entityDec #"S" = ?entVar(fn Ustamp s => ?strExp(fn UstrExp x =>
			       ?symbol(fn Usymbol n =>
			  %UentityDec(M.STRdec(s,x,n)))))
        | entityDec #"F" = ?entVar(fn Ustamp s => ?fctExp(fn UfctExp x =>
 			  %UentityDec(M.FCTdec(s,x))))
        | entityDec #"Q" = ?entityDecList(fn UentityDecList e =>
			  %UentityDec(M.SEQdec e))
        | entityDec #"L" = ?entityDec(fn UentityDec a =>
                             ?entityDec(fn UentityDec b =>
   			       %UentityDec(M.LOCALdec(a,b))))
        | entityDec #"E" =   %UentityDec M.ERRORdec
        | entityDec #"M" =   %UentityDec M.EMPTYdec
        | entityDec _ = raise Fail "    | entityDec"

      and entityDecList x = list(?entityDec,fn UentityDec t => t,
			     fn UentityDecList t => t, UentityDecList) x


      and entityEnv #"X" = 
            ?modId(fn UmodId id =>
		       case SCStaticEnv.lookEENV context0 id
                        of SOME e => %UentityEnv e
		         | NONE => raise Fail "missing external entityEnv")
        | entityEnv #"M" = ?stamp(fn Ustamp s =>
			?entityEnv(fn UentityEnv r =>
			  %UentityEnv(M.MARKeenv(s,r))))
        | entityEnv #"B" = 
            ?entVElist(fn UentVElist vs =>
  	      ?entityEnv(fn UentityEnv r =>
		%UentityEnv(M.BINDeenv(
                   foldr (fn ((v,e), z) => ED.insert(z,v,e)) (ED.mkDict()) vs, 
                   r))))
        | entityEnv #"N" = %UentityEnv(M.NILeenv)
        | entityEnv #"E" = %UentityEnv(M.ERReenv)

        | entityEnv _ = raise Fail "    | entityEnv"

      and entVElist x = list (?entVETuple, fn UentVETuple x => x,
                              fn UentVElist x => x, UentVElist) x

      and entVETuple #"T" = ?entVar (fn Ustamp v =>
                              ?entity (fn Uentity e => %UentVETuple(v, e)))
        | entVETuple _ = raise Fail "   | entVETuple"

      and strEntity #"S" = ?stamp(fn Ustamp s =>
		        ?entityEnv(fn UentityEnv e =>
			  ?ipath(fn UsymbolList r =>
			   %UstrEntity{stamp=s,entities=e,
				       lambdaty=ref NONE,
				       rpath=IP.IPATH r})))

        | strEntity _ = raise Fail "    | strEntity"

      and fctEntity #"F" = ?stamp(fn Ustamp s =>
			?fctClosure(fn UfctClosure c =>
			  ?ipath(fn UsymbolList r =>
			   %UfctEntity{stamp=s,closure=c,
				       lambdaty=ref NONE,
                                       tycpath=NONE, rpath=IP.IPATH r})))

        | fctEntity _ = raise Fail "    | fctEntity"

      and tycEntity x = tycon x


      fun fixity #"N" = %Ufixity Fixity.NONfix
        | fixity #"I" = 
            R.int(fn i => R.int(fn j => %Ufixity(Fixity.INfix(i,j))))
        | fixity _ = raise Fail "    | fixity"

      fun binding #"V" = ?var(fn Uvar x => %Ubinding(B.VALbind x))
        | binding #"C" = ?datacon(fn Udatacon x => %Ubinding(B.CONbind x))
        | binding #"T" = ?tycon(fn Utycon x => %Ubinding(B.TYCbind x))
        | binding #"G" = ?Signature(fn USignature x => %Ubinding(B.SIGbind x))
        | binding #"S" = ?Structure(fn UStructure x => %Ubinding(B.STRbind x))
        | binding #"I" = ?fctSig(fn UfctSig x => %Ubinding(B.FSGbind x))
        | binding #"F" = ?Functor(fn UFunctor x => %Ubinding(B.FCTbind x))
        | binding #"X" = ?fixity(fn Ufixity x => %Ubinding(B.FIXbind x))
        | binding _ = raise Fail "    | binding"
 
      fun bind #"T" = ?symbol(fn Usymbol s =>
 		        ?binding(fn Ubinding b =>
		           %Ubind(s,b)))
        | bind _ = raise Fail "    | bind"

      val bindList = list(?bind, fn Ubind t => t, 
                          fn UbindList t => t, UbindList)

      fun env #"E" = ?bindList(fn UbindList l =>
		 %Uenv(Env.consolidate(foldr(fn((s,b),e)=>Env.bind(s,b,e)) 
				            Env.empty l)))
        | env _ = raise Fail "    | env"

      val Uenv result = R.root(p0,env)

   in result
  end (* function unPickleEnv *)

end (* local *)
end (* structure UnpickleMod *)

