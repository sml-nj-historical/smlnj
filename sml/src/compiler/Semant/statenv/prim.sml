(* Copyright 1996 by AT&T Bell Laboratories *)
(* prim.sml *)

signature PRIM_ENV = 
sig 
  val primEnv : StaticEnv.staticEnv
end (* signature PRIM_ENV *)


structure PrimEnv : PRIM_ENV = 
struct

local
  structure S = Symbol
  structure M = Modules
  structure B = Bindings
  structure SP = SymPath
  structure IP = InvPath
  structure SE = StaticEnv
  structure EE = EntityEnv

  structure BT = BasicTypes
  structure T = Types
  structure TU = TypesUtil
  structure MU = ModuleUtil

  structure ST = Stamps
  structure V = VarCon

  structure A = Access

in

fun mkTycElement (name: string, tyc) = 
     (S.tycSymbol name, M.TYCspec{entVar=ST.special name, spec=tyc, repl=false,
				  scope=0})

(* 
 * Note: this function only applies to constructors but not exceptions;
 * exceptions will have a non-trivial slot number 
 *)
fun mkConElement (name, d) = 
    (S.varSymbol name, M.CONspec{spec=d, slot=NONE})

(* Below there is a bunch of very long list literals which would create
 * huge register pressure on the compiler.  We construct them backwards
 * using an alternative "cons" that takes its two arguments in opposite
 * order.  This effectively puts the lists' ends to the left and alleviates
 * this effect. (Stupid ML trick No. 21b) (Blume, 1/2001) *)
infix :-:				(* inverse :: *)
fun l :-: e = e :: l

(* primTypes structure *)
val primTypes =
  let val primTycs =
	  [] :-:
             ("bool", BT.boolTycon) :-:
             ("list", BT.listTycon) :-:
             ("ref", BT.refTycon) :-:
             ("unit", BT.unitTycon) :-:
             ("int", BT.intTycon) :-:
             ("int32", BT.int32Tycon) :-:
             ("int64", BT.int64Tycon) :-:
	     ("intinf", BT.intinfTycon) :-:
             ("real", BT.realTycon) :-:
             ("word", BT.wordTycon) :-:
             ("word8", BT.word8Tycon) :-:
             ("word32", BT.word32Tycon) :-:
             ("word64", BT.word64Tycon) :-:
             ("cont", BT.contTycon) :-:
             ("control_cont", BT.ccontTycon) :-:
             ("array", BT.arrayTycon) :-:
             ("vector", BT.vectorTycon) :-:
             ("object", BT.objectTycon) :-:
             ("c_function", BT.c_functionTycon) :-:
             ("word8array", BT.word8arrayTycon) :-:
             ("real64array", BT.real64arrayTycon) :-:
             ("spin_lock", BT.spin_lockTycon) :-:
             ("string", BT.stringTycon) :-:
             ("char", BT.charTycon) :-:
             ("exn", BT.exnTycon) :-:
             ("frag", BT.fragTycon) :-:
             ("susp", BT.suspTycon)

      val primCons = 
          [] :-:
	     ("true", BT.trueDcon) :-:
             ("false", BT.falseDcon) :-:
             ("::", BT.consDcon) :-:
             ("nil", BT.nilDcon) :-:
             ("ref", BT.refDcon) :-:
             ("QUOTE", BT.QUOTEDcon) :-:
             ("ANTIQUOTE", BT.ANTIQUOTEDcon) :-:
             ("$", BT.dollarDcon)

      val tycElements = map mkTycElement primTycs
      val conElements = map mkConElement primCons

      val allElements = tycElements@conElements
      val allSymbols = map #1 allElements

      val entities = let
	  fun f ((_,M.TYCspec{spec,entVar,repl,scope}),r) =
	      EE.bind(entVar,M.TYCent spec,r)
	    | f _ = ErrorMsg.impossible "primTypes:entities"
      in
          foldr f EE.empty tycElements
      end

      val entities = EntityEnv.mark(fn _ => ST.special"primEntEnv", entities)

      val sigrec = 
	  {stamp=ST.special "PrimTypesSig",
	   name=SOME(S.sigSymbol "PRIMTYPES"), closed=true,
	   fctflag=false,
	   symbols=allSymbols,elements=allElements,
	   typsharing=nil,strsharing=nil,
	   properties = PropList.newHolder (),
	   (* boundeps=ref (SOME []), *)
	   (* lambdaty=ref(NONE), *)
	   stub = NONE}
      val _ = ModulePropLists.setSigBoundeps (sigrec, SOME [])
      val strrec =
	  {sign=M.SIG sigrec,
	   rlzn={stamp=ST.special "PrimTypesStr",
		 stub=NONE,
		 entities=entities,
		 properties = PropList.newHolder (),
		 (* lambdaty=ref NONE,  *)
		 rpath=IP.IPATH[S.strSymbol "primTypes"]},
	   access=A.nullAcc, prim= []}
   in M.STR strrec

  end (* primTypes *)


(**************************************************************************
 *                 BUILDING A COMPLETE LIST OF PRIMOPS                    *
 **************************************************************************)

(* We generate unique numbers for each primop, and bind them as components
of a structure InLine, with the generic type all = (All 'a).'a. The primop
intrinsic types will be specified in a separate table used in the translate
phase (and FLINT?).
*)

(*
val v1 = T.IBOUND 0
fun p1 t = T.POLYty {sign=[false], tyfun=T.TYFUN {arity=1, body=t}}
*)
(* the generic type (All 'a).'a *)
val all = T.POLYty {sign=[false], tyfun=T.TYFUN {arity=1, body=T.IBOUND 0}}

val allPrimops =
    ["callcc",
     "throw",
     "capture",
     "isolate",
     "cthrow",
     "!",
     ":=",
     "makeref",
     "boxed",
     "unboxed",
     "cast",
     "=",
     "<>",
     "ptreql",
     "ptrneq",
     "getvar",
     "setvar",
     "setpseudo",
     "getpseudo",
     "mkspecial",
     "getspecial",
     "setspecial",
     "gethdlr",
     "sethdlr",
     "gettag",
     "setmark",
     "dispose",
     "compose",
     "before",
     "ignore",
     "identity",
     "length",
     "objlength",
     "unboxedupdate",
     "inlnot",
     "floor",
     "round",
     "real",
     "real32",
     "ordof",
     "store",
     "inlbyteof",
     "inlstore",
     "inlordof",
     "mkarray",
     "arrSub",
     "arrChkSub",
     "vecSub",
     "vecChkSub",
     "arrUpdate",
     "arrChkUpdate",
     "newArray0",
     "getSeqData",
     "recordSub",
     "raw64Sub",
     "test_32_31_w",
     "test_32_31_i",
     "testu_31_31",
     "testu_32_31",
     "testu_32_32",
     "copy_32_32_ii",
     "copy_32_32_wi",
     "copy_32_32_iw",
     "copy_32_32_ww",
     "copy_31_31_ii",
     "copy_31_31_wi",
     "copy_31_31_iw",
     "copy_31_32_i",
     "copy_31_32_w",
     "copy_8_32_i",
     "copy_8_32_w",
     "copy_8_31",
     "extend_31_32_ii",
     "extend_31_32_iw",
     "extend_31_32_wi",
     "extend_31_32_ww",
     "extend_8_31",
     "extend_8_32_i",
     "extend_8_32_w",
     "trunc_32_31_i",
     "trunc_32_31_w",
     "trunc_31_8",
     "trunc_32_8_i",
     "trunc_32_8_w",
     "test_inf_31",
     "test_inf_32",
     "test_inf_64",
     "copy_8_inf",
     "copy_8_inf_w",
     "copy_31_inf_w",
     "copy_32_inf_w",
     "copy_64_inf_w",
     "copy_31_inf_i",
     "copy_32_inf_i",
     "copy_64_inf_i",
     "extend_8_inf",
     "extend_8_inf_w",
     "extend_31_inf_w",
     "extend_32_inf_w",
     "extend_64_inf_w",
     "extend_31_inf_i",
     "extend_32_inf_i",
     "extend_64_inf_i",
     "trunc_inf_8",
     "trunc_inf_31",
     "trunc_inf_32",
     "trunc_inf_64",
     "w64p",
     "p64w",
     "i64p",
     "p64i",
     "i31add",
     "i31add_8",
     "i31sub",
     "i31sub_8",
     "i31mul",
     "i31mul_8",
     "i31div",
     "i31div_8",
     "i31mod",
     "i31mod_8",
     "i31quot",
     "i31rem",
     "i31orb",
     "i31orb_8",
     "i31andb",
     "i31andb_8",
     "i31xorb",
     "i31xorb_8",
     "i31notb",
     "i31notb_8",
     "i31neg",
     "i31neg_8",
     "i31lshift",
     "i31lshift_8",
     "i31rshift",
     "i31rshift_8",
     "i31lt",
     "i31lt_8",
     "i31lt_c",
     "i31le",
     "i31le_8",
     "i31le_c",
     "i31gt",
     "i31gt_8",
     "i31gt_c",
     "i31ge",
     "i31ge_8",
     "i31ge_c",
     "i31ltu",
     "i31geu",
     "i31eq",
     "i31ne",
     "i31min",
     "i31min_8",
     "i31max",
     "i31max_8",
     "i31abs",
     "i32mul",
     "i32div",
     "i32mod",
     "i32quot",
     "i32rem",
     "i32add",
     "i32sub",
     "i32orb",
     "i32andb",
     "i32xorb",
     "i32lshift",
     "i32rshift",
     "i32neg",
     "i32lt",
     "i32le",
     "i32gt",
     "i32ge",
     "i32eq",
     "i32ne",
     "i32min",
     "i32max",
     "i32abs",
     "f64add",
     "f64sub",
     "f64div",
     "f64mul",
     "f64neg",
     "f64ge",
     "f64gt",
     "f64le",
     "f64lt",
     "f64eq",
     "f64ne",
     "f64abs",
     "f64sin",
     "f64cos",
     "f64tan",
     "f64sqrt",
     "f64min",
     "f64max",
     "f64Sub",
     "f64chkSub",
     "f64Update",
     "f64chkUpdate",
     "w8orb",
     "w8xorb",
     "w8andb",
     "w8gt",
     "w8ge",
     "w8lt",
     "w8le",
     "w8eq",
     "w8ne",
     "w8Sub",
     "w8chkSub",
     "w8subv",
     "w8chkSubv",
     "w8update",
     "w8chkUpdate",
     "w31mul",
     "w31div",
     "w31mod",
     "w31add",
     "w31sub",
     "w31orb",
     "w31xorb",
     "w31andb",
     "w31notb",
     "w31neg",
     "w31rshift",
     "w31rshiftl",
     "w31lshift",
     "w31gt",
     "w31ge",
     "w31lt",
     "w31le",
     "w31eq",
     "w31ne",
     "w31ChkRshift",
     "w31ChkRshiftl",
     "w31ChkLshift",
     "w31min",
     "w31max",
     "w31mul_8",
     "w31div_8",
     "w31mod_8",
     "w31add_8",
     "w31sub_8",
     "w31orb_8",
     "w31xorb_8",
     "w31andb_8",
     "w31notb_8",
     "w31neg_8",
     "w31rshift_8",
     "w31rshiftl_8",
     "w31lshift_8",
     "w31gt_8",
     "w31ge_8",
     "w31lt_8",
     "w31le_8",
     "w31eq_8",
     "w31ne_8",
     "w31ChkRshift_8",
     "w31ChkRshiftl_8",
     "w31ChkLshift_8",
     "w31min_8",
     "w31max_8",
     "w32mul",
     "w32div",
     "w32mod",
     "w32add",
     "w32sub",
     "w32orb",
     "w32xorb",
     "w32andb",
     "w32notb",
     "w32neg",
     "w32rshift",
     "w32rshiftl",
     "w32lshift",
     "w32gt",
     "w32ge",
     "w32lt",
     "w32le",
     "w32eq",
     "w32ne",
     "w32ChkRshift",
     "w32ChkRshiftl",
     "w32ChkLshift",
     "w32min",
     "w32max",
     "raww8l",
     "rawi8l",
     "raww16l",
     "rawi16l",
     "raww32l",
     "rawi32l",
     "rawf32l",
     "rawf64l",
     "raww8s",
     "rawi8s",
     "raww16s",
     "rawi16s",
     "raww32s",
     "rawi32s",
     "rawf32s",
     "rawf64s",
     "rawccall",
     "rawrecord",
     "rawrecord64",
     "rawselectw8",
     "rawselecti8",
     "rawselectw16",
     "rawselecti16",
     "rawselectw32",
     "rawselecti32",
     "rawselectf32",
     "rawselectf64",
     "rawupdatew8",
     "rawupdatei8",
     "rawupdatew16",
     "rawupdatei16",
     "rawupdatew32",
     "rawupdatei32",
     "rawupdatef32",
     "rawupdatef64"]

(* uList structure *)
val uList =
  let val ev = ST.special "uListVar"
      val allElements = 
            [(S.tycSymbol "list", M.TYCspec{spec=BT.ulistTycon,entVar=ev,
					    repl=false,scope=0}),
              mkConElement("nil", BT.unilDcon),
              mkConElement("::", BT.uconsDcon)]
      val allSymbols = map #1 allElements
      val sigrec = {stamp=ST.special "uListSig",
		       name=NONE, closed=true, 
		       fctflag=false,
		       symbols=allSymbols, elements=allElements,
		       typsharing=nil, strsharing=nil,
		       properties = PropList.newHolder (),
		       (* boundeps=ref (SOME []), *)
		       (* lambdaty=ref NONE, *)
		       stub = NONE}
      val _ = ModulePropLists.setSigBoundeps (sigrec, SOME [])
   in M.STR{sign=M.SIG sigrec,
            rlzn={stamp=ST.special "uListStr",
		  stub=NONE,
		  entities=EE.bind(ev,M.TYCent BT.ulistTycon,EE.empty),
		  properties = PropList.newHolder (),
		  (* lambdaty=ref(NONE), *)
		  rpath=IP.IPATH[S.strSymbol "uList"]},
            access=A.nullAcc, prim= []}
  end

(* inLine structure *)
val inLine =
  let val bottom = T.POLYty{sign=[false], 
                            tyfun=T.TYFUN{arity=1,body=T.IBOUND 0}}

      fun mkVarElement(name,(symbols,elements,primElems,offset)) =
        let val s = S.varSymbol name
            val ty = PrimOpTypeMap.primopTypeMap name (* the intrinsic type *)
            val sp = M.VALspec{spec=ty, slot=offset}
                    (* using universal generic type bottom for all components *)
            val p = PrimOpId.PrimE(PrimOpId.Prim name) (* the primop code *)
         in (s::symbols, (s,sp)::elements, p::primElems, offset+1)
        end
      
      val (allSymbols, allElements, primList, _) = 
            foldl mkVarElement ([],[],[],0) allPrimops

      val (allSymbols, allElements, primList) = 
            (rev allSymbols, rev allElements, rev primList)

      val sigrec ={stamp=ST.special "inLineSig",
		   name=NONE, closed=true, 
		   fctflag=false,
		   symbols=allSymbols, elements=allElements,
		   typsharing=nil, strsharing=nil,
		   properties = PropList.newHolder (),  (* dbm: ??? *)
		   stub = NONE}

      val _ = ModulePropLists.setSigBoundeps (sigrec, SOME [])

   in M.STR{sign = M.SIG sigrec,
            rlzn = {stamp=ST.special "inLineStr",
		    stub=NONE,
		    entities=EE.empty,
		    properties = PropList.newHolder (),  (* dbm: ??? *)
		    rpath=IP.IPATH[S.strSymbol "inLine"]},
	    access = A.nullAcc,
            prim = primList}
  end

(* priming structures: PrimTypes and InLine *)
val nameofPT = S.strSymbol "PrimTypes"
val nameofUL = S.strSymbol "UnrolledList"
val nameofIL = S.strSymbol "InLine"

val primEnv =
      SE.bind(nameofIL,B.STRbind inLine,
          SE.bind(nameofUL,B.STRbind uList,
  	     SE.bind(nameofPT,B.STRbind primTypes,
                MU.openStructure(SE.empty,primTypes))))

val primEnv = let
    val { hash, pickle, ... } =
	PickMod.pickleEnv (PickMod.INITIAL ModuleId.emptyTmap) primEnv
in
    UnpickMod.unpickleEnv (fn _ => ModuleId.emptyTmap) (hash, pickle)
end

end (* local *)

end (* structure PrimEnv *)
