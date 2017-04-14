(* Copyright 1996 by AT&T Bell Laboratories *)
(* Copyright 2003 by The SML/NJ Fellowship *)
(* ppmod.sml *)

(* modified to use SML/NJ Lib PP. [dbm, 7/30/03]) *)

signature PPMOD =
sig
  val ppSignature: PrettyPrintNew.stream
        -> Modules.Signature * StaticEnv.staticEnv * int -> unit
  val ppStructure: PrettyPrintNew.stream
        -> Modules.Structure * StaticEnv.staticEnv * int -> unit
  val ppOpen: PrettyPrintNew.stream
        -> SymPath.path * Modules.Structure * StaticEnv.staticEnv * int -> unit
  val ppStructureName : PrettyPrintNew.stream
	-> Modules.Structure * StaticEnv.staticEnv -> unit
  val ppFunctor : PrettyPrintNew.stream
	-> Modules.Functor * StaticEnv.staticEnv * int -> unit
  val ppFunsig : PrettyPrintNew.stream
        -> Modules.fctSig * StaticEnv.staticEnv * int -> unit
  val ppBinding: PrettyPrintNew.stream
	-> Symbol.symbol * Bindings.binding * StaticEnv.staticEnv * int
             -> unit
  val ppEnv : PrettyPrintNew.stream
	      -> StaticEnv.staticEnv * StaticEnv.staticEnv * int *
	         Symbol.symbol list option
	      -> unit

  (* module internals *)

  val ppElements : (StaticEnv.staticEnv * int * Modules.entityEnv option)
                   -> PrettyPrintNew.stream
                   -> Modules.elements -> unit

  val ppEntity : PrettyPrintNew.stream
                 -> Modules.entity * StaticEnv.staticEnv * int
                 -> unit

  val ppEntityEnv : PrettyPrintNew.stream
                    -> Modules.entityEnv * StaticEnv.staticEnv * int
                    -> unit

end (* signature PPMOD *)


structure PPModules : PPMOD =
struct

local structure S = Symbol
      structure SP = SymPath
      structure IP = InvPath
      structure A = Access
      (* structure II = InlInfo *)
      structure T = Types
      structure TU = TypesUtil
      structure BT = BasicTypes
      structure V = VarCon
      structure M = Modules
      structure MU = ModuleUtil
      structure B = Bindings
      structure SE = StaticEnv
      structure EE = EntityEnv
      structure LU = Lookup

      structure PP = PrettyPrintNew
      structure PU = PPUtilNew
      open PrettyPrintNew PPUtilNew

in

val internals = ElabControl.internals
fun bug msg = ErrorMsg.impossible("PPModules: "^msg)
fun C f x y = f y x;

val pps = PP.string
val ppType = PPType.ppType
val ppTycon = PPType.ppTycon
val ppTyfun = PPType.ppTyfun
val ppFormals = PPType.ppFormals

val resultId = S.strSymbol "<resultStr>"

fun strToEnv(M.SIG {elements,...},entities) =
    let fun bindElem ((sym,spec), env) =
	    case spec
              of M.TYCspec{entVar,...} =>
		  let val tyc = EE.lookTycEnt(entities,entVar)
		   in SE.bind(sym,B.TYCbind tyc,env)
		  end
	       | M.STRspec{entVar,sign,...} =>
		  let val strEnt = EE.lookStrEnt(entities,entVar)
		   in SE.bind(sym,B.STRbind(M.STR{sign=sign,rlzn=strEnt,
						  access=A.nullAcc,
						  prim=[]}),
			      env)
		  end
	       | M.CONspec{spec=dcon, ...} => SE.bind(sym,B.CONbind dcon,env)
	       | _ => env
     in foldl bindElem SE.empty elements
    end
  | strToEnv _ = SE.empty

fun sigToEnv(M.SIG {elements,...}) =
    let fun bindElem ((sym,spec), env) =
	  (case spec
            of M.TYCspec{info=M.RegTycSpec{spec,...},...} =>
                SE.bind(sym,B.TYCbind spec,env)
             | M.TYCspec{info=M.InfTycSpec{name,arity},...} =>
                let val tyc =
                        T.GENtyc{stamp=Stamps.special "x", arity=arity,
                                 eq=ref(T.UNDEF), kind=T.FORMAL, stub=NONE,
                                 path=InvPath.extend(InvPath.empty,name)}
                in SE.bind(sym,B.TYCbind tyc,env)
                end
	     | M.STRspec{sign,slot,def,entVar=ev} =>
		 SE.bind(sym,B.STRbind(M.STRSIG{sign=sign,entPath=[ev]}),env)
	     | M.CONspec{spec=dcon, ...} => SE.bind(sym,B.CONbind dcon,env)
	     | _ => env)
     in foldl bindElem SE.empty elements
    end
  | sigToEnv _ = bug "sigToEnv"

(*
 * Support for a hack to make sure that non-visible ConBindings don't
 * cause spurious blank lines when pp-ing signatures.
 *)
fun is_ppable_ConBinding (T.DATACON{rep=A.EXN _, ...}, _) = true
  | is_ppable_ConBinding (con,env) =
      let exception Hidden
	  val visibleDconTyc =
	        let val tyc = TU.dconTyc con
		 in (TU.equalTycon
		      (LU.lookTyc
			 (env,
			  SP.SPATH[IP.last(TU.tycPath tyc)],
			  fn _ => raise Hidden),
		       tyc)
		       handle Hidden => false)
		end
       in (!internals orelse not visibleDconTyc)
      end

fun all_ppable_bindings alist env =
    List.filter (fn (name,B.CONbind con) => is_ppable_ConBinding(con,env)
                  | b => true)
                alist


fun ppLty ppstrm ( (* lambdaty,depth *) ) =  pps ppstrm "<lambdaty>"

fun ppEntVar ppstrm entVar =
    pps ppstrm (EntPath.entVarToString entVar)

fun ppEntPath ppstrm entPath =
    pps ppstrm (EntPath.entPathToString entPath)
(*    ppClosedSequence ppstream
      {front=(fn ppstrm => pps ppstrm "["),
       sep=(fn ppstrm => (pps ppstrm ","; break ppstrm {nsp=0,offset=0})),
       back=(fn ppstrm => pps ppstrm "]"),
       style=INCONSISTENT,
       pr=ppEntVar}
*)

fun ppTycExp ppstrm (tycExp,depth) =
    if depth <= 0 then pps ppstrm "<tycExp>" else
    case tycExp
      of M.VARtyc ep =>
	  (pps ppstrm "TE.V:"; break ppstrm {nsp=1,offset=1};
	   ppEntPath ppstrm ep)
       | M.CONSTtyc tycon =>
	  (pps ppstrm "TE.C:"; break ppstrm {nsp=1,offset=1};
	   ppTycon SE.empty ppstrm tycon)
       | M.FORMtyc tycon =>
	  (pps ppstrm "TE.FM:"; break ppstrm {nsp=1,offset=1};
	   ppTycon SE.empty ppstrm tycon)

fun ppStructureName ppstrm (str,env) =
    let val rpath =
	    case str
	     of M.STR { rlzn, ... } => #rpath rlzn
	      | _ => bug "ppStructureName"
	fun look a = LU.lookStr(env,a,(fn _ => raise StaticEnv.Unbound))
	fun check str' = MU.eqOrigin(str',str)
	val (syms,found) = findPath(rpath,check,look)
     in pps ppstrm (if found then SP.toString(SP.SPATH syms)
		    else "?"^(SP.toString(SP.SPATH syms)))
    end

fun ppVariable ppstrm  =
    let val {openHVBox, openHOVBox,closeBox,pps,...} = en_pp ppstrm
	fun ppV(V.VALvar{path,access,typ,prim,btvs},env:StaticEnv.staticEnv) =
	      (openHVBox 0;
	       pps (SP.toString path);
	       if !internals then PPVal.ppAccess ppstrm access else ();
	       pps " : "; ppType env ppstrm (!typ);
	       closeBox())
	  | ppV (V.OVLDvar {name,options=optl,scheme=T.TYFUN{body,...}},env) =
	      (openHVBox 0;
	       ppSym ppstrm (name); pps " : "; ppType env ppstrm body;
	       pps " as ";
	       ppSequence ppstrm
		 {sep=C PrettyPrintNew.break{nsp=1,offset=0},
		  pr=(fn ppstrm => fn{variant,...} =>ppV(variant,env)),
		  style=CONSISTENT}
		 optl;
	       closeBox())
	  | ppV(V.ERRORvar,_) = pps "<ERRORvar>"
     in ppV
    end

fun ppConBinding ppstrm =
    let val {openHVBox, openHOVBox,closeBox,pps,...} = en_pp ppstrm
	fun ppCon (T.DATACON{name, typ, rep=A.EXN _, ...}, env) =
	      (openHOVBox 4;
	       pps "exception "; ppSym ppstrm name;
               if BasicTypes.isArrowType typ then
                  (pps " of "; ppType env ppstrm (BasicTypes.domain typ))
               else ();
	       closeBox())
	  | ppCon (con as T.DATACON{name,typ,...},env) =
 	      if !internals
 	      then (openHOVBox 4;
 		    pps "datacon "; ppSym ppstrm name; pps " : ";
 		    ppType env ppstrm typ;
 		    closeBox())
 	      else ()
     in ppCon
    end

fun ppStructure ppstrm (str,env,depth) =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline} = en_pp ppstrm
     in case str
	  of M.STR { sign, rlzn as { entities, ... }, prim, ... } =>
	     (if !internals
	      then (openHVBox 2;
		       pps "STR";
		       nl_indent ppstrm 2;
		       openHVBox 0;
			pps "sign:";
			break {nsp=1,offset=2};
			ppSignature0 ppstrm (sign,env,depth-1,SOME entities);
			newline();
		        pps "rlzn:";
			break {nsp=1,offset=2};
			ppStrEntity ppstrm (rlzn,env,depth-1);
			newline();
			pps "prim:";
			break {nsp=1,offset=2};
			(* GK: This should be cleaned up soon so as to use a
			   ppStrInfo that is an actual pretty printer conforming
			   to the pattern of the other pretty printers.
			PrimOpId.ppStrInfo prim; *)
			PPPrim.ppStrPrimInfo ppstrm prim;
		       closeBox();
		      closeBox())
		else case sign
		       of M.SIG { name = SOME sym, ... } =>
			  ((if MU.eqSign
				   (sign,
				    LU.lookSig
					(env,sym,(fn _ => raise SE.Unbound)))
			    then ppSym ppstrm sym
			    else (ppSym ppstrm sym; pps "?"))
			   handle SE.Unbound =>
				  (ppSym ppstrm sym; pps "?"))
			| M.SIG { name = NONE, ... } =>
			  if depth <= 1 then pps "<sig>"
			  else ppSignature0 ppstrm
				            (sign,env,depth-1,SOME entities)
			| M.ERRORsig => pps "<error sig>")
	   | M.STRSIG _ => pps "<strsig>"
	   | M.ERRORstr => pps "<error str>"
    end

and ppElements (env,depth,entityEnvOp) ppstrm elements =
    let fun pr first (sym,spec) =
	   case spec
	     of M.STRspec{sign,entVar,def,slot} =>
		 (if first then () else newline ppstrm;
		  openHVBox ppstrm (PP.Rel 0);
		   pps ppstrm "structure ";
		   ppSym ppstrm sym; pps ppstrm " :";
		   break ppstrm {nsp=1,offset=2};
		   openHVBox ppstrm (PP.Rel 0);
		    case entityEnvOp
		      of NONE => ppSignature0 ppstrm (sign,env,depth-1,NONE)
		       | SOME eenv =>
			  let val {entities,...} =
				  case EE.look(eenv,entVar) of
				      M.STRent e => e
				    | _ => bug "ppElements:STRent"
			   in ppSignature0 ppstrm
			        (sign,env,depth-1,SOME entities)
			  end;
		    if !internals
		    then (newline ppstrm;
			  pps ppstrm "entVar: ";
			  pps ppstrm (EntPath.entVarToString entVar))
		    else ();
		   closeBox ppstrm;
		  closeBox ppstrm)

	      | M.FCTspec{sign,entVar,slot} =>
		 (if first then () else newline ppstrm;
		  openHVBox ppstrm (PP.Rel 0);
		   pps ppstrm "functor ";
		   ppSym ppstrm sym; pps ppstrm " :";
		   break ppstrm {nsp=1,offset=2};
		   openHVBox ppstrm (PP.Rel 0);
		    ppFunsig ppstrm (sign,env,depth-1);
		    if !internals
		    then (newline ppstrm;
			  pps ppstrm "entVar: ";
			  pps ppstrm (EntPath.entVarToString entVar))
		    else ();
		   closeBox ppstrm;
		  closeBox ppstrm)

	      | M.TYCspec{entVar,info} =>
		 (if first then () else newline ppstrm;
                  case info
                    of M.RegTycSpec{spec,repl,scope} =>
		       (openHVBox ppstrm (PP.Rel 0);
                         case entityEnvOp
                          of NONE =>
                             if repl then
                                 ppReplBind ppstrm (spec,env)
                             else ppTycBind ppstrm (spec,env)
                           | SOME eenv =>
                             (case EE.look(eenv,entVar)
                               of M.TYCent tyc =>
                                  if repl then
                                      ppReplBind ppstrm (tyc,env)
                                  else ppTycBind ppstrm (tyc,env)
                                | M.ERRORent => pps ppstrm "<ERRORent>"
                                | _ => bug "ppElements:TYCent");
                         if !internals
                         then (newline ppstrm;
                               pps ppstrm "entVar: ";
                               pps ppstrm (EntPath.entVarToString entVar);
                               newline ppstrm;
                               pps ppstrm "scope: ";
                               pps ppstrm (Int.toString scope))
                         else ();
		        closeBox ppstrm)
                     | M.InfTycSpec{name,arity} =>
                       (openHVBox ppstrm (PP.Rel 0);
                         case entityEnvOp
                           of NONE =>
                               (pps ppstrm "type";
                                ppFormals ppstrm arity;
                                pps ppstrm " ";
                                ppSym ppstrm name)
                            | SOME eenv =>
                               (case EE.look(eenv,entVar)
                                  of M.TYCent tyc =>
                                       ppTycBind ppstrm (tyc,env)
                                   | M.ERRORent => pps ppstrm "<ERRORent>"
                                   | _ => bug "ppElements:TYCent");
                         if !internals
                         then (newline ppstrm;
                               pps ppstrm "entVar: ";
                               pps ppstrm (EntPath.entVarToString entVar))
                         else ();
                        closeBox ppstrm))

	      | M.VALspec{spec=typ,...} =>
		 (if first then () else newline ppstrm;
		  openHOVBox ppstrm (PP.Rel 4);
		   pps ppstrm "val ";
		   ppSym ppstrm sym; pps ppstrm " : ";
		   ppType env ppstrm (typ);
		  closeBox ppstrm)

	      | M.CONspec{spec=dcon as T.DATACON{rep=A.EXN _,...}, ...} =>
		 (if first then () else newline ppstrm;
	          ppConBinding ppstrm (dcon,env))

              | M.CONspec{spec=dcon,...} =>
 		 if !internals
 		 then (if first then () else newline ppstrm;
 		       ppConBinding ppstrm (dcon,env))
 		 else () (* don't pring ordinary data constructor,
                          * because it was printed with its datatype *)

     in openHVBox ppstrm (PP.Rel 0);
	case elements
          of nil => ()
	   | first :: rest => (pr true first; app (pr false) rest);
	closeBox ppstrm
    end

and ppSignature0 ppstrm (sign,env,depth,entityEnvOp) =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline} =
            en_pp ppstrm
	val env = SE.atop(case entityEnvOp
			    of NONE => sigToEnv sign
			     | SOME entEnv => strToEnv(sign,entEnv),
			  env)
	fun ppConstraints (variety,constraints : M.sharespec list) =
		(openHVBox 0;
		 ppvseq ppstrm 0 ""
		  (fn ppstrm => fn paths =>
		      (openHOVBox 2;
			pps "sharing "; pps variety;
			ppSequence ppstrm
			 {sep=(fn ppstrm =>
				(pps " ="; break{nsp=1,offset=0})),
			  pr=ppSymPath,
			  style=INCONSISTENT}
			 paths;
		       closeBox()))
		  constraints;
		closeBox ())
	val somePrint = ref false (* i.e., signature is not empty sig end *)
     in if depth <= 0
	then pps "<sig>"
	else
	case sign
	  of M.SIG {stamp,name,elements,typsharing,strsharing,...} =>
	     let
		 (* Filter out ordinary dcon that do not print in ppElements
		    for element printing so that we do not print the spurious
	            newline. We still use the unfiltered elements
		    for determining whether the sig ... end should be
		    multiline even with just one datatype. *)
                val elems' =
		    List.filter
		    (fn (_,M.CONspec{spec=T.DATACON{rep=A.EXN _,...},...})
			    => true
		      | (_,M.CONspec{spec=dcon,...}) => false
		      | _ => true)
		    elements
	     in
	     if !internals then
	       (openHVBox 0;
		 pps "SIG:";
		 nl_indent ppstrm 2;
		 openHVBox 0;
		  pps "stamp: "; pps (Stamps.toShortString stamp);
		  newline();
		  pps "name: ";
		  case name
		    of NONE => pps "ANONYMOUS"
		     | SOME p => (pps "NAMED "; ppSym ppstrm p);
		  case elements
		    of nil => ()
		     | _ => (newline(); pps "elements:";
			     nl_indent ppstrm 2;
			     ppElements (env,depth,entityEnvOp) ppstrm elements);
		  case strsharing
                    of nil => ()
		     | _ => (newline(); pps "strsharing:";
			     nl_indent ppstrm 2;
			     ppConstraints("",strsharing));
		  case typsharing
                    of nil => ()
		     | _ => (newline(); pps "tycsharing:";
			     nl_indent ppstrm 2;
			     ppConstraints("type ",typsharing));
		 closeBox();
		closeBox())
	      else (* not !internals *)
		(openHVBox 0;
		  pps "sig";
		  (case elements
		       of nil => pps " "
			| [(_,M.STRspec _)] => nl_indent ppstrm 2
			| [_] => pps " "
			| _ => nl_indent ppstrm 2);
		  openHVBox 0;
		   case elements
		     of nil => ()
		      | _ => (ppElements (env,depth,entityEnvOp) ppstrm elems';
			      somePrint := true);
		   case strsharing
		     of nil => ()
		      | _ => (if !somePrint then newline() else ();
			      ppConstraints("",strsharing);
			      somePrint := true);
		   case typsharing
		     of nil => ()
		      | _ => (if !somePrint then newline() else ();
			      ppConstraints("type ",typsharing);
			      somePrint := true);
		  closeBox();
		  (case elements
		    of nil => ()
		     | [(_,M.STRspec _)] => newline()
		     | [_] => pps " "
		     | _ => newline());
		  pps "end";
		 closeBox())
	     end
	   | M.ERRORsig => pps "<error sig>"
    end

and ppFunsig ppstrm (sign,env,depth) =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline} =
            en_pp ppstrm
	fun trueBodySig (orig as M.SIG { elements =
					 [(sym, M.STRspec { sign, ... })],
					 ... }) =
	    if Symbol.eq (sym, resultId) then sign else orig
	  | trueBodySig orig = orig
     in if depth<=0 then pps "<fctsig>"
	else case sign
	       of M.FSIG {paramsig,paramvar,paramsym,bodysig, ...} =>
		   if !internals
		   then (openHVBox 0;
			  pps "FSIG:";
			  nl_indent ppstrm 2;
			  openHVBox 0;
			   pps "psig: ";
			   ppSignature0 ppstrm (paramsig,env,depth-1,NONE);
			   newline();
			   pps "pvar: ";
			   pps (EntPath.entVarToString paramvar);
			   newline();
			   pps "psym: ";
			   (case paramsym
			      of NONE => pps "<anonymous>"
			       | SOME sym => ppSym ppstrm sym);
			   newline();
			   pps "bsig: ";
			   ppSignature0 ppstrm (bodysig,env,depth-1,NONE);
			  closeBox();
			 closeBox())
		   else (openHVBox 0;
			  pps "(";
                          case paramsym
			    of SOME x => pps (S.name x)
			     | _ => pps "<param>";
			  pps ": ";
			  ppSignature0 ppstrm (paramsig,env,depth-1,NONE);
			  pps ") :";
			  break{nsp=1,offset=0};
			  ppSignature0 ppstrm
			    (trueBodySig bodysig,env,depth-1,NONE);
			 closeBox())
		| M.ERRORfsig => pps "<error fsig>"
    end

and ppStrEntity ppstrm (e,env,depth) =
    let val {stamp,entities,properties,rpath,stub} = e
	val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline} =
            en_pp ppstrm
     in if depth <= 1
	then pps "<structure entity>"
	else (openHVBox 0;
	       pps "strEntity:";
	       nl_indent ppstrm 2;
	       openHVBox 0;
		pps "rpath: ";
		pps (IP.toString rpath);
		newline();
		pps "stamp: ";
		pps (Stamps.toShortString stamp);
		newline();
		pps "entities:";
		nl_indent ppstrm 2;
		ppEntityEnv ppstrm (entities,env,depth-1);
		newline();
		pps "lambdaty:";
		nl_indent ppstrm 2;
	       closeBox ();
	      closeBox ())
    end

and ppFctEntity ppstrm (e, env, depth) =
    let val {stamp,closure,properties,tycpath,rpath,stub} = e
	val {openHVBox,openHOVBox,closeBox,pps,ppi,break,newline} = en_pp ppstrm
    in if depth <= 1
	then pps "<functor entity>"
	else (openHVBox 0;
	       pps "fctEntity:";
	       nl_indent ppstrm 2;
	       openHVBox 0;
		pps "rpath: ";
		pps (IP.toString rpath);
		newline();
		pps "stamp: ";
		pps (Stamps.toShortString stamp);
		newline();
		pps "closure:";
		break{nsp=1,offset=2};
		ppClosure ppstrm (closure,depth-1);
		newline();
		pps "lambdaty:";
		break{nsp=1,offset=2};
		pps "tycpath:";
		break{nsp=1,offset=2};
		pps "--printing of tycpath not implemented yet--";
	       closeBox ();
	      closeBox ())
    end

and ppFunctor ppstrm =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline} = en_pp ppstrm
	fun ppF (M.FCT { sign, rlzn, ... }, env, depth) =
		if depth <= 1
		then pps "<functor>"
		else (openHVBox 0;
		      pps "sign:";
		      nl_indent ppstrm 2;
		      ppFunsig ppstrm (sign,env,depth-1);
		      newline();
		      pps "rlzn:";
		      nl_indent ppstrm 2;
		      ppFctEntity ppstrm (rlzn,env,depth-1);
		      closeBox ())
	  | ppF (M.ERRORfct,_,_) = pps "<error functor>"
     in ppF
    end

and ppTycBind ppstrm (tyc,env) =
    let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline} = en_pp ppstrm
        fun visibleDcons(tyc,dcons) =
	    let fun checkCON(V.CON c) = c
		  | checkCON _ = raise SE.Unbound
		fun find ((actual as {name,rep,domain}) :: rest) =
		     (let val found =
			      checkCON(LU.lookValSym
					(env,name,
					 fn _ => raise SE.Unbound))
		       in (* test whether the datatypes of actual and
			     found constructor agree *)
			  case TU.dconTyc found
			    of tyc1 as T.GENtyc _ =>
			       (* the expected form in structures *)
				  if TU.eqTycon(tyc,tyc1)
				  then found :: find rest
				  else find rest
			     | T.PATHtyc _ =>
			       (* the expected form in signatures;
				  we won't check visibility [dbm] *)
			       found :: find rest
			     | d_found =>
			       (* something's weird *)
				 let val old_internals = !internals
				  in internals := true;
				     openHVBox 0;
				      pps "ppTycBind failure: ";
				      newline();
				      ppTycon env ppstrm tyc;
				      newline();
				      ppTycon env ppstrm d_found;
				      newline();
				     closeBox();
				     internals := old_internals;
				     find rest
				 end
		      end
		      handle SE.Unbound => find rest)
		  | find [] = []
	     in find dcons
	    end
	fun stripPoly(T.POLYty{tyfun=T.TYFUN{body,...},...}) = body
	  | stripPoly ty = ty
	fun ppDcon (T.DATACON{name,typ,...}) =
	    (ppSym ppstrm name;
	     let val typ = stripPoly typ
	      in if BT.isArrowType typ
		 then (pps " of "; ppType env ppstrm (BT.domain typ))
		 else ()
	     end)
     in if !internals
	then (openHVBox 0;
	       pps "type "; ppTycon env ppstrm tyc;
	      closeBox())
	else
	    case tyc of
		T.GENtyc { path, arity, eq, kind, ... } =>
		(case (!eq, kind) of
		     (T.ABS, _) =>
		     (* abstype *)
		     (openHVBox 0;
		      pps "type";
		      ppFormals ppstrm arity;
		      pps " ";
		      ppSym ppstrm (IP.last path);
		      closeBox())
		   | (_, T.DATATYPE{index,family={members,...},...}) =>
		     (* ordinary datatype *)
		     let val {dcons,...} = Vector.sub(members,index)
			 val visdcons = visibleDcons(tyc,dcons)
			 val incomplete = length visdcons < length dcons
		     in
			 openHVBox 0;
			 pps "datatype";
			 ppFormals ppstrm arity;
			 pps " ";
			 ppSym ppstrm (IP.last path);
			 case visdcons
			   of nil => pps " = ..."
			    | first :: rest =>
			       (break{nsp=1,offset=2};
				openHVBox 0;
				 pps "= "; ppDcon first;
				 app (fn d => (break{nsp=1,offset=0};
                                               pps "| "; ppDcon d))
				     rest;
				 if incomplete
				     then (break{nsp=1,offset=0}; pps "... ")
				 else ();
				closeBox());
			closeBox()
		    end
		   | _ =>
		     (openHVBox 0;
		      if EqTypes.isEqTycon tyc
		      then pps "eqtype"
		      else pps "type";
		      ppFormals ppstrm arity;
		      pps " ";
		      ppSym ppstrm (IP.last path);
		      closeBox()))
	      | T.DEFtyc{path,tyfun=T.TYFUN{arity,body},...} =>
		(openHOVBox 2;
		 pps "type";
		 ppFormals ppstrm arity;
		 break{nsp=1,offset=0};
		 ppSym ppstrm (InvPath.last path);
		 pps " =";
		 break{nsp=1,offset=0};
		 ppType env ppstrm body;
		 closeBox ())
	      | T.ERRORtyc =>
		(pps "ERRORtyc")
	      | T.PATHtyc _ =>
		(pps "PATHtyc:";
		 ppTycon env ppstrm tyc)
	      | tycon =>
		(pps "strange tycon: ";
		 ppTycon env ppstrm tycon)
    end (* ppTycBind *)

and ppReplBind ppstrm =
    let
	val {openHVBox, openHOVBox,closeBox,pps,ppi,break,newline} =
	      en_pp ppstrm
    in
        fn (T.DEFtyc{tyfun=T.TYFUN{body=T.CONty(rightTyc,_),...},path,...},
	    env) =>
	   (* [GK 5/4/07] Does this case ever occur? All datatype
	      replication tycs are GENtycs after elaboration *)
	   (openHOVBox 2;
            pps "datatype"; break{nsp=1,offset=0};
            ppSym ppstrm (IP.last path);
            pps " ="; break{nsp=1,offset=0};
            pps "datatype"; break{nsp=1,offset=0};
            ppTycon env ppstrm rightTyc;
            closeBox ())
	 | (tyc as T.GENtyc{stamp, arity, eq, kind, path, stub}, env) =>
	   (openHOVBox 2;
	    pps "datatype"; break{nsp=1,offset=0};
	    ppSym ppstrm (IP.last path);
	    pps " ="; break{nsp=1,offset=0};
	    ppTycBind ppstrm (tyc, env);
	    closeBox())
	 | (T.PATHtyc _, _) => ErrorMsg.impossible "<replbind:PATHtyc>"
	 | (T.RECtyc _, _) => ErrorMsg.impossible "<replbind:RECtyc>"
	 | (T.FREEtyc _, _) => ErrorMsg.impossible "<replbind:FREEtyc>"
	 | _ => ErrorMsg.impossible "ppReplBind"
    end (* fun ppReplBind *)

and ppEntity ppstrm (entity,env,depth) =
    case entity
      of M.TYCent tycon => ppTycon env ppstrm tycon
       | M.STRent strEntity => ppStrEntity ppstrm (strEntity,env,depth-1)
       | M.FCTent fctEntity => ppFctEntity ppstrm (fctEntity,env,depth-1)
       | M.ERRORent => pps ppstrm "ERRORent"

and ppEntityEnv ppstrm (entEnv,env,depth) =
    if depth <= 1
    then pps ppstrm "<entityEnv>"
    else (ppvseq ppstrm 2 ""
	      (fn ppstrm => fn (entVar,entity) =>
		let val {openHVBox,openHOVBox,closeBox,pps,ppi,break,newline} =
			 en_pp ppstrm
		 in openHVBox 2;
		     pps (EntPath.entVarToString entVar);
		     pps ":";
		     nl_indent ppstrm 2;
		     ppEntity ppstrm (entity,env,depth-1);
		     newline();
		    closeBox()
		end)
	  (EE.toList entEnv))

and ppEntDec ppstrm (entDec,depth) =
    if depth <= 0 then pps ppstrm "<entDec>"
    else case entDec
	  of M.TYCdec(entVar,tycExp) =>
	      (pps ppstrm "ED.T: ";
	       ppEntVar ppstrm entVar; break ppstrm {nsp=1,offset=1};
	       ppTycExp ppstrm (tycExp,depth-1))
	   | M.STRdec(entVar,strExp,sym) =>
	      (pps ppstrm "ED.S: ";
	       ppEntVar ppstrm entVar; break ppstrm {nsp=1,offset=1};
	       ppStrExp ppstrm (strExp,depth-1); break ppstrm {nsp=1,offset=1};
	       ppSym ppstrm sym)
	   | M.FCTdec(entVar,fctExp) =>
	      (pps ppstrm "ED.F: ";
	       ppEntVar ppstrm entVar; break ppstrm {nsp=1,offset=1};
	       ppFctExp ppstrm (fctExp,depth-1))
	   | M.SEQdec entityDecs =>
	      ppvseq ppstrm 0 ""
	        (fn ppstrm => fn entDec => ppEntDec ppstrm (entDec,depth))
		entityDecs
	   | M.LOCALdec(entityDecL,entityDecB) => pps ppstrm "ED.L:"
	   | M.ERRORdec => pps ppstrm "ED.ER:"
	   | M.EMPTYdec => pps ppstrm "ED.EM:"

and ppStrExp ppstrm (strExp,depth) =
    if depth <= 0 then pps ppstrm "<strExp>" else
    case strExp
      of M.VARstr ep =>
	  (pps ppstrm "SE.V:"; break ppstrm {nsp=1,offset=1};
           ppEntPath ppstrm ep)
       | M.CONSTstr { stamp, rpath, ... } =>
	 (pps ppstrm "SE.C:"; break ppstrm {nsp=1,offset=1};
	  ppInvPath ppstrm rpath)
       | M.STRUCTURE{stamp,entDec} =>
	  (pps ppstrm "SE.S:"; break ppstrm {nsp=1,offset=1};
	   ppEntDec ppstrm (entDec,depth-1))
       | M.APPLY(fctExp,strExp) =>
	  (openHVBox ppstrm (PP.Rel 0);
	    pps ppstrm "SE.AP:"; break ppstrm {nsp=1,offset=1};
	    openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "fct:"; ppFctExp ppstrm (fctExp, depth -1);
	     break ppstrm {nsp=1,offset=0};
	     pps ppstrm "arg:"; ppStrExp ppstrm (strExp, depth -1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.LETstr(entDec,strExp) =>
	  (openHVBox ppstrm (PP.Rel 0);
           pps ppstrm "SE.L:"; break ppstrm {nsp=1,offset=1};
           openHVBox ppstrm (PP.Rel 0);
	   pps ppstrm "let:"; ppEntDec ppstrm (entDec,depth-1);
           break ppstrm {nsp=1,offset=0};
           pps ppstrm "in:"; ppStrExp ppstrm (strExp, depth -1);
           closeBox ppstrm;
	   closeBox ppstrm)
       | M.ABSstr(sign,strExp) =>
          (openHVBox ppstrm (PP.Rel 0);
           pps ppstrm "SE.AB:"; break ppstrm {nsp=1,offset=1};
            openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "sign: <omitted>";
	     break ppstrm {nsp=1,offset=0};
	     pps ppstrm "sexp:"; ppStrExp ppstrm (strExp, depth -1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.CONSTRAINstr{boundvar,raw,coercion} =>
          (openHVBox ppstrm (PP.Rel 0);
           pps ppstrm "SE.CO:"; break ppstrm {nsp=1,offset=1};
            openHVBox ppstrm (PP.Rel 0);
             ppEntVar ppstrm boundvar; break ppstrm {nsp=1,offset=1};
	     pps ppstrm "src:"; ppStrExp ppstrm (raw, depth -1);
	     break ppstrm {nsp=1,offset=0};
	     pps ppstrm "tgt:"; ppStrExp ppstrm (coercion, depth -1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.FORMstr(sign) => pps ppstrm "SE.FM:"

and ppFctExp ppstrm (fctExp,depth) =
    if depth <= 0 then pps ppstrm "<fctExp>" else
    case fctExp
      of M.VARfct ep =>
	  (pps ppstrm "FE.V:"; ppEntPath ppstrm ep)
       | M.CONSTfct { rpath, ... } =>
	  (pps ppstrm "FE.C:"; ppInvPath ppstrm rpath)
       | M.LAMBDA_TP {param, body, ...} =>
	  (openHVBox ppstrm (PP.Rel 0);
	    pps ppstrm "FE.LP:"; break ppstrm {nsp=1,offset=1};
	    openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "par:"; ppEntVar ppstrm param;
	     break ppstrm {nsp=1,offset=0};
	     pps ppstrm "bod:"; ppStrExp ppstrm (body, depth-1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.LAMBDA {param, body} =>
	  (openHVBox ppstrm (PP.Rel 0);
	    pps ppstrm "FE.L:"; break ppstrm {nsp=1,offset=1};
	    openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "par:"; ppEntVar ppstrm param;
	     break ppstrm {nsp=1,offset=0};
	     pps ppstrm "bod:"; ppStrExp ppstrm (body, depth-1);
	    closeBox ppstrm;
	   closeBox ppstrm)
       | M.LETfct (entDec,fctExp) =>
          (openHVBox ppstrm (PP.Rel 0);
            pps ppstrm "FE.LT:"; break ppstrm {nsp=1,offset=1};
            openHVBox ppstrm (PP.Rel 0);
  	     pps ppstrm "let:"; ppEntDec ppstrm (entDec,depth-1);
             break ppstrm {nsp=1,offset=0};
             pps ppstrm "in:"; ppFctExp ppstrm (fctExp, depth -1);
            closeBox ppstrm;
	   closeBox ppstrm)

(*
and ppBodyExp ppstrm (bodyExp,depth) =
    if depth <= 0 then pps ppstrm "<bodyExp>" else
    case bodyExp
      of M.FLEX sign => pps ppstrm "BE.F:"
       | M.OPAQ (sign,strExp) =>
	   (openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "BE.O:"; break ppstrm {nsp=1,offset=1};
	     ppStrExp ppstrm (strExp,depth-1);
	    closeBox ppstrm)
       | M.TNSP (sign,strExp) =>
	   (openHVBox ppstrm (PP.Rel 0);
	     pps ppstrm "BE.T:"; break ppstrm {nsp=1,offset=1};
	     ppStrExp ppstrm (strExp,depth-1);
	    closeBox ppstrm)

*)

and ppClosure ppstrm (M.CLOSURE{param,body,env},depth) =
    let val {openHVBox, openHOVBox,closeBox,pps,newline,break,...} = en_pp ppstrm
     in openHVBox 0;
	 pps "CL:"; break{nsp=1,offset=1};
	  openHVBox 0;
	   pps "param: "; ppEntVar ppstrm param; newline();
	   pps "body: "; ppStrExp ppstrm (body,depth-1); newline();
           pps "env: "; ppEntityEnv ppstrm (env,SE.empty,depth-1);
	  closeBox();
	closeBox()
    end

(* assumes no newline is needed before pping *)
and ppBinding ppstrm (name,binding:B.binding,env:SE.staticEnv,depth:int) =
    case binding
      of B.VALbind var => (pps ppstrm "val "; ppVariable ppstrm (var,env))
       | B.CONbind con => ppConBinding ppstrm (con,env)
       | B.TYCbind tycon => ppTycBind ppstrm (tycon,env)
       | B.SIGbind sign =>
	  let val {openHVBox,openHOVBox,closeBox,pps,ppi,break,...} = en_pp ppstrm
	   in openHVBox 0;
	       pps "signature "; ppSym ppstrm name; pps " =";
	       break{nsp=1,offset=2};
	       ppSignature0 ppstrm (sign,env,depth,NONE);
	      closeBox()
	  end
       | B.FSGbind fs =>
	  let val {openHVBox,openHOVBox,closeBox,pps,...} = en_pp ppstrm
	   in openHVBox 2;
	       pps "funsig "; ppSym ppstrm name;
	       ppFunsig ppstrm (fs,env,depth);
	      closeBox()
	  end
       | B.STRbind str =>
	  let val {openHVBox, openHOVBox,closeBox,pps,ppi,break,...} = en_pp ppstrm
	   in openHVBox 0;
	       pps "structure "; ppSym ppstrm name; pps " :";
	       break{nsp=1,offset=2};
	       ppStructure ppstrm (str,env,depth);
	      closeBox()
	  end
       | B.FCTbind fct =>
	  let val {openHVBox,openHOVBox,closeBox,pps,...} = en_pp ppstrm
	   in openHVBox 0;
	       pps "functor ";
	       ppSym ppstrm name;
	       pps " : <sig>";  (* DBM -- should print the signature *)
	      closeBox()
	  end
       | B.FIXbind fixity =>
	  (pps ppstrm (Fixity.fixityToString fixity); ppSym ppstrm name)

(* ppEnv: pp an environment in the context of the top environment.
   The environment must either be for a signature or be absolute (i.e.
   all types and structures have been interpreted) *)
(* Note: I make a preliminary pass over bindings to remove
         invisible ConBindings -- Konrad.
	 and invisible structures too -- PC *)
and ppEnv ppstrm (env,topenv,depth,boundsyms) =
    let val bindings =
	    case boundsyms
	      of NONE => SE.sort env
	       | SOME l => foldr (fn (x,bs) =>
				    ((x,SE.look(env,x))::bs
				     handle SE.Unbound => bs))
				[] l
	val pp_env = StaticEnv.atop(env,topenv)
     in ppSequence ppstrm
	  {sep=newline,
	   pr=(fn ppstrm => fn (name,binding) =>
	          ppBinding ppstrm (name,binding,pp_env,depth)),
	   style=CONSISTENT}
	  (all_ppable_bindings bindings pp_env)
    end

fun ppOpen ppstrm (path,str,env,depth) =
    let val {openHVBox,openHOVBox,closeBox,pps,ppi,break,newline} = en_pp ppstrm
     in openHVBox 0;
	 openHVBox 2;
	  pps "opening ";
	  ppSymPath ppstrm path;
	  if depth < 1 then ()
          else (case str
		  of M.STR { sign, rlzn as {entities,...}, ... } =>
		     (case sign
			 of M.SIG {elements = [],...} => ()
			  | M.SIG {elements,...} =>
			    (newline ();
			     openHVBox 0;
			     ppElements (SE.atop(sigToEnv sign, env),
					 depth,SOME entities)
				        ppstrm elements;
			     closeBox ())
			  | M.ERRORsig => ())
		   | M.ERRORstr => ()
		   | M.STRSIG _ => bug "ppOpen");
         closeBox ();
         newline();
        closeBox ()
    end

fun ppSignature ppstrm (sign,env,depth) =
    ppSignature0 ppstrm (sign,env,depth,NONE)

end (* local *)
end (* structure PPModules *)
