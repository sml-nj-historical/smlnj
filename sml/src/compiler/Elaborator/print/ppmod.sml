(* Copyright 1996 by AT&T Bell Laboratories *)
(* ppmod.sml *)

signature PPMOD = 
sig
  val ppSignature: PrettyPrint.ppstream 
        -> Modules.Signature * StaticEnv.staticEnv * int -> unit
  val ppStructure: PrettyPrint.ppstream
        -> Modules.Structure * StaticEnv.staticEnv * int -> unit
  val ppOpen: PrettyPrint.ppstream
        -> SymPath.path * Modules.Structure * StaticEnv.staticEnv * int -> unit
  val ppStructureName : PrettyPrint.ppstream
	-> Modules.Structure * StaticEnv.staticEnv -> unit
  val ppFunctor : PrettyPrint.ppstream
	-> Modules.Functor * StaticEnv.staticEnv * int -> unit
  val ppFunsig : PrettyPrint.ppstream
        -> Modules.fctSig * StaticEnv.staticEnv * int -> unit
  val ppBinding: PrettyPrint.ppstream 
	-> Symbol.symbol * Bindings.binding * StaticEnv.staticEnv * int
             -> unit
  val ppEnv : PrettyPrint.ppstream
	      -> StaticEnv.staticEnv * StaticEnv.staticEnv * int *
	         Symbol.symbol list option
	      -> unit

  (* module internals *)

  val ppElements : (StaticEnv.staticEnv * int * Modules.entityEnv option)
                   -> PrettyPrint.ppstream
                   -> Modules.elements -> unit

  val ppEntity : PrettyPrint.ppstream
                 -> Modules.entity * StaticEnv.staticEnv * int
                 -> unit

  val ppEntityEnv : PrettyPrint.ppstream
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
     
      structure PP = PrettyPrint
      open PrettyPrint PPUtil

in 

val internals = ElabControl.internals
fun bug msg = ErrorMsg.impossible("PPModules: "^msg)
fun C f x y = f y x;

val pps = PP.add_string
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
						  info=II.Null}),
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
            of M.TYCspec{spec,...} => SE.bind(sym,B.TYCbind spec,env)
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


fun ppLty ppstrm ( (* lambdaty,depth *) ) =  add_string ppstrm "<lambdaty>"

fun ppEntVar ppstrm entVar = 
    add_string ppstrm (EntPath.entVarToString entVar)

fun ppEntPath ppstrm entPath = 
    add_string ppstrm (EntPath.entPathToString entPath)
(*    ppClosedSequence ppstream 
      {front=(fn pps => add_string pps "["),
       sep=(fn pps => (add_string pps ","; add_break pps (0,0))),
       back=(fn pps => add_string pps "]"),
       style=INCONSISTENT,
       pr=ppEntVar}
*)

fun ppTycExp ppstrm (tycExp,depth) =
    if depth <= 0 then add_string ppstrm "<tycExp>" else
    case tycExp
      of M.VARtyc ep =>
	  (add_string ppstrm "TE.V:"; add_break ppstrm (1,1);
	   ppEntPath ppstrm ep)
       | M.CONSTtyc tycon => 
	  (add_string ppstrm "TE.C:"; add_break ppstrm (1,1);
	   ppTycon SE.empty ppstrm tycon)
       | M.FORMtyc tycon =>
	  (add_string ppstrm "TE.FM:"; add_break ppstrm (1,1);
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
    let val {begin_block,end_block,pps,...} = en_pp ppstrm
	fun ppV(V.VALvar{path,access,typ,info},env:StaticEnv.staticEnv) = 
	      (begin_block CONSISTENT 0;
	       pps(SP.toString path);
	       if !internals then PPVal.ppAccess ppstrm access else ();
	       pps " : "; ppType env ppstrm (!typ);
	       end_block())
	  | ppV (V.OVLDvar {name,options=ref optl,scheme=T.TYFUN{body,...}},env) =
	      (begin_block CONSISTENT 0;
	       ppSym ppstrm (name); pps " : "; ppType env ppstrm body; 
	       pps " as ";
	       ppSequence ppstrm
		 {sep=C PrettyPrint.add_break(1,0),
		  pr=(fn ppstrm => fn{variant,...} =>ppV(variant,env)),
		  style=CONSISTENT}
		 optl;
	       end_block())
	  | ppV(V.ERRORvar,_) = pps "<ERRORvar>"
     in ppV
    end

fun ppConBinding ppstrm =
    let val {begin_block,end_block,pps,...} = en_pp ppstrm
	fun ppCon (T.DATACON{name, typ, rep=A.EXN _, ...}, env) =
	      (begin_block INCONSISTENT 4;
	       pps "exception "; ppSym ppstrm name; 
               if BasicTypes.isArrowType typ then
                  (pps " of "; ppType env ppstrm (BasicTypes.domain typ))
               else ();
	       end_block())
	  | ppCon (con as T.DATACON{name,typ,...},env) = 
 	      if !internals
 	      then (begin_block INCONSISTENT 4;
 		    pps "datacon "; ppSym ppstrm name; pps " : ";
 		    ppType env ppstrm typ;
 		    end_block())
 	      else ()
     in ppCon
    end

fun ppStructure ppstrm (str,env,depth) =
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
     in case str
	  of M.STR { sign, rlzn as { entities, ... }, ... } =>
	     (if !internals 
	      then (begin_block CONSISTENT 2;
		       pps "STR";
		       nl_indent ppstrm 2;
		       begin_block CONSISTENT 0;
			pps "sign:";
			add_break (1,2);
			ppSignature0 ppstrm (sign,env,depth-1,SOME entities);
			add_newline();
		        pps "rlzn:";
			add_break (1,2);
			ppStrEntity ppstrm (rlzn,env,depth-1);
		       end_block();
		      end_block())
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
		 (if first then () else add_newline ppstrm;
		  begin_block ppstrm CONSISTENT 0;
		   add_string ppstrm "structure ";
		   ppSym ppstrm sym; add_string ppstrm " :";
		   add_break ppstrm (1,2);
		   begin_block ppstrm CONSISTENT 0;
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
		    then (add_newline ppstrm;
			  add_string ppstrm "entVar: ";
			  add_string ppstrm (EntPath.entVarToString entVar))
		    else ();
		   end_block ppstrm;
		  end_block ppstrm)

	      | M.FCTspec{sign,entVar,slot} => 
		 (if first then () else add_newline ppstrm;
		  begin_block ppstrm CONSISTENT 0;
		   add_string ppstrm "functor ";
		   ppSym ppstrm sym; add_string ppstrm " :";
		   add_break ppstrm (1,2);
		   begin_block ppstrm CONSISTENT 0;
		    ppFunsig ppstrm (sign,env,depth-1);
		    if !internals
		    then (add_newline ppstrm;
			  add_string ppstrm "entVar: ";
			  add_string ppstrm (EntPath.entVarToString entVar))
		    else ();
		   end_block ppstrm;
		  end_block ppstrm)

	      | M.TYCspec{spec,entVar,repl,scope} => 
		 (if first then () else add_newline ppstrm;
		  begin_block ppstrm CONSISTENT 0;
		   case entityEnvOp
		     of NONE => ppTycBind ppstrm (spec,env)
		      | SOME eenv =>
			 (case EE.look(eenv,entVar)
			    of M.TYCent tyc => ppTycBind ppstrm (tyc,env)
			     | M.ERRORent => add_string ppstrm "<ERRORent>"
			     | _ => bug "ppElements:TYCent");
		   if !internals
		   then (add_newline ppstrm;
			 add_string ppstrm "entVar: ";
			 add_string ppstrm (EntPath.entVarToString entVar);
			 add_newline ppstrm;
			 add_string ppstrm "scope: ";
			 add_string ppstrm (Int.toString scope))
		   else ();
		  end_block ppstrm)

	      | M.VALspec{spec=typ,...} =>
		 (if first then () else add_newline ppstrm;
		  begin_block ppstrm INCONSISTENT 4;
		   add_string ppstrm "val ";
		   ppSym ppstrm sym; add_string ppstrm " : ";
		   ppType env ppstrm (typ);
		  end_block ppstrm)

	      | M.CONspec{spec=dcon as T.DATACON{rep=A.EXN _,...}, ...} =>
		 (if first then () else add_newline ppstrm;
	          ppConBinding ppstrm (dcon,env))

              | M.CONspec{spec=dcon,...} => 
 		 if !internals
 		 then (if first then () else add_newline ppstrm;
 		       ppConBinding ppstrm (dcon,env))
 		 else () (* ordinary data constructor, don't print *)

     in begin_block ppstrm CONSISTENT 0;
	case elements
          of nil => ()
	   | first :: rest => (pr true first; app (pr false) rest);
	end_block ppstrm
    end

and ppSignature0 ppstrm (sign,env,depth,entityEnvOp) = 
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
	val env = SE.atop(case entityEnvOp
			    of NONE => sigToEnv sign
			     | SOME entEnv => strToEnv(sign,entEnv),
			  env)
	fun ppConstraints (variety,constraints : M.sharespec list) = 
		(begin_block CONSISTENT 0;
		 ppvseq ppstrm 0 ""
		  (fn ppstrm => fn paths =>
		      (begin_block INCONSISTENT 2;
			pps "sharing "; pps variety;
			ppSequence ppstrm 
			 {sep=(fn ppstrm => 
				(pps " ="; add_break (1,0))),
			  pr=ppSymPath,
			  style=INCONSISTENT}
			 paths;
		       end_block()))
		  constraints;
		end_block ())
	val somePrint = ref false
     in if depth <= 0
	then pps "<sig>"
	else
	case sign
	  of M.SIG {stamp,name,elements,typsharing,strsharing,...} =>
	     if !internals then 
	       (begin_block CONSISTENT 0;
		 pps "SIG:";
		 nl_indent ppstrm 2;
		 begin_block CONSISTENT 0;
		  pps "stamp: "; pps (Stamps.toShortString stamp);
		  add_newline();
		  pps "name: ";
		  case name
		    of NONE => pps "ANONYMOUS"
		     | SOME p => (pps "NAMED "; ppSym ppstrm p);
		  case elements
		    of nil => ()
		     | _ => (add_newline(); pps "elements:";
			     nl_indent ppstrm 2;
			     ppElements (env,depth,entityEnvOp) ppstrm elements);
		  case strsharing
                    of nil => ()
		     | _ => (add_newline(); pps "strsharing:";
			     nl_indent ppstrm 2;
			     ppConstraints("",strsharing));
		  case typsharing
                    of nil => ()
		     | _ => (add_newline(); pps "tycsharing:";
			     nl_indent ppstrm 2;
			     ppConstraints("type ",typsharing));
		 end_block();
		end_block())
	      else (* not !internals *)
		(begin_block CONSISTENT 0;
		  pps "sig";
		  add_break (1,2);
		  begin_block CONSISTENT 0;
		   case elements
		     of nil => ()
		      | _ => (ppElements (env,depth,entityEnvOp) ppstrm elements;
			      somePrint := true);
		   case strsharing
		     of nil => ()
		      | _ => (if !somePrint then add_newline() else ();
			      ppConstraints("",strsharing);
			      somePrint := true);
		   case typsharing
		     of nil => ()
		      | _ => (if !somePrint then add_newline() else ();
			      ppConstraints("type ",typsharing);
			      somePrint := true);
		  end_block();
		  if !somePrint then add_break(1,0) else ();
		  pps "end";
		 end_block())
	   | M.ERRORsig => pps "<error sig>"
    end

and ppFunsig ppstrm (sign,env,depth) =
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
	fun trueBodySig (orig as M.SIG { elements =
					 [(sym, M.STRspec { sign, ... })],
					 ... }) =
	    if Symbol.eq (sym, resultId) then sign else orig
	  | trueBodySig orig = orig
     in if depth<=0 then pps "<fctsig>"
	else case sign
	       of M.FSIG {paramsig,paramvar,paramsym,bodysig, ...} => 
		   if !internals
		   then (begin_block CONSISTENT 0;
			  pps "FSIG:";
			  nl_indent ppstrm 2;
			  begin_block CONSISTENT 0;
			   pps "psig: ";
			   ppSignature0 ppstrm (paramsig,env,depth-1,NONE);
			   add_newline();
			   pps "pvar: ";
			   pps (EntPath.entVarToString paramvar);
			   add_newline();
			   pps "psym: ";
			   (case paramsym
			      of NONE => pps "<anonymous>"
			       | SOME sym => ppSym ppstrm sym);
			   add_newline();
			   pps "bsig: ";
			   ppSignature0 ppstrm (bodysig,env,depth-1,NONE);
			  end_block();
			 end_block())
		   else (begin_block CONSISTENT 0;
			  pps "(";
                          case paramsym
			    of SOME x => pps (S.name x)
			     | _ => pps "<param>";
			  pps ": ";
			  ppSignature0 ppstrm (paramsig,env,depth-1,NONE);
			  pps ") :";
			  add_break(1,0);
			  ppSignature0 ppstrm
			    (trueBodySig bodysig,env,depth-1,NONE);
			 end_block())
		| M.ERRORfsig => pps "<error fsig>"
    end


and ppStrEntity ppstrm (e,env,depth) =
    let val {stamp,entities,properties,rpath,stub} = e
	val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
     in if depth <= 1 
	then pps "<structure entity>"
	else (begin_block CONSISTENT 0;
	       pps "strEntity:";
	       nl_indent ppstrm 2;
	       begin_block CONSISTENT 0;
		pps "rpath: ";
		pps (IP.toString rpath);
		add_newline();
		pps "stamp: ";
		pps (Stamps.toShortString stamp);
		add_newline();
		pps "entities:";
		nl_indent ppstrm 2;
		ppEntityEnv ppstrm (entities,env,depth-1);
		add_newline();
		pps "lambdaty:";
		nl_indent ppstrm 2;
		ppLty ppstrm ( (* ModulePropLists.strEntityLty e,depth-1 *));
	       end_block ();
	      end_block ())
    end

and ppFctEntity ppstrm (e, env, depth) =
    let val {stamp,closure,properties,tycpath,rpath,stub} = e
	val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
    in if depth <= 1 
	then pps "<functor entity>"
	else (begin_block CONSISTENT 0;
	       pps "fctEntity:";
	       nl_indent ppstrm 2;
	       begin_block CONSISTENT 0;
		pps "rpath: ";
		pps (IP.toString rpath);
		add_newline();
		pps "stamp: ";
		pps (Stamps.toShortString stamp);
		add_newline();
		pps "closure:";
		add_break (1,2);
		ppClosure ppstrm (closure,depth-1);
		add_newline();
		pps "lambdaty:";
		add_break (1,2);
		ppLty ppstrm ( (* ModulePropLists.fctEntityLty e,depth-1 *) );
		pps "tycpath:";
		add_break (1,2);
		pps "--printing of tycpath not implemented yet--";
	       end_block ();
	      end_block ())
    end

and ppFunctor ppstrm =
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
	fun ppF (M.FCT { sign, rlzn, ... }, env, depth) =
		if depth <= 1 
		then pps "<functor>"
		else (begin_block CONSISTENT 0;
		      pps "sign:";
		      nl_indent ppstrm 2;
		      ppFunsig ppstrm (sign,env,depth-1);
		      add_newline();
		      pps "rlzn:";
		      nl_indent ppstrm 2;
		      ppFctEntity ppstrm (rlzn,env,depth-1);
		      end_block ())
	  | ppF (M.ERRORfct,_,_) = pps "<error functor>"
     in ppF
    end

and ppTycBind ppstrm (tyc,env) =
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
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
				     begin_block CONSISTENT 0;
				      pps "ppTycBind failure: ";
				      add_newline();
				      ppTycon env ppstrm tyc;
				      add_newline();
				      ppTycon env ppstrm d_found;
				      add_newline();
				     end_block();
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
	then (begin_block CONSISTENT 0;
	       pps "type "; ppTycon env ppstrm tyc;
	      end_block())
	else
	    case tyc of
		T.GENtyc { path, arity, eq, kind, ... } =>
		(case (!eq, kind) of
		     (T.ABS, _) =>
		     (* abstype *)
		     (begin_block CONSISTENT 0;
		      pps "type";
		      ppFormals ppstrm arity; 
		      pps " ";
		      ppSym ppstrm (IP.last path);
		      end_block())
		   | (_, T.DATATYPE{index,family={members,...},...}) =>
		     (* ordinary datatype *)
		     let val {dcons,...} = Vector.sub(members,index)
			 val visdcons = visibleDcons(tyc,dcons)
			 val incomplete = length visdcons < length dcons
		     in
			 begin_block CONSISTENT 0;
			 pps "datatype";
			 ppFormals ppstrm arity;
			 pps " ";
			 ppSym ppstrm (IP.last path);
			 case visdcons
			   of nil => pps " = ..."
			    | first :: rest =>
			       (add_break(1,2);
				begin_block CONSISTENT 0;
				 pps "= "; ppDcon first;
				 app (fn d => (add_break(1,0); pps "| "; ppDcon d))
				     rest;
				 if incomplete
				     then (add_break(1,0); pps "... ")
				 else ();
				end_block());
			end_block()
		    end
		   | _ =>
		     (begin_block CONSISTENT 0;
		      if EqTypes.isEqTycon tyc
		      then pps "eqtype" 
		      else pps "type";
		      ppFormals ppstrm arity; 
		      pps " ";
		      ppSym ppstrm (IP.last path);
		      end_block()))
	      | T.DEFtyc{path,tyfun=T.TYFUN{arity,body},...} =>
		(begin_block INCONSISTENT 2;
		 pps "type"; 
		 ppFormals ppstrm arity; 
		 add_break (1,0);
		 ppSym ppstrm (InvPath.last path); 
		 pps " ="; 
		 add_break (1,0);
		 ppType env ppstrm body;
		 end_block ())
	      | tycon =>
		(pps "strange tycon: ";
		 ppTycon env ppstrm tycon)
    end (* ppTycBind *)

and ppEntity ppstrm (entity,env,depth) =
    case entity
      of M.TYCent tycon => ppTycon env ppstrm tycon
       | M.STRent strEntity => ppStrEntity ppstrm (strEntity,env,depth-1)
       | M.FCTent fctEntity => ppFctEntity ppstrm (fctEntity,env,depth-1)
       | M.ERRORent => add_string ppstrm "ERRORent"

and ppEntityEnv ppstrm (entEnv,env,depth) =
    if depth <= 1 
    then add_string ppstrm "<entityEnv>"
    else (ppvseq ppstrm 2 ""
	      (fn ppstrm => fn (entVar,entity) =>
		let val {begin_block,end_block,pps,add_break,add_newline} =
			 en_pp ppstrm
		 in begin_block CONSISTENT 2;
		     pps (EntPath.entVarToString entVar);
		     pps ":";
		     nl_indent ppstrm 2;
		     ppEntity ppstrm (entity,env,depth-1);
		     add_newline();
		    end_block()
		end)
	  (EE.toList entEnv))

and ppEntDec ppstrm (entDec,depth) =
    if depth <= 0 then add_string ppstrm "<entDec>"
    else case entDec
	  of M.TYCdec(entVar,tycExp) =>
	      (add_string ppstrm "ED.T: ";
	       ppEntVar ppstrm entVar; add_break ppstrm (1,1);
	       ppTycExp ppstrm (tycExp,depth-1))
	   | M.STRdec(entVar,strExp,sym) =>
	      (add_string ppstrm "ED.S: ";
	       ppEntVar ppstrm entVar; add_break ppstrm (1,1);
	       ppStrExp ppstrm (strExp,depth-1); add_break ppstrm (1,1);
	       ppSym ppstrm sym)
	   | M.FCTdec(entVar,fctExp) =>
	      (add_string ppstrm "ED.F: ";
	       ppEntVar ppstrm entVar; add_break ppstrm (1,1);
	       ppFctExp ppstrm (fctExp,depth-1))
	   | M.SEQdec entityDecs =>
	      ppvseq ppstrm 0 ""
	        (fn ppstrm => fn entDec => ppEntDec ppstrm (entDec,depth))
		entityDecs
	   | M.LOCALdec(entityDecL,entityDecB) => add_string ppstrm "ED.L:"
	   | M.ERRORdec => add_string ppstrm "ED.ER:"
	   | M.EMPTYdec => add_string ppstrm "ED.EM:"

and ppStrExp ppstrm (strExp,depth) =
    if depth <= 0 then add_string ppstrm "<strExp>" else
    case strExp
      of M.VARstr ep =>
	  (add_string ppstrm "SE.V:"; add_break ppstrm (1,1); 
           ppEntPath ppstrm ep)
       | M.CONSTstr { stamp, rpath, ... } =>
	 (add_string ppstrm "SE.C:"; add_break ppstrm (1,1);
	  ppInvPath ppstrm rpath)
       | M.STRUCTURE{stamp,entDec} =>
	  (add_string ppstrm "SE.S:"; add_break ppstrm (1,1);
	   ppEntDec ppstrm (entDec,depth-1))
       | M.APPLY(fctExp,strExp) =>
	  (begin_block ppstrm CONSISTENT 0;
	    add_string ppstrm "SE.AP:"; add_break ppstrm (1,1);
	    begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "fct:"; ppFctExp ppstrm (fctExp, depth -1);
	     add_break ppstrm (1,0);
	     add_string ppstrm "arg:"; ppStrExp ppstrm (strExp, depth -1);
	    end_block ppstrm;
	   end_block ppstrm)
       | M.LETstr(entDec,strExp) => 
	  (begin_block ppstrm CONSISTENT 0;
           add_string ppstrm "SE.L:"; add_break ppstrm (1,1);
           begin_block ppstrm CONSISTENT 0;
	   add_string ppstrm "let:"; ppEntDec ppstrm (entDec,depth-1);
           add_break ppstrm (1,0);
           add_string ppstrm "in:"; ppStrExp ppstrm (strExp, depth -1);
           end_block ppstrm;
	   end_block ppstrm)
       | M.ABSstr(sign,strExp) => 
          (begin_block ppstrm CONSISTENT 0;
           add_string ppstrm "SE.AB:"; add_break ppstrm (1,1);
            begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "sign: <omitted>"; 
	     add_break ppstrm (1,0);
	     add_string ppstrm "sexp:"; ppStrExp ppstrm (strExp, depth -1);
	    end_block ppstrm;
	   end_block ppstrm)
       | M.CONSTRAINstr{boundvar,raw,coercion} => 
          (begin_block ppstrm CONSISTENT 0;
           add_string ppstrm "SE.CO:"; add_break ppstrm (1,1);
            begin_block ppstrm CONSISTENT 0;
             ppEntVar ppstrm boundvar; add_break ppstrm (1,1);
	     add_string ppstrm "src:"; ppStrExp ppstrm (raw, depth -1);
	     add_break ppstrm (1,0);
	     add_string ppstrm "tgt:"; ppStrExp ppstrm (coercion, depth -1);
	    end_block ppstrm;
	   end_block ppstrm)
       | M.FORMstr(sign) => add_string ppstrm "SE.FM:"

and ppFctExp ppstrm (fctExp,depth) =
    if depth <= 0 then add_string ppstrm "<fctExp>" else
    case fctExp
      of M.VARfct ep =>
	  (add_string ppstrm "FE.V:"; ppEntPath ppstrm ep)
       | M.CONSTfct { rpath, ... } =>
	  (add_string ppstrm "FE.C:"; ppInvPath ppstrm rpath)
       | M.LAMBDA_TP {param, body, ...} =>
	  (begin_block ppstrm CONSISTENT 0;
	    add_string ppstrm "FE.LP:"; add_break ppstrm (1,1);
	    begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "par:"; ppEntVar ppstrm param;
	     add_break ppstrm (1,0);
	     add_string ppstrm "bod:"; ppStrExp ppstrm (body, depth-1);
	    end_block ppstrm;
	   end_block ppstrm)    
       | M.LAMBDA {param, body} =>
	  (begin_block ppstrm CONSISTENT 0;
	    add_string ppstrm "FE.L:"; add_break ppstrm (1,1);
	    begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "par:"; ppEntVar ppstrm param;
	     add_break ppstrm (1,0);
	     add_string ppstrm "bod:"; ppStrExp ppstrm (body, depth-1);
	    end_block ppstrm;
	   end_block ppstrm)    
       | M.LETfct (entDec,fctExp) => 
          (begin_block ppstrm CONSISTENT 0;
            add_string ppstrm "FE.LT:"; add_break ppstrm (1,1);
            begin_block ppstrm CONSISTENT 0;
  	     add_string ppstrm "let:"; ppEntDec ppstrm (entDec,depth-1);
             add_break ppstrm (1,0);
             add_string ppstrm "in:"; ppFctExp ppstrm (fctExp, depth -1);
            end_block ppstrm;
	   end_block ppstrm)    

(*
and ppBodyExp ppstrm (bodyExp,depth) =
    if depth <= 0 then add_string ppstrm "<bodyExp>" else
    case bodyExp
      of M.FLEX sign => add_string ppstrm "BE.F:"
       | M.OPAQ (sign,strExp) =>
	   (begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "BE.O:"; add_break ppstrm (1,1);
	     ppStrExp ppstrm (strExp,depth-1);
	    end_block ppstrm)
       | M.TNSP (sign,strExp) =>
	   (begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm "BE.T:"; add_break ppstrm (1,1);
	     ppStrExp ppstrm (strExp,depth-1);
	    end_block ppstrm)

*)

and ppClosure ppstrm (M.CLOSURE{param,body,env},depth) =
    let val {begin_block,end_block,pps,add_newline,add_break,...} = en_pp ppstrm
     in begin_block CONSISTENT 0;
	 pps "CL:"; add_break (1,1);
	  begin_block CONSISTENT 0;
	   pps "param: "; ppEntVar ppstrm param; add_newline();
	   pps "body: "; ppStrExp ppstrm (body,depth-1); add_newline();
           pps "env: "; ppEntityEnv ppstrm (env,SE.empty,depth-1);
	  end_block();
	end_block()
    end

(* assumes no newline is needed before pping *)
and ppBinding ppstrm (name,binding:B.binding,env:SE.staticEnv,depth:int) =
    case binding
      of B.VALbind var => (pps ppstrm "val "; ppVariable ppstrm (var,env))
       | B.CONbind con => ppConBinding ppstrm (con,env)
       | B.TYCbind tycon => ppTycBind ppstrm (tycon,env)
       | B.SIGbind sign =>
	  let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
	   in begin_block CONSISTENT 0;
	       pps "signature "; ppSym ppstrm name; pps " =";
	       add_break(1,2);
	       ppSignature0 ppstrm (sign,env,depth,NONE);
	      end_block()
	  end
       | B.FSGbind fs =>
	  let val {begin_block,end_block,pps,...} = en_pp ppstrm
	   in begin_block CONSISTENT 2;
	       pps "funsig "; ppSym ppstrm name; 
	       ppFunsig ppstrm (fs,env,depth);
	      end_block()
	  end
       | B.STRbind str =>
	  let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
	   in begin_block CONSISTENT 0;
	       pps "structure "; ppSym ppstrm name; pps " :";
	       add_break(1,2);
	       ppStructure ppstrm (str,env,depth);
	      end_block()
	  end
       | B.FCTbind fct =>
	  let val {begin_block,end_block,pps,...} = en_pp ppstrm
	   in begin_block CONSISTENT 0;
	       pps "functor ";
	       ppSym ppstrm name;
	       pps " : <sig>";  (* DBM -- should print the signature *)
	      end_block()
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
	  {sep=add_newline,
	   pr=(fn ppstrm => fn (name,binding) =>
	          ppBinding ppstrm (name,binding,pp_env,depth)),
	   style=CONSISTENT}
	  (all_ppable_bindings bindings pp_env)
    end

fun ppOpen ppstrm (path,str,env,depth) =
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
     in begin_block CONSISTENT 0;
	 begin_block CONSISTENT 2;
	  add_string ppstrm "opening ";
	  ppSymPath ppstrm path;
	  if depth < 1 then ()
          else (case str
		  of M.STR { sign, rlzn as {entities,...}, ... } =>
		     (case sign
			 of M.SIG {elements = [],...} => ()
			  | M.SIG {elements,...} => 
			    (add_newline ();		       
			     begin_block CONSISTENT 0;
			     ppElements (SE.atop(sigToEnv sign, env),
					 depth,SOME entities)
				        ppstrm elements;
			     end_block ())
			  | M.ERRORsig => ())
		   | M.ERRORstr => ()
		   | M.STRSIG _ => bug "ppOpen");
         end_block ();
         add_newline();
        end_block ()
    end        

fun ppSignature ppstrm (sign,env,depth) = 
    ppSignature0 ppstrm (sign,env,depth,NONE)

end (* local *)
end (* structure PPModules *)
