(* Copyright 1996 by AT&T Bell Laboratories *)
(* ppval.sml *)

signature PPVAL = 
sig
  val ppAccess: PrettyPrint.ppstream -> Access.access -> unit
  val ppRep: PrettyPrint.ppstream -> Access.conrep -> unit
  val ppDcon: PrettyPrint.ppstream -> VarCon.datacon -> unit
  val ppVar: PrettyPrint.ppstream -> VarCon.var -> unit
  val ppDebugDcon : PrettyPrint.ppstream
		    -> StaticEnv.staticEnv -> VarCon.datacon -> unit
  val ppDebugVar: PrettyPrint.ppstream 
		  -> StaticEnv.staticEnv -> VarCon.var -> unit
end (* signature PPVAL *)

structure PPVal : PPVAL =
struct

local structure PP = PrettyPrint
      structure TU = TypesUtil
      structure LU = Lookup 
      structure A = Access
      structure II = InlInfo
      open PrettyPrint PPUtil VarCon Types

in 

val internals = Control.internals

fun C f x y = f y x

val pps = PP.add_string
val ppType = PPType.ppType
val ppTycon = PPType.ppTycon
val ppTyfun = PPType.ppTyfun

fun ppAccess ppstrm a = pps ppstrm (" ["^(A.prAcc a)^"]")

fun ppInfo ppstrm a = pps ppstrm (" ["^(II.prInfo a)^"]")

fun ppRep ppstrm =
    let val {pps,...} = en_pp ppstrm
     in fn rep => pps (A.prRep rep)
    end

fun ppCsig ppstrm =
    let val {pps, ...} = en_pp ppstrm
     in fn csig => pps (A.prCsig csig)
    end

fun ppDcon ppstrm =
    let fun ppD(DATACON{name, rep=A.EXN acc, ...}) =
	       (ppSym ppstrm (name); 
		if !internals then ppAccess ppstrm acc else ())
	  | ppD(DATACON{name,...}) = ppSym ppstrm (name)
     in ppD
    end

fun ppDebugDcon ppstrm env (DATACON{name,rep,const,typ,sign,lazyp}) =
    let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
	val ppSym = ppSym ppstrm
     in begin_block CONSISTENT 3;
        pps "DATACON";
	add_break(0,0);
	pps "{name = "; ppSym name; add_comma_nl ppstrm;
	pps "const = "; pps (Bool.toString const); add_comma_nl ppstrm;
	pps "typ = "; ppType env ppstrm typ; add_comma_nl ppstrm;
	pps "lazyp = "; pps (Bool.toString lazyp); add_comma_nl ppstrm;
	pps "conrep ="; ppRep ppstrm rep; add_comma_nl ppstrm;
        pps "sign = ["; ppCsig ppstrm sign; pps "]}";
        end_block()
    end

fun ppDatacon (env:StaticEnv.staticEnv,DATACON{name,typ,...}) ppstrm =
    let val {begin_block,end_block,pps,...} = en_pp ppstrm
     in begin_block INCONSISTENT 0;
	ppSym ppstrm name; pps " : "; ppType env ppstrm typ;
	end_block()
    end

fun ppConBinding ppstrm =
    let val {begin_block,end_block,pps,...} = en_pp ppstrm
	fun ppCon (DATACON{name, typ, rep=A.EXN _, ...}, env) =
		(begin_block CONSISTENT 0;
		 pps "exception "; ppSym ppstrm name; 
                 if BasicTypes.isArrowType typ then
                   (pps " of "; 
   		    ppType env ppstrm (BasicTypes.domain typ))
                 else ();
		 end_block())
	  | ppCon (con,env) = 
	      let exception Hidden
		  val visibleDconTyc =
		      let val tyc = TU.dconTyc con
		       in 
			  (TypesUtil.equalTycon
			      (LU.lookTyc
			         (env,SymPath.SPATH
				       [InvPath.last(TypesUtil.tycPath tyc)],
				  fn _ => raise Hidden),
			       tyc)
			     handle Hidden => false)
		      end
	       in if !internals orelse not visibleDconTyc 
	          then (begin_block CONSISTENT 0;
			pps "con ";
			ppDatacon(env,con) ppstrm;
		        end_block())
	          else ()
	      end
     in ppCon
    end

fun ppVar ppstrm (VALvar {access,path,...}) =
      (pps ppstrm (SymPath.toString path);
       if !internals then ppAccess ppstrm access else ())
  | ppVar ppstrm (OVLDvar {name,...}) = ppSym ppstrm (name)
  | ppVar ppstrm (ERRORvar) = add_string ppstrm "<errorvar>"

fun ppDebugVar ppstrm env  = 
    let val {begin_block,end_block,pps,...} = en_pp ppstrm
	val ppAccess = ppAccess ppstrm
        val ppInfo = ppInfo ppstrm
	fun ppDV(VALvar {access,path,typ,info}) = 
	     (begin_block CONSISTENT 0;
	      pps "VALvar";
	      begin_block CONSISTENT 3;
	      pps "({access="; ppAccess access; add_comma_nl ppstrm;
              pps "info="; ppInfo info; add_comma_nl ppstrm;
	      pps "path="; pps (SymPath.toString path); add_comma_nl ppstrm;
	      pps "typ=ref "; ppType env ppstrm (!typ); 
	      pps "})";
	      end_block(); end_block())
	  | ppDV (OVLDvar {name,options,scheme}) = 
	     (begin_block CONSISTENT 0;
	      pps "OVLDvar";
	      begin_block CONSISTENT 3;
	      pps "({name="; ppSym ppstrm (name); add_comma_nl ppstrm;
	      pps "options=["; 
	      (ppvseq ppstrm 0 ","
	       (fn ppstrm => fn {indicator,variant} =>
		  (pps "{indicator=";ppType env ppstrm  indicator; 
		   add_comma_nl ppstrm;
		   pps " variant ="; ppDebugVar ppstrm env variant; pps "}"))
	       (!options));
	      pps "]"; add_comma_nl ppstrm;
	      pps "scheme="; ppTyfun env ppstrm scheme; pps "})";
	      end_block();
	      end_block())
	  | ppDV (ERRORvar) = pps "<ERRORvar>"
     in ppDV
    end

fun ppVariable ppstrm  =
    let val {begin_block,end_block,pps,...} = en_pp ppstrm
	fun ppV(env:StaticEnv.staticEnv,VALvar{path,access,typ,info}) = 
	      (begin_block CONSISTENT 0;
	       pps(SymPath.toString path);
	       if !internals then ppAccess ppstrm access else ();
	       pps " : "; ppType env ppstrm (!typ);
	       end_block())
	  | ppV (env,OVLDvar {name,options=ref optl,scheme=TYFUN{body,...}}) =
	      (begin_block CONSISTENT 0;
	       ppSym ppstrm (name); pps " : "; ppType env ppstrm body; 
	       pps " as ";
	       ppSequence ppstrm
		 {sep=C PrettyPrint.add_break(1,0),
		  pr=(fn ppstrm => fn{variant,...} =>ppV(env,variant)),
		  style=CONSISTENT}
		 optl;
	       end_block())
	  | ppV(_,ERRORvar) = pps "<ERRORvar>"
     in ppV
    end

end (* local *)
end (* structure PPVal *)

