(* Copyright 1996 by AT&T Bell Laboratories *)
(* ppdec.sml *)

signature PPDEC =
sig
  val ppDec : Environment.environment -> PrettyPrint.ppstream 
                -> (Absyn.dec * Access.lvar list) -> unit
  val debugging : bool ref
end (* signature PPDEC *)

structure PPDec : PPDEC =
struct 

local 
  structure S = Symbol
  structure IP = InvPath
  structure M = Modules
  structure V = VarCon
  open Types VarCon Modules Bindings Fixity Absyn
       PrettyPrint PPUtil PPType PPObj Access
in 

(* debugging *)
val say = Control.Print.say
val debugging = ref false
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("PPDec: "^msg)


type object = Unsafe.Object.object

val signatures = Control.Print.signatures
val printOpens = Control.Print.printOpens
val printDepth = Control.Print.printDepth
val anonSym = S.strSymbol "<anonymousSig>"
val anonFsym = S.fctSymbol "<anonymousFsig>"

fun pplist_nl ppstrm pr =
  let fun pp [] = ()
        | pp [el] = pr el
        | pp (el::rst) = (pr el; add_newline ppstrm; pp rst)
   in pp
  end

fun C f x y = f y x;

fun xtract (v, pos) = Vector.sub (Unsafe.Object.toTuple v, pos)

exception OVERLOAD

fun ppDec ({static,dynamic,...}: Environment.environment)
          (ppstrm: ppstream) (dec: Absyn.dec, exportLvars) =
   let val dec = (* pruneDec *) dec

       fun isExport (x : Access.lvar, []) = false
         | isExport (x, a::r) = if x = a then true else isExport(x, r)

       val pps = add_string ppstrm
	(* trueValType: get the type of the bound variable from static,
	   since the stamps in the absyn haven't been converted by the pickler *)
       fun trueValType path =
	    let val err = fn _ => fn _ => fn _ => (bug "trueValType: unbound")
	     in case path
		  of SymPath.SPATH[id] =>
		      (case Lookup.lookValSym(static,id,err)
			 of V.VAL(V.VALvar{typ,...}) => !typ
			  | V.VAL(V.OVLDvar{name,scheme,...}) =>
			     (print ("#trueValType: OVLDvar"^Symbol.name name^"\n");
			      raise OVERLOAD)
			  | V.VAL(V.ERRORvar) =>
			     bug "trueValType: ERRORvar\n"
			  | V.CON(DATACON{name,typ,...}) =>
			     bug ("trueValType: DATACON"^Symbol.name name^"\n"))
		   | _ => bug "trueValType: not singleton path"
	    end

       fun trueTycon (path: IP.path) =
	    let val err = fn _ => fn _ => fn _ => (bug "trueTycon: unbound ")
	     in case Lookup.lookTyc(static,ConvertPaths.invertIPath(path),err)
		  of tyc as DEFtyc _ => tyc
		   | _ => bug "trueTycon: not a DEFtyc"
	    end

       fun ppVar (VALvar{path, access, typ=(t0 as ref ty), info}) =
             (begin_block ppstrm CONSISTENT 0;
	      begin_block ppstrm INCONSISTENT 2;
	      add_string ppstrm "val "; 
	      ppSymPath ppstrm path; 
	      add_string ppstrm " =";
	      add_break ppstrm (1,0);

	      case access
	       of LVAR lv =>  
                    (case StaticEnv.look (static, SymPath.last path)
                      of VALbind(VALvar{access=PATH (EXTERN pid, pos), ...}) =>
                           if isExport(lv, exportLvars)
                           then (let val obj = 
                                       xtract (DynamicEnv.look dynamic pid, 
                                               pos)
                                  in ppObj static ppstrm 
                                       (obj, ty, !printDepth);
				     add_break ppstrm (1,0);
				     add_string ppstrm ": "; 
			 	     ppType static ppstrm (trueValType path 
					   handle OVERLOAD => ty)
                                 end)
                           else (add_string ppstrm "<hidden-value>";
				 add_break ppstrm (1,0);
				 add_string ppstrm ": "; 
			 	 ppType static ppstrm ty)

                       | _ => add_string ppstrm "<PPDec.getVal failure>")
               
           (*** | PRIMOP _ => add_string ppstrm "<primop>" *)
 	        | _ => ErrorMsg.impossible "ppDec.ppVb.ppBind.VARpat";

	      end_block ppstrm;
	      add_newline ppstrm;
 	      end_block ppstrm)

         | ppVar _ = ()

       fun ppVb (VB{pat,...}) =
	 let fun ppBind(pat) =
	           case pat
		    of VARpat v => ppVar v
		     | RECORDpat{fields,...} => app (ppBind o #2) fields
		     | VECTORpat(pats,_) => app ppBind pats
		     | APPpat(_,_,pat) => ppBind pat
		     | CONSTRAINTpat(pat,_) => ppBind pat
		     | LAYEREDpat(pat1,pat2) => (ppBind pat1; ppBind pat2)
                     | ORpat(p1, _) => ppBind p1
		     | _ => ()
	  in ppBind pat
	 end

       and ppRvb (RVB{var, ...}) = ppVar var

       and ppTb(DEFtyc{path,...}) =
	    let val DEFtyc{path,tyfun=TYFUN{arity,body},...} = trueTycon path
	     in begin_block ppstrm CONSISTENT 0;
		 begin_block ppstrm INCONSISTENT 2;
		  add_string ppstrm "type"; 
		  ppFormals ppstrm arity; 
		  add_break ppstrm (1,0);
		  ppSym ppstrm (InvPath.last path); 
		  add_string ppstrm " ="; 
		  add_break ppstrm (1,0);
		  ppType static ppstrm body;
		 end_block ppstrm;
	         add_newline ppstrm;
		end_block ppstrm
	    end

	and ppAbsTyc(GENtyc{path, arity, eq=ref(ABS), ...}) =
	      (begin_block ppstrm CONSISTENT 0;
	         begin_block ppstrm INCONSISTENT 2;
	         add_string ppstrm "type"; 
	         ppFormals ppstrm arity; 
	         add_break ppstrm (1,0);
	         ppSym ppstrm (InvPath.last path); 
	         end_block ppstrm;
               add_newline ppstrm;
	        end_block ppstrm)
          | ppAbsTyc(GENtyc{path, arity, eq=ref _, ...}) =
	      (begin_block ppstrm CONSISTENT 0;
	         begin_block ppstrm INCONSISTENT 2;
	         add_string ppstrm "type"; 
	         ppFormals ppstrm arity; 
	         add_break ppstrm (1,0);
	         ppSym ppstrm (InvPath.last path); 
	         end_block ppstrm;
               add_newline ppstrm;
	        end_block ppstrm)
          | ppAbsTyc _ = bug "unexpected case in ppAbsTyc"

	and ppDataTyc(GENtyc{path, arity,
                             kind=DATATYPE{index, freetycs, family={members, ...},...}, ...}) =
	    let fun ppDcons nil = ()
		  | ppDcons (first::rest) =
		     let fun ppDcon ({name,domain,rep}) =
			     (ppSym ppstrm name; 
			      case domain
				of SOME dom =>
			            (add_string ppstrm " of ";
				     ppDconDomain (members,freetycs) static ppstrm dom)
			         | NONE => ())
		      in add_string ppstrm "= "; ppDcon first;
			 app (fn d => (add_break ppstrm (1,0);
				       add_string ppstrm "| "; ppDcon d))
			 rest
		     end
		val {tycname,dcons,...} = Vector.sub(members,index)
	     in begin_block ppstrm CONSISTENT 0;
		 begin_block ppstrm CONSISTENT 0;
		  add_string ppstrm "datatype";
		  ppFormals ppstrm arity;
		  add_string ppstrm " ";
		  ppSym ppstrm (InvPath.last path); 
		  add_break ppstrm (1,2);
		  begin_block ppstrm CONSISTENT 0;
		   ppDcons dcons;
		  end_block ppstrm;
		 end_block ppstrm;
		 add_newline ppstrm;
		end_block ppstrm
	    end

	and ppEb(EBgen{exn=DATACON{name,...},etype,...}) =
	      (begin_block ppstrm CONSISTENT 0;
	       begin_block ppstrm INCONSISTENT 2;
	       add_string ppstrm "exception "; 
	       ppSym ppstrm name;
	       case etype
		 of NONE => ()
		  | SOME ty' => 
		           (add_string ppstrm " of"; 
			    add_break ppstrm (1,0);
			    ppType static ppstrm ty');
	       end_block ppstrm;
 	       add_newline ppstrm;
	       end_block ppstrm)

	  | ppEb(EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =
	      (begin_block ppstrm CONSISTENT 0;
	       begin_block ppstrm INCONSISTENT 2;
	       add_string ppstrm "exception "; 
	       ppSym ppstrm name;
	       add_string ppstrm " ="; 
	       add_break ppstrm (1,0);
	       ppSym ppstrm dname;
	       end_block ppstrm;
 	       add_newline ppstrm;
	       end_block ppstrm)

	and ppStrb isAbs (STRB{name,str,...}) =    (* isAbs strvar *)
	    (begin_block ppstrm CONSISTENT 0;
	      begin_block ppstrm CONSISTENT 0;
	       pps "structure ";
	       ppSym ppstrm name;
	       pps " :";
	       add_break ppstrm (1,2);
	       PPModules.ppStructure ppstrm (str,static,!signatures);
	      end_block ppstrm;
	      add_newline ppstrm;
	     end_block ppstrm)

	and ppFctb(FCTB{name,fct,...}) = 
	    (begin_block ppstrm CONSISTENT 0;
	      pps "functor ";
	      ppSym ppstrm name;
	      pps " : <sig>";  (* DBM -- should print the signature *)
	      add_newline ppstrm;
	     end_block ppstrm)

        and ppSigb sign = 
	    let val name = case sign 
                            of M.SIG{name=SOME s, ...} => s
                             | _ => anonSym

             in (begin_block ppstrm CONSISTENT 0;
		  begin_block ppstrm CONSISTENT 0;
   	           pps "signature "; ppSym ppstrm name; pps " =";
	           add_break ppstrm (1,2);
	           PPModules.ppSignature ppstrm (sign,static,!signatures);
	          end_block ppstrm;
	         add_newline ppstrm;
	         end_block ppstrm)
            end

        and ppFsigb fsig = 
	    let val name = case fsig 
                            of M.FSIG{kind=SOME s, ...} => s
                             | _ => anonFsym

	     in (begin_block ppstrm CONSISTENT 0;
	         pps "funsig "; ppSym ppstrm name; 
	         PPModules.ppFunsig ppstrm (fsig,static,!signatures);
	         add_newline ppstrm;
	         end_block ppstrm)
            end

	and ppFixity{fixity,ops} =
	    (begin_block ppstrm CONSISTENT 0;
	     begin_block ppstrm CONSISTENT 0;
	     add_string ppstrm (Fixity.fixityToString fixity);
	     PPUtil.ppSequence ppstrm {sep=C PrettyPrint.add_break (1,0),
			               pr=PPUtil.ppSym,
			               style=INCONSISTENT}
	                       ops;
	     end_block ppstrm;
	     add_newline ppstrm;		       
	     end_block ppstrm)

	and ppOpen(pathStrs) =  
	    if !printOpens
	    then (begin_block ppstrm CONSISTENT 0;
		   app (fn (path,str) =>
			 PPModules.ppOpen ppstrm (path,str,static,!signatures))
		       pathStrs;
		  end_block ppstrm)
	    else (begin_block ppstrm CONSISTENT 0;
		  begin_block ppstrm CONSISTENT 0;
		  add_string ppstrm "open ";
		  ppSequence ppstrm {sep=C PrettyPrint.add_break (1,0),
			     pr=(fn ppstrm => fn (path,_)
				    => ppSymPath ppstrm path),
			     style=INCONSISTENT}
			     pathStrs;
		  end_block ppstrm;
		  add_newline ppstrm;		       
		  end_block ppstrm)

	and ppDec0 dec =
	    case (resetPPType(); dec)
	      of VALdec vbs => app ppVb vbs
	       | VALRECdec rvbs => app ppRvb rvbs
	       | TYPEdec tbs => app ppTb tbs
	       | DATATYPEdec{datatycs,withtycs} =>
		   (app ppDataTyc datatycs; 
		    app ppTb withtycs)
	       | ABSTYPEdec{abstycs,withtycs,body} =>
		   (app ppAbsTyc abstycs;
		    app ppTb withtycs;
		    ppDec0 body)
	       | EXCEPTIONdec ebs => app ppEb ebs
	       | STRdec strbs => app (ppStrb false) strbs
	       | ABSdec strbs => app (ppStrb true) strbs
	       | FCTdec fctbs => app ppFctb fctbs
	       | SIGdec sigbs => app ppSigb sigbs
	       | FSIGdec fsigbs => app ppFsigb fsigbs
	       | LOCALdec(decIn,decOut) => ppDec0 decOut
	       | SEQdec decs => 
		  (case decs
		     of OPENdec pathStrs :: rest =>
			 ppOpen pathStrs
                      | _ => app ppDec0 decs)
	       | FIXdec fixd => ppFixity fixd
	       | OVLDdec _ => 
                   (add_string ppstrm "overload"; add_newline ppstrm)
	       | OPENdec pathStrs => ppOpen pathStrs
	       | MARKdec(dec,_) => ppDec0 dec

     in begin_block ppstrm CONSISTENT 0;
	ppDec0 dec;
	end_block ppstrm;
	flush_ppstream ppstrm
    end

end (* local *)
end (* structure PPDec *)

(*
 * $Log: ppdec.sml,v $
 * Revision 1.12  1997/10/07  16:11:44  dbm
 *   Changed ppOpen to call PPModules.ppOpen for improved printing of
 *   top level open declarations.
 *
 * Revision 1.11  1997/10/02  22:20:15  dbm
 *   Minor cleanup of type decl printing.
 *
 * Revision 1.10  1997/09/23  04:01:48  dbm
 *   Fix pretty printing of type declarations (extra space) and signature
 *   constraints (unnecessary newline for short sigs).
 *
 * Revision 1.9  1997/09/17  21:35:15  dbm
 *   Print short signatures on same line.
 *
 * Revision 1.8  1997/08/26  20:50:42  appel
 * Fixed bug in which <hidden-value> had wrong type.
 *
 * Revision 1.7  1997/08/15  20:46:27  dbm
 *   Added code to detect top-level opens and print them in a short form
 *   or long form (with signatures, the default), according to the value
 *   of Control.printOpens.
 *
 * Revision 1.6  1997/07/15  16:19:53  dbm
 *   Adjust to changes in signature representation.
 *
 * Revision 1.5  1997/06/30  19:37:33  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.4  1997/05/20  12:27:44  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.3  1997/03/25  13:41:45  george
 *   Fixing the coredump bug caused by duplicate top-level declarations.
 *   For example, in almost any versions of SML/NJ, typing
 *           val x = "" val x = 3
 *   would lead to core dump. This is avoided by changing the "exportLexp"
 *   field returned by the pickling function (pickle/picklemod.sml) into
 *   a list of lambdavars, and then during the pretty-printing (print/ppdec.sml),
 *   each variable declaration is checked to see if it is in the "exportLvars"
 *   list, if true, it will be printed as usual, otherwise, the pretty-printer
 *   will print the result as <hiddle-value>.
 * 						-- zsh
 *
 * Revision 1.2  1997/03/17  18:58:48  dbm
 * Changes in datatype representation to support datatype replication.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:43  george
 *   Version 109.24
 *
 *)
