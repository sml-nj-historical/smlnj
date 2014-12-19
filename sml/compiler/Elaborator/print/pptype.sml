(* Copyright 1991 by AT&T Bell Laboratories *)
(* Copyright 2003 by The SML/NJ Fellowship *)
(* pptype.sml *)

(* modified to use SML/NJ Lib PP. [dbm, 7/30/03]) *)

signature PPTYPE = 
sig
  val typeFormals : int -> string list
  val tyvarPrintname : Types.tyvar -> string
  val ppTycon : StaticEnv.staticEnv -> PrettyPrintNew.stream 
                -> Types.tycon -> unit
  val ppTyfun : StaticEnv.staticEnv -> PrettyPrintNew.stream 
                -> Types.tyfun -> unit 
  val ppType  : StaticEnv.staticEnv -> PrettyPrintNew.stream 
                -> Types.ty -> unit
  val ppDconDomain : (Types.dtmember vector * Types.tycon list) 
                     -> StaticEnv.staticEnv 
                     -> PrettyPrintNew.stream -> Types.ty -> unit
  val ppDataconTypes : StaticEnv.staticEnv -> PrettyPrintNew.stream 
                -> Types.tycon -> unit
  val resetPPType : unit -> unit
  val ppFormals : PrettyPrintNew.stream -> int -> unit

  val debugging : bool ref
  val unalias : bool ref

end (* signature PPTYPE *)

structure PPType : PPTYPE =
struct

local 
      structure SP = SymPath
      structure IP = InvPath
      structure BT = BasicTypes
      structure T = Types
      structure TU = TypesUtil
      structure PP = PrettyPrintNew
      structure PU = PPUtilNew
      open Types PPUtilNew  
in

val debugging = ref false
val unalias = ref true

fun bug s = ErrorMsg.impossible ("PPType: " ^ s)
val pps = PP.string

fun C f x y = f y x

val internals = ElabControl.internals

val unitPath = IP.extend(IP.empty,Symbol.tycSymbol "unit")

fun boundTyvarName k =
    let val a = Char.ord #"a"
     in if k < 26
	then String.str(Char.chr(k+a))
	else implode[Char.chr(Int.div(k,26) + a), 
                     Char.chr(Int.mod(k,26) + a)]
    end

fun metaTyvarName' k =
    let val a = Char.ord #"Z" (* use reverse order for meta vars *)
     in if k < 26
	then String.str(Char.chr(a - k))
	else implode[Char.chr(a - (Int.div(k,26))), 
                     Char.chr(a - (Int.mod(k,26)))]
    end

fun typeFormals n =
    let fun loop i =
	if i>=n then []
	else (boundTyvarName i)::loop(i+1)
     in loop 0
    end

fun litKindPrintName (lk: T.litKind) =
    case lk
      of T.INT => "int"  (* or "INT" *)
       | T.WORD => "word" (* or "WORD" *)
       | T.REAL => "real" (* or "REAL" *)
       | T.CHAR => "char" (* or "CHAR" *)
       | T.STRING => "string" (* or "STRING" *)

local  (* WARNING -- compiler global variables *)
  val count = ref(~1)  
  val metaTyvars = ref([]:tyvar list)
in
  fun metaTyvarName(tv: tyvar) =
      let fun find([],_) =
	        (metaTyvars := tv::(!metaTyvars);
		 count := !count+1;
		 !count)
	    | find(tv'::rest,k) =
	        if tv = tv'
		then !count - k
		else find(rest,k+1)
       in metaTyvarName' (find(!metaTyvars,0))
      end
  fun resetPPType() = (count := ~1; metaTyvars := [])
end

fun tvHead (eq,base) =
    (if eq then "''" else "'")^base

fun annotate (name,annotation,depthOp) =
    if !internals
      then concat(name::"."::annotation::
		  (case depthOp
		     of SOME depth => ["[",(Int.toString depth),"]"]
		      | NONE => nil))
    else name

fun sourcesToString (OVAR(name,_)::_) = Symbol.name name
  | sourcesToString (OLIT(kind,_,_)::_) =
      (case kind
        of INT => "int"
         | WORD => "word"
         | _ => "?")
  | sourcesToString nil = bug "sourcesToString"

fun tyvarPrintname (tyvar) =
let fun prKind info =
	case info of
	    INSTANTIATED(VARty(tyvar)) => tyvarPrintname tyvar
	  | INSTANTIATED _ => "<INSTANTIATED ?>"
	  | OPEN{depth,eq,kind} =>
	    tvHead(eq, annotate(metaTyvarName tyvar,
				case kind of META => "M" | FLEX _ => "F",
				SOME depth))
	  | UBOUND{name,depth,eq} =>
	    tvHead(eq,annotate(Symbol.name name,"U",SOME depth))
	  | OVLD{sources,options} =>
	    annotate("["^sourcesToString sources ^ " ty]",
		     "O:" ^ Int.toString(length options),NONE)
	  | LBOUND{depth,eq,index} => 
	      (if eq then "<LBDeq" else "<LBD")^Int.toString depth^"."
	      ^Int.toString index^">"
in
    prKind (!tyvar)
end

fun ppkind ppstrm kind =
    pps ppstrm
      (case kind
	 of PRIMITIVE _ => "P" | FORMAL => "F"
          | FLEXTYC _ => "X" | ABSTRACT _ => "A"
	  | DATATYPE _ => "D" | TEMP => "T")

fun effectivePath(path,tyc,env) : string =
    let fun tycPath (GENtyc{path,...} | DEFtyc{path,...} | PATHtyc{path,...}) =
	    SOME path
	  | tycPath _ = NONE
	fun find(path,tyc) =
	    (findPath(path,
		(fn tyc' => TU.equalTycon(tyc',tyc)),
		(fn x => Lookup.lookTyc(env,x,
					(fn _ => raise StaticEnv.Unbound)))))
	fun search(path,tyc) =
	    let val (suffix,found) = find(path,tyc)
	     in if found then (suffix,true)
		else if not (!unalias) then (suffix, false)
		else case TU.unWrapDef1 tyc
		       of SOME tyc' =>
			   (case tycPath tyc'
			      of SOME path' =>
				  let val x as (suffix',found') = search(path',tyc')
				   in if found' then x
				      else (suffix,false)
				  end
			       | NONE => (suffix,false))
			| NONE => (suffix,false)
	    end
	val (suffix,found) = search(path,tyc)
	val name = SP.toString(SP.SPATH suffix)
     in if found
	    then name
	else "?."^name
    end

val arrowStamp = BT.arrowStamp

fun strength(ty) =
    case ty
      of VARty(ref(INSTANTIATED ty')) => strength(ty')
       | CONty(tycon, args) =>
	   (case tycon
 	      of GENtyc { stamp, kind = PRIMITIVE _, ... } =>
		 if Stamps.eq(stamp,arrowStamp) then 0 else 2
	       | RECORDtyc (_::_) =>  (* excepting type unit *)
		 if Tuples.isTUPLEtyc(tycon) then 1 else 2
	       | _ => 2)
       | MARKty(ty, region) => strength ty
       | _ => 2

fun ppEqProp ppstrm p =
    let val a = case p
		  of NO => "NO"
		   | YES => "YES"
		   | IND => "IND"
		   | OBJ => "OBJ"
		   | DATA => "DATA"
		   | ABS => "ABS"
		   | UNDEF => "UNDEF"
     in pps ppstrm a
    end

fun ppInvPath ppstream (InvPath.IPATH path: InvPath.path) = 
    PP.string ppstream (SymPath.toString (SymPath.SPATH(rev path)))

fun ppBool ppstream b =
    case b of true => pps ppstream "t" | false => pps ppstream "f"

fun ppTycon1 env ppstrm membersOp =
    let val {openHVBox,openHOVBox,closeBox,pps,break,...} = en_pp ppstrm
	fun ppTyc (tyc as GENtyc { path, stamp, eq, kind, ... }) =
	    if !internals
	    then (openHOVBox 1;
		  ppInvPath ppstrm path;
		  pps "[";
		  pps "G"; ppkind ppstrm kind; pps ";"; 
		  pps (Stamps.toShortString stamp);
		  pps ";";
		  ppEqProp ppstrm (!eq);
		  pps "]";
		  closeBox())
	    else pps(effectivePath(path,tyc,env))
	  | ppTyc(tyc as DEFtyc{path,strict,tyfun=TYFUN{body,...},...}) =
	     if !internals
	     then (openHOVBox 1;
		    ppInvPath ppstrm path;
		    pps "["; pps "D"; 
                    ppClosedSequence ppstrm
                      {front=C PP.string "(",
                       sep=fn ppstrm => (PP.string ppstrm ","; 
				   PP.break ppstrm {nsp=0,offset=0}),
		       back=C PP.string ");",
		       style=CONSISTENT,
                       pr=ppBool} strict;
		    ppType env ppstrm body;
		    pps "]";
		   closeBox())
	     else pps(effectivePath(path,tyc,env))
	  | ppTyc(RECORDtyc labels) =
	      ppClosedSequence ppstrm 
		{front=C PP.string "{",
		 sep=fn ppstrm => (PP.string ppstrm ","; 
				   PP.break ppstrm {nsp=0,offset=0}),
		 back=C PP.string "}",
		 style=INCONSISTENT,
		 pr=ppSym} labels

          | ppTyc (RECtyc n) =
              (case membersOp
                of SOME (members,_) => 
                    let val {tycname,dcons,...} = Vector.sub(members,n)
                     in ppSym ppstrm tycname
                    end
                 | NONE => pps (String.concat ["<RECtyc ",Int.toString n,">"]))

          | ppTyc (FREEtyc n) =
              (case membersOp
                of SOME (_, freetycs) => 
                    let val tyc = (List.nth(freetycs, n) handle _ =>
                                    bug "unexpected freetycs in ppTyc")
                     in ppTyc tyc
                    end
                 | NONE => 
                    pps (String.concat ["<FREEtyc ",Int.toString n,">"]))

 	  | ppTyc (tyc as PATHtyc{arity,entPath,path}) =
	     if !internals
	     then (openHOVBox 1;
		    ppInvPath ppstrm path; pps "[P;"; 
		    pps (EntPath.entPathToString entPath);
		    pps "]";
		   closeBox())
	     else ppInvPath ppstrm path

	  | ppTyc ERRORtyc = pps "[E]"
     in ppTyc
    end


and ppType1 env ppstrm (ty: ty, sign: T.polysign, 
                        membersOp: (T.dtmember vector * T.tycon list) option) : unit =
    let val {openHVBox,openHOVBox,closeBox,pps,ppi,break,newline} = en_pp ppstrm
        fun prty ty =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => prty(ty')
	       | VARty(tv) => ppTyvar tv
	       | IBOUND n =>
		   let val eq = List.nth(sign,n) 
		                handle Subscript => false
		    in pps (tvHead(eq,(boundTyvarName n)))
		   end
	       | CONty(tycon, args) => let
		     fun otherwise () =
			 (openHOVBox 2;
			  ppTypeArgs args; 
			  break{nsp=0,offset=0};
			  ppTycon1 env ppstrm membersOp tycon;
			  closeBox())
		 in
		     case tycon
		      of GENtyc { stamp, kind, ... } =>
			 (case kind of
			      PRIMITIVE _ =>
			      if Stamps.eq(stamp,arrowStamp)
			      then case args
			            of [domain,range] =>
				       (openHVBox 0;
					if strength domain = 0
					then (openHVBox 1;
					      pps "(";
					      prty domain;
					      pps ")";
					      closeBox())
					else prty domain;
					break{nsp=1,offset=0};
					pps "-> ";
					prty range;
					closeBox())
				     | _ => bug "CONty:arity"
			      else (openHOVBox 2;
                                    ppTypeArgs args;
                                    break{nsp=0,offset=0};
                                    ppTycon1 env ppstrm membersOp tycon;
                                    closeBox())
			    | _ => otherwise ())
		       | RECORDtyc labels =>
			 if Tuples.isTUPLEtyc(tycon)
			 then ppTUPLEty args
			 else ppRECORDty(labels, args)
		       | _ => otherwise ()
		 end
	       | POLYty{sign,tyfun=TYFUN{arity,body}} => 
                   if !internals
                   then (openHOVBox 1;
                         pps "[POLY("; pps(Int.toString arity); pps ")";
                         ppType1 env ppstrm (body,sign, membersOp);
                         pps "]";
                         closeBox())
                   else ppType1 env ppstrm (body,sign, membersOp)
	       | MARKty(ty, region) => prty ty
	       | WILDCARDty => pps "_"
	       | UNDEFty => pps "undef"

	and ppTypeArgs [] = ()
	  | ppTypeArgs [ty] = 
	     (if strength ty <= 1
	      then (openHOVBox 1;
                    pps "("; 
                    prty ty; 
                    pps ")";
                    closeBox())
	      else prty ty;
	      break{nsp=1,offset=0})
	  | ppTypeArgs tys =
              ppClosedSequence ppstrm 
	        {front=C PP.string "(",
		 sep=fn ppstrm => (PP.string ppstrm ",";
                                   PP.break ppstrm {nsp=0,offset=0}),
		 back=C PP.string ") ",
		 style=INCONSISTENT, 
                 pr=fn _ => fn ty => prty ty}
		tys

	and ppTUPLEty [] = pps(effectivePath(unitPath,RECORDtyc [],env))
	  | ppTUPLEty tys = 
	      ppSequence ppstrm
		 {sep = fn ppstrm => (PP.break ppstrm {nsp=1,offset=0};
				    PP.string ppstrm "* "),
		  style = INCONSISTENT,
		  pr = (fn _ => fn ty =>
			  if strength ty <= 1
			  then (openHOVBox 1;
				pps "("; 
				prty ty; 
				pps ")";
				closeBox())
			  else prty ty)}
	        tys

	and ppField(lab,ty) = (openHVBox 0;
			       ppSym ppstrm lab; 
			       pps ":";
			       prty ty;
			       closeBox())

	and ppRECORDty([],[]) = pps(effectivePath(unitPath,RECORDtyc [],env))
              (* this case should not occur *)
	  | ppRECORDty(lab::labels, arg::args) =
	      (openHOVBox 1;
               pps "{";
               ppField(lab,arg);
	       ListPair.app 
		 (fn field => (pps ","; break{nsp=1,offset=0}; ppField field))
		 (labels,args);
               pps "}";
               closeBox())
	  | ppRECORDty _ = bug "PPType.ppRECORDty"

	and ppTyvar (tv as (ref info) :tyvar) :unit =
	    let val printname = tyvarPrintname tv
	     in case info
		  of OPEN{depth,eq,kind} =>
		       (case kind
			  of FLEX fields =>
			      (case fields
				 of [] => (pps "{"; pps printname; pps "}")
				  | field::fields =>
				      (openHOVBox 1;
				       pps "{";
				       ppField field;
				       app (fn x => (pps ",";
						     break{nsp=1,offset=0};
						     ppField x))
					    fields;
				       pps ";";
				       break{nsp=1,offset=0};
				       pps printname;
				       pps "}";
				       closeBox()))
			   | _ => pps printname)
		   | _ => pps printname
	    end
     in prty ty
    end  (* ppType1 *)

and ppType (env:StaticEnv.staticEnv) ppstrm (ty:ty) : unit = 
      (PP.openHOVBox ppstrm (PP.Rel 1);
       ppType1 env ppstrm (ty,[],NONE);
       PP.closeBox ppstrm)

fun ppDconDomain members (env:StaticEnv.staticEnv)
                 ppstrm (ty:ty) : unit = 
      (PP.openHOVBox ppstrm (PP.Rel 1);
       ppType1 env ppstrm (ty,[],SOME members);
       PP.closeBox ppstrm)

fun ppTycon env ppstrm tyc = ppTycon1 env ppstrm NONE tyc

fun ppTyfun env ppstrm (TYFUN{arity,body}) =
    let val {openHVBox, openHOVBox, closeBox, pps, break,...} = en_pp ppstrm
     in openHOVBox 2;
	pps "TYFUN({arity="; 
	ppi ppstrm arity; ppcomma ppstrm;
	break{nsp=0,offset=0};
	pps "body="; 
	ppType env ppstrm body; 
	pps "})";
        closeBox()
    end

fun ppFormals ppstrm =
    let fun ppF 0 = ()
	  | ppF 1 = pps ppstrm " 'a"
	  | ppF n = (pps ppstrm " ";
		     ppTuple ppstrm (fn ppstrm => fn s => pps ppstrm ("'"^s))
				    (typeFormals n))
     in ppF
    end

fun ppDataconTypes env ppstrm (GENtyc { kind = DATATYPE dt, ... }) =
    let val {index,freetycs,family={members,...},...} = dt
	val {openHVBox, openHOVBox, closeBox, pps, break,...} = en_pp ppstrm
	val {dcons,...} = Vector.sub(members,index)
    in
	openHVBox 0;
	app (fn {name,domain,...} =>
		(pps (Symbol.name name); pps ":";
		 case domain
		  of SOME ty =>
		     ppType1 env ppstrm (ty,[],SOME (members,freetycs))
		   | NONE => pps "CONST";
		 break{nsp=1,offset=0}))
	    dcons;
	    closeBox()
    end
  | ppDataconTypes env ppstrm _ = bug "ppDataconTypes"

end (* toplevel local *)
end (* structure PPType *)
