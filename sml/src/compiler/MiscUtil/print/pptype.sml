(* Copyright 1991 by AT&T Bell Laboratories *)

signature PPTYPE = 
sig
  val typeFormals : int -> string list
  val tyvarPrintname : Types.tyvar -> string
  val ppTycon : StaticEnv.staticEnv -> PrettyPrint.ppstream 
                -> Types.tycon -> unit
  val ppTyfun : StaticEnv.staticEnv -> PrettyPrint.ppstream 
                -> Types.tyfun -> unit 
  val ppType  : StaticEnv.staticEnv -> PrettyPrint.ppstream 
                -> Types.ty -> unit
  val ppDconDomain : (Types.dtmember vector * Types.tycon list) 
                     -> StaticEnv.staticEnv 
                     -> PrettyPrint.ppstream -> Types.ty -> unit
  val ppDataconTypes : StaticEnv.staticEnv -> PrettyPrint.ppstream 
                -> Types.tycon -> unit
  val resetPPType : unit -> unit
  val ppFormals : PrettyPrint.ppstream -> int -> unit

  val debugging : bool ref
  val unalias : bool ref

end (* signature PPTYPE *)

structure PPType : PPTYPE =
struct

local 
      structure SP = SymPath
      structure BT = BasicTypes
      structure T = Types
      structure TU = TypesUtil
      structure PP = PrettyPrint
      open Types PPUtil  
in

val debugging = ref false
val unalias = ref true

fun bug s = ErrorMsg.impossible ("PPType: " ^ s)
val pps = PP.add_string

fun C f x y = f y x

val internals = Control.internals

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

fun tyvarPrintname (tyvar as ref info) =
    case info
      of INSTANTIATED(VARty(tyvar)) => tyvarPrintname tyvar
       | OPEN{depth,eq,kind} =>
	  tvHead(eq, annotate(metaTyvarName tyvar,
			      case kind of META => "M" | FLEX _ => "F",
			      SOME depth))
       | UBOUND{name,depth,eq} =>
	  tvHead(eq,annotate(Symbol.name name,"U",SOME depth))
       | LITERAL{kind,...} =>
	  annotate(litKindPrintName kind,"L",NONE)
       | SCHEME eq =>
	  tvHead(eq,annotate(metaTyvarName tyvar,"S",NONE))
       | LBOUND _ => "<LBOUND tyvar>"
(*
fun ppkind ppstrm kind =
    pps ppstrm
      (case kind
	 of PRIMITIVE _ => "PRIMITIVE" | FORMAL => "FORMAL"
          | FLEXTYC _ => "FLEXTYC" | ABSTRACT _ => "ABSTYC"
	  | DATATYPE _ => "DATATYPE" | TEMP => "TEMP")
*)
fun ppkind ppstrm kind =
    pps ppstrm
      (case kind
	 of PRIMITIVE _ => "P" | FORMAL => "F"
          | FLEXTYC _ => "X" | ABSTRACT _ => "A"
	  | DATATYPE _ => "D" | TEMP => "T")

fun effectivePath(path,tyc,env) : string =
    let fun tycPath (GENtyc{path,...}) = SOME path
	  | tycPath (DEFtyc{path,...}) = SOME path
	  | tycPath (PATHtyc{path, ...}) = SOME path
	  | tycPath _ = NONE
	fun find(path,tyc) =
	    (findPath(path,
		(fn tyc' => TU.equalTycon(tyc',tyc)),
		(fn x => Lookup.lookTyc(env,x,(fn _ => raise Env.Unbound)))))
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

val GENtyc{stamp=arrowStamp,...} = BT.arrowTycon

fun strength(ty) =
    case ty
      of VARty(ref(INSTANTIATED ty')) => strength(ty')
       | CONty(tycon, args) =>
	   (case tycon
 	      of GENtyc{kind=PRIMITIVE _, stamp,...} => 
		   if Stamps.eq(stamp,arrowStamp) then 0 else 2
	       | RECORDtyc (_::_) =>  (* excepting type unit *)
		   if Tuples.isTUPLEtyc(tycon) then 1 else 2
	       | _ => 2)
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
    PP.add_string ppstream (SymPath.toString (SymPath.SPATH(rev path)))

fun ppTycon1 env ppstrm membersOp =
    let val {begin_block,end_block,pps,add_break,...} = en_pp ppstrm
	fun ppTyc (tyc as GENtyc{path,stamp,eq,kind,...}) =
	     if !internals
	     then (begin_block PP.INCONSISTENT 1;
		    ppInvPath ppstrm path;
		    pps "[";
		    pps "G"; ppkind ppstrm kind; pps ";"; 
		    pps (Stamps.stampToShortString stamp);
		    pps ";";
		    ppEqProp ppstrm (!eq);
		    pps "]";
		   end_block())
	     else pps(effectivePath(path,tyc,env))
	  | ppTyc(tyc as DEFtyc{path,tyfun=TYFUN{body,...},...}) =
	     if !internals
	     then (begin_block PP.INCONSISTENT 1;
		    ppInvPath ppstrm path;
		    pps "["; pps "D;"; 
		    ppType env ppstrm body;
		    pps "]";
		   end_block())
	     else pps(effectivePath(path,tyc,env))
	  | ppTyc(RECORDtyc labels) =
	      ppClosedSequence ppstrm 
		{front=C PP.add_string "{",
		 sep=fn ppstrm => (PP.add_string ppstrm ","; 
				   PP.add_break ppstrm (0,0)),
		 back=C PP.add_string "}",
		 style=PP.INCONSISTENT,
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
	     then (begin_block PP.INCONSISTENT 1;
		    ppInvPath ppstrm path; pps "[P;"; 
		    pps (EntPath.entPathToString entPath);
		    pps "]";
		   end_block())
	     else ppInvPath ppstrm path

	  | ppTyc ERRORtyc = pps "[E]"
     in ppTyc
    end


and ppType1 env ppstrm (ty: ty, sign: T.polysign, 
                        membersOp: (T.dtmember vector * T.tycon list) option) : unit =
    let val {begin_block,end_block,pps,add_break,add_newline} = en_pp ppstrm
        fun prty ty =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => prty(ty')
	       | VARty(tv) => ppTyvar tv
	       | IBOUND n =>
		   let val eq = List.nth(sign,n) 
		                handle Subscript => false
		    in pps (tvHead(eq,(boundTyvarName n)))
		   end
	       | CONty(tycon, args) =>
		   (case tycon
		      of GENtyc{kind=PRIMITIVE _, stamp,...} => 
			   if Stamps.eq(stamp,arrowStamp)
			   then case args
			         of [domain,range] =>
				    (begin_block PP.CONSISTENT 0;
				     if strength domain = 0
				     then (begin_block PP.CONSISTENT 1;
					   pps "(";
					   prty domain;
					   pps ")";
					   end_block())
				     else prty domain;
				     add_break(1,0);
				     pps "-> ";
				     prty range;
				     end_block())
				  | _ => bug "CONty:arity"
			   else (begin_block PP.INCONSISTENT 2;
                                 ppTypeArgs args;
                                 add_break(0,0);
                                 ppTycon1 env ppstrm membersOp tycon;
                                 end_block())
		       | RECORDtyc labels =>
			   if Tuples.isTUPLEtyc(tycon)
			   then ppTUPLEty args
			   else ppRECORDty(labels, args)
		       | _ =>
			   (begin_block PP.INCONSISTENT 2;
			    ppTypeArgs args; 
			    add_break(0,0);
			    ppTycon1 env ppstrm membersOp tycon;
			    end_block()))
	       | POLYty{sign,tyfun=TYFUN{arity,body}} => 
                        ppType1 env ppstrm (body,sign, membersOp)
	       | WILDCARDty => pps "_"
	       | UNDEFty => pps "undef"

	and ppTypeArgs [] = ()
	  | ppTypeArgs [ty] = 
	     (if strength ty <= 1
	      then (begin_block PP.INCONSISTENT 1;
                    pps "("; 
                    prty ty; 
                    pps ")";
                    end_block())
	      else prty ty;
	      add_break(1,0))
	  | ppTypeArgs tys =
              ppClosedSequence ppstrm 
	        {front=C PP.add_string "(",
		 sep=fn ppstrm => (PP.add_string ppstrm ",";
                                   PP.add_break ppstrm (0,0)),
		 back=C PP.add_string ") ",
		 style=PP.INCONSISTENT, 
                 pr=fn _ => fn ty => prty ty}
		tys

	and ppTUPLEty [] = pps "unit"
	  | ppTUPLEty tys = 
	      ppSequence ppstrm
		 {sep = fn ppstrm => (PP.add_break ppstrm (1,0);
				    PP.add_string ppstrm "* "),
		  style = PP.INCONSISTENT,
		  pr = (fn _ => fn ty =>
			  if strength ty <= 1
			  then (begin_block PP.INCONSISTENT 1;
				pps "("; 
				prty ty; 
				pps ")";
				end_block())
			  else prty ty)}
	        tys

	and ppField(lab,ty) = (begin_block PP.CONSISTENT 0;
			       ppSym ppstrm lab; 
			       pps ":";
			       prty ty;
			       end_block())

	and ppRECORDty([],[]) = pps "unit"
	  | ppRECORDty(lab::labels, arg::args) =
	      (begin_block PP.INCONSISTENT 1;
               pps "{";
               ppField(lab,arg);
	       ListPair.app 
		 (fn field => (pps ","; add_break(1,0);ppField field))
		 (labels,args);
               pps "}";
               end_block())
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
				      (begin_block PP.INCONSISTENT 1;
				       pps "{";
				       ppField field;
				       app (fn x => (pps ",";
						     add_break(1,0);
						     ppField x))
					    fields;
				       pps ";";
				       add_break(1,0);
				       pps printname;
				       pps "}";
				       end_block()))
			   | _ => pps printname)
		   | _ => pps printname
	    end
     in prty ty
    end  (* ppType1 *)

and ppType (env:StaticEnv.staticEnv) ppstrm (ty:ty) : unit = 
      (PP.begin_block ppstrm PP.INCONSISTENT 1;
       ppType1 env ppstrm (ty,[],NONE);
       PP.end_block ppstrm)

fun ppDconDomain members (env:StaticEnv.staticEnv)
                 ppstrm (ty:ty) : unit = 
      (PP.begin_block ppstrm PP.INCONSISTENT 1;
       ppType1 env ppstrm (ty,[],SOME members);
       PP.end_block ppstrm)

fun ppTycon env ppstrm tyc = ppTycon1 env ppstrm NONE tyc

fun ppTyfun env ppstrm (TYFUN{arity,body}) =
    let val {begin_block, end_block, pps, add_break,...} = en_pp ppstrm
     in begin_block PP.INCONSISTENT 2;
	pps "TYFUN({arity="; 
	ppi ppstrm arity; add_comma ppstrm;
	add_break(0,0);
	pps "body="; 
	ppType env ppstrm body; 
	pps "})";
        end_block()
    end

fun ppFormals ppstrm =
    let fun ppF 0 = ()
	  | ppF 1 = pps ppstrm " 'a"
	  | ppF n = (pps ppstrm " ";
		     ppTuple ppstrm (fn ppstrm => fn s => pps ppstrm ("'"^s))
				    (typeFormals n))
     in ppF
    end

fun ppDataconTypes env ppstrm (GENtyc{kind=DATATYPE{index,freetycs,family={members,...},...},...}) =
    let val {begin_block, end_block, pps, add_break,...} = en_pp ppstrm
	val {dcons,...} = Vector.sub(members,index)
     in begin_block PP.CONSISTENT 0;
	app (fn {name,domain,...} =>
	       (pps (Symbol.name name); pps ":";
		case domain
		  of SOME ty => ppType1 env ppstrm (ty,[],SOME (members,freetycs))
		   | NONE => pps "CONST";
		add_break(1,0)))
	    dcons;
	end_block()
    end
  | ppDataconTypes env ppstrm _ = bug "ppDataconTypes"

end (* toplevel local *)
end (* structure PPType *)

(*
 * $Log$
 *)
