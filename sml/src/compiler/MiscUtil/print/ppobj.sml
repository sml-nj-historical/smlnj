(* ppobj.sml
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories.
 *)


signature PPOBJ = 
sig
  type object
  val ppObj : StaticEnv.staticEnv
              -> PrettyPrint.ppstream
                 -> object * Types.ty * int
                    -> unit
  val debugging : bool ref
end


structure PPObj : PPOBJ =
struct

structure PP = PrettyPrint
structure V = Vector
structure A = Access
structure T = Types
structure TU = TypesUtil
structure BT = BasicTypes
structure F = Fixity
structure Obj = Unsafe.Object

open PrettyPrint PPUtil

(* debugging *)
val say = Control.Print.say
val debugging = ref false
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("PPObj: "^msg)


type object = Obj.object

fun gettag obj = Obj.toInt (V.sub(Obj.toTuple obj, 0))

exception Switch

(*
fun switch(obj, GENtyc{kind=DATATYPE{index,family={members,...},...},...}) =
    let val {tycname,dcons,...} = List.nth(members,index)
*)
fun switch(obj, dcons) = let
      fun chk (f, tag : int) =
	    (f obj = tag) handle Obj.Representation => false
      fun try ((d as {name,rep,domain})::r) = (case rep
	     of A.TAGGED i =>
		  if chk(gettag, i) then d else try r
	      | A.CONSTANT i =>
		  if chk(Obj.toInt, i) then d else try r
	      | A.TRANSPARENT => d
	      | A.UNTAGGED => if Obj.boxed obj then d else try r
	      | A.REF => d
	      | A.LISTCONS => if (Obj.boxed obj) then d else try r
	      | A.LISTNIL => if chk(Obj.toInt, 0) then d else try r
              | A.SUSP _ => d (* ZIDO *)           
	      | _ => bug "switch: funny datacon"
	    (* end case *))
	| try [] = bug "switch: none of the datacons matched"
      in
	try dcons
      end

(** a temporary hack for printing UNTAGGEDREC objects *)
fun isRecTy (T.VARty(ref (T.INSTANTIATED t))) = isRecTy t
  | isRecTy (T.CONty(T.RECORDtyc _, _::_)) = true
  | isRecTy _ = false

fun isUbxTy (T.VARty(ref (T.INSTANTIATED t))) = isUbxTy t
  | isUbxTy (T.CONty(tc as T.GENtyc _, [])) =
      (TU.eqTycon(tc, BT.int32Tycon)) orelse 
      (TU.eqTycon(tc, BT.word32Tycon))
  | isUbxTy _ = false

fun decon(obj, {rep,name,domain}) = (case rep
      of A.UNTAGGED => 
           (case domain 
             of SOME t => 
                 if (isRecTy t) orelse (isUbxTy t)
                 then obj else V.sub(Obj.toTuple obj, 0)
              | _ => bug "decon -- unexpected conrep-domain")

       | A.TAGGED _ => V.sub(Obj.toTuple obj,1)
(*     | A.TAGGEDREC _ =>
	   let (* skip first element, i.e. discard tag *)
	       val a = tuple obj
	       fun f i =
		   if i < V.length a
		   then V.sub(a,i) :: f(i+1)
		   else []
	    in U.cast (V.fromList (f(1)))
	   end
*)
       | A.CONSTANT _ => Obj.toObject ()
       | A.TRANSPARENT => obj
       | A.REF => !(Obj.toRef obj)
       | A.EXN _ => V.sub(Obj.toTuple obj,0)
       | A.LISTCONS => obj 
       | A.LISTNIL => bug "decon - constant datacon in decon"
       | A.SUSP _ => obj
  (* end case *))

val noparen = F.INfix(0,0)

(* define listDcons and ulistDcons *)
val T.GENtyc{kind=T.DATATYPE{family={members= #[{dcons=listDcons,...}],...},...},...} =
    BT.listTycon
val T.GENtyc{kind=T.DATATYPE{family={members= #[{dcons=ulistDcons,...}],...},...},...} =
    BT.ulistTycon

local
  (* counter to generate identifier *)
  val cpt = ref 0

  (* test membership in an association list and gives back 
   * the second element *)
  fun mem a =
      let fun m [] = NONE | m ((x,r)::l) = if a = x then SOME r else m l
       in m
      end

  (* verifies if an object has been seen and if yes, gives back its
   * identification number, creating a new one if necessary *)
  fun isSeen obj l =
      let val obj' = Unsafe.cast obj : unit ref
       in case mem obj' l 
	    of NONE => (false,0)
	     | SOME (r as ref NONE) => let
		val id = !cpt
		in cpt := id+1; r := SOME id; (true,id) end
	     | SOME (ref (SOME id)) => (true,id)
      end

in

(* reset the identifier counter *)
fun initCpt () = cpt := 0

(* print with sharing if necessary. The "printer" already knows the 
   ppstream.      *)
fun printWithSharing ppstrm (obj,accu,printer) = 
    if !Control.Print.printLoop then
      let val (seen,nb) = isSeen obj accu
       in if seen then
	     (PP.add_string ppstrm "%";
	      PP.add_string ppstrm (Int.toString nb))
	  else let val modif = ref NONE
		   val nlAccu = (Unsafe.cast obj : unit ref,modif) :: accu
	        in printer (obj,nlAccu);
		   case !modif 
		     of NONE => () 
		      | SOME i => (PP.add_string ppstrm " as %";
				   PP.add_string ppstrm (Int.toString i))
	       end
      end
    else printer (obj,accu)

end (* local *)

fun interpArgs(tys,NONE) = tys
  | interpArgs(tys,SOME (members,freetycs)) = 
    let fun subst(T.CONty(T.RECtyc n,args)) =
	      let val tyc' = (List.nth(members,n)
	                     handle Subscript => bug "interpArgs 1")
	       in T.CONty(tyc', map subst args)
	      end
	  | subst(T.CONty(T.FREEtyc n,args)) =
	      let val tyc' = (List.nth(freetycs,n)
	                     handle Subscript => bug "interpArgs 2")
	       in T.CONty(tyc', map subst args)
	      end
	  | subst(T.CONty(tyc,args)) = T.CONty(tyc, map subst args)
	  | subst(T.VARty(ref(T.INSTANTIATED ty))) = subst ty
	  | subst ty = ty
     in map subst tys
    end

fun transMembers(stamps: Stamps.stamp vector, 
                 freetycs: T.tycon list, root,
                 family as {members,...} : T.dtypeFamily) = 
    let fun dtmemberToTycon(n, {tycname,arity,dcons,eq,sign}, l) =
	      T.GENtyc{stamp=Vector.sub(stamps,n),arity=arity,eq=ref(T.YES),
		       path=InvPath.IPATH[tycname], 
		       kind=T.DATATYPE{index=n,
                              stamps=stamps, freetycs=freetycs, root=root,
                              family=family}}  :: l
     in (Vector.foldri dtmemberToTycon nil (members,0,NONE), 
         freetycs)
    end


(* main function: ppObj: staticEnv -> ppstream -> (object * ty * int) -> unit *)

fun ppObj env ppstrm =
let fun ppValue (obj: object, ty: T.ty, depth: int) : unit =
        ppVal' (obj, ty, NONE, depth, noparen, noparen, [])

    and ppValShare (obj:object, ty:T.ty, membersOp: (T.tycon list * T.tycon list) option,
		    depth:int, accu) =
        ppVal' (obj, ty, membersOp, depth, noparen, noparen, accu)

    and ppVal' (_,_,_,0,_,_,_) = add_string ppstrm  "#"
      | ppVal' (obj: object, ty: T.ty, membersOp: (T.tycon list * T.tycon list) option, 
                depth: int, l: F.fixity, r: F.fixity, accu) : unit =
       ((case ty
	  of T.VARty(ref(T.INSTANTIATED t)) =>
	       ppVal'(obj,t,membersOp,depth,r,l,accu)
	   | T.POLYty{tyfun=T.TYFUN{body,arity},...} =>
              if arity=0
              then  ppVal'(obj, body,membersOp,depth,l,r,accu)
              else
               (let fun zeros n = if n < 1 then [] else 0::(zeros (n-1))
                    val args = Vector.fromList (zeros arity)

(*
                 in (case (TU.headReduceType body) 
                     of T.CONty(T.RECORDtyc _, []) =>
                            bug "unexpected poly type in ppObj"
                      | T.CONty(T.RECORDtyc _, _) =>
                            add_string ppstrm "<poly-record>"
                      | _ => 
                            let val tobj : int Vector.vector -> object 
                                       = Unsafe.cast obj
                                val res = tobj args
                             in ppVal'(res, body,membersOp,depth,l,r,accu)
                            end)
                end)
*)
                    val tobj : int Vector.vector -> object 
                                = Unsafe.cast obj
                    val res = tobj args
                 in ppVal'(res, body,membersOp,depth,l,r,accu)
                end)
	

	   | T.CONty(tyc as T.GENtyc{kind=T.PRIMITIVE _,...}, argtys) =>
	      let fun ppWord s = PP.add_string ppstrm ("0wx"^s)
	      in
	        if TU.eqTycon(tyc,BT.intTycon) then
		  add_string ppstrm (Int.toString(Obj.toInt obj))
		else if TU.eqTycon(tyc,BT.int32Tycon) then
		  add_string ppstrm (Int32.toString(Obj.toInt32 obj))
	        else if TU.eqTycon(tyc,BT.wordTycon) then 
		  ppWord (Word.toString(Obj.toWord obj))
	        else if TU.eqTycon(tyc,BT.word8Tycon) then 
		  ppWord (Word8.toString(Obj.toWord8 obj))
	        else if TU.eqTycon(tyc,BT.word32Tycon) then 
		  ppWord (Word32.toString(Obj.toWord32 obj))
	        else if TU.eqTycon(tyc,BT.realTycon) then
		  add_string ppstrm (Real.toString(Obj.toReal obj))
	        else if TU.eqTycon(tyc,BT.stringTycon) then
		  PPUtil.pp_mlstr ppstrm (Obj.toString obj)
	        else if TU.eqTycon(tyc,BT.charTycon) then
		  (add_string ppstrm "#";
		   PPUtil.pp_mlstr ppstrm (String.str(Char.chr(Obj.toInt obj))))
	        else if TU.eqTycon(tyc,BT.arrowTycon) then add_string ppstrm  "fn"
	        else if TU.eqTycon(tyc,BT.exnTycon) then
		 let val name = General.exnName(Obj.toExn obj)
		  in add_string ppstrm name;
		     add_string ppstrm "(-)"
		 end
	        else if TU.eqTycon(tyc,BT.contTycon) then add_string ppstrm  "cont"
	        else if TU.eqTycon(tyc,BT.vectorTycon) then 
		  ppVector(Obj.toTuple obj, hd argtys, membersOp, depth,
			  !Control.Print.printLength, accu)
		  handle Obj.Representation => add_string ppstrm  "prim?"
	        else if TU.eqTycon(tyc,BT.arrayTycon) then
		  (printWithSharing ppstrm
		    (obj,accu,
		     fn (obj,accu) =>
			  ppArray(Obj.toArray obj, hd argtys, membersOp, depth,
				   !Control.Print.printLength, accu))
		  handle Obj.Representation => add_string ppstrm  "prim?")
	        else add_string ppstrm  "prim?"
	      end
	   | T.CONty(T.GENtyc{kind=T.DATATYPE _,stamp,eq=ref(T.ABS),...}, _) => 
	       (PPTable.pp_object ppstrm stamp obj 
		handle PP_NOT_INSTALLED => add_string ppstrm  "-" )
	   | T.CONty(tyc as T.GENtyc{kind=T.DATATYPE{index,stamps,
                     family as {members,...}, freetycs, root},...},
		     argtys) =>
	       if TU.eqTycon(tyc,BT.ulistTycon) then
	         ppUrList(obj,hd argtys,membersOp,depth,
			  !Control.Print.printLength,accu)
	       else if TU.eqTycon(tyc,BT.suspTycon) then 
                 add_string ppstrm  "$$"  (* ZIDO *)
               else if TU.eqTycon(tyc,BT.listTycon) then
		 ppList(obj,hd argtys,membersOp,depth,
			!Control.Print.printLength,accu)
	       else if TU.eqTycon(tyc,BT.refTycon) then
		 (printWithSharing ppstrm
		   (obj,accu,
		    let val argtys' = interpArgs(argtys,membersOp)
		     in fn (obj,accu) => ppDcon(obj,
                          (Vector.sub(stamps,index),Vector.sub(members,index)),
  			   SOME([BT.refTycon],[]),argtys',depth,l,r,accu)
		    end))
	       else let val argtys' = interpArgs(argtys,membersOp)
		     in ppDcon(obj,(Vector.sub(stamps,index),
                                    Vector.sub(members,index)),
                               SOME(transMembers (stamps, freetycs, 
                                                  root, family)),
			       argtys',depth,l,r,accu)
		    end
	   | T.CONty(tyc as T.RECORDtyc [], _) => add_string ppstrm  "()"
	   | T.CONty(tyc as T.RECORDtyc labels, argtys) =>
	       if Tuples.isTUPLEtyc tyc
	       then ppTuple(Obj.toTuple obj, argtys, membersOp, depth, accu)
	       else ppRecord(Obj.toTuple obj, labels, argtys, membersOp, depth, accu)
	   | T.CONty(tyc as T.DEFtyc _, _) => 
	       ppVal'(obj, TU.reduceType ty, membersOp, depth, l, r,accu)
	   | T.CONty(tyc as T.RECtyc i,argtys) =>
	       (case membersOp
		  of SOME (memberTycs,_) => 
		      let val tyc' =
			      List.nth(memberTycs,i)
			      handle Subscript =>
			       (flush_ppstream ppstrm;
				print "#ppVal':  ";
				print (Int.toString i);
				print " "; print(Int.toString(length memberTycs));
				print "\n";
				bug "ppVal': bad index for RECtyc")
		       in case tyc'
			    of T.GENtyc{kind=T.DATATYPE{index,stamps,
                                   family={members,...},...},...} =>
				  ppDcon(obj,(Vector.sub(stamps,index),
                                              Vector.sub(members,index)),
                                         membersOp, argtys,
					 depth,l,r,accu)
			     | _ => bug "ppVal': bad tycon in members"
		      end
		   | NONE => bug "ppVal': RECtyc with no members")

	   | T.CONty(tyc as T.FREEtyc i,argtys) =>
	       (case membersOp
		  of SOME (_, freeTycs) => 
		      let val tyc' =
			      List.nth(freeTycs,i)
			      handle Subscript =>
			       (flush_ppstream ppstrm;
				print "#ppVal':  ";
				print (Int.toString i);
				print " "; 
                                print(Int.toString(length freeTycs));
				print "\n";
				bug "ppVal': bad index for FREEtyc")
		       in ppVal'(obj, T.CONty(tyc', argtys), membersOp, 
                                 depth, l, r, accu)
		      end
		   | NONE => bug "ppVal': RECtyc with no members")

	   | _ => add_string ppstrm  "-")
	handle e => raise e)

and ppDcon(_,_,_,_,0,_,_,_) = add_string ppstrm  "#"
  | ppDcon(obj:object, (stamp, {tycname,dcons,...}), membersOp : (T.tycon list * T.tycon list) option,
	   argtys, depth:int, l:F.fixity, r:F.fixity, accu) =
     PPTable.pp_object ppstrm stamp obj
	   (* attempt to find and apply user-defined pp on obj *)
     handle PP_NOT_INSTALLED => 
       if length dcons = 0 then add_string ppstrm "-"
       else
	let val dcon as {name,domain,...} = switch(obj,dcons)
	    val dname = Symbol.name name
	 in case domain
	      of NONE => add_string ppstrm dname
	       | SOME dom =>
		  let val fixity = 
		          Lookup.lookFix(env,Symbol.fixSymbol dname)
		      (* (??) may be inaccurate *)
		  val dom = TU.applyTyfun(T.TYFUN{arity=length argtys,body=dom},
					  argtys)
		  val dom = TU.headReduceType dom (* unnecessary *)
		  fun prdcon() =
		      case (fixity,dom)
			of (F.INfix _,T.CONty(domTyc as T.RECORDtyc _, [tyL,tyR])) =>
			   let val twoTuple = Obj.toTuple(decon(obj,dcon))
			    in if Tuples.isTUPLEtyc domTyc
			       then (begin_block ppstrm INCONSISTENT 0;
				     ppVal'(V.sub(twoTuple,0),tyL,
					    membersOp,
					    depth-1,F.NONfix,fixity,accu);
				     add_break ppstrm (1,0);
				     add_string ppstrm  dname;
				     add_break ppstrm (1,0);
				     ppVal'(V.sub(twoTuple,1),tyR,
					    membersOp,
					    depth-1,fixity, F.NONfix,accu);
				     end_block ppstrm)
			       else (begin_block ppstrm INCONSISTENT 2;
				     add_string ppstrm  dname;
				     add_break ppstrm (1,0);
				     ppVal'(decon(obj,dcon),dom,
					    membersOp, depth-1,
					    F.NONfix,F.NONfix,accu);
				     end_block ppstrm)
			   end
			 | _ => (begin_block ppstrm INCONSISTENT 2;
				 add_string ppstrm  dname; add_break ppstrm (1,0);
				 ppVal'(decon(obj,dcon),dom,membersOp,depth-1,
					F.NONfix,F.NONfix,accu);
				 end_block ppstrm)
                  fun prpardcon() =
		      (begin_block ppstrm INCONSISTENT 0;
		       add_string ppstrm  "("; prdcon(); add_string ppstrm  ")";
		       end_block ppstrm)
	       in case(l,r,fixity)
		    of (F.NONfix,F.NONfix,_) => prpardcon()
		     | (F.INfix _,F.INfix _,_) => prdcon()
		       (* special case: only on first iteration, for no parens *)
		     | (_,_,F.NONfix) => prdcon()
		     | (F.INfix(_,p1),_,F.INfix(p2,_)) =>
			 if p1 >= p2 then prpardcon()
			 else prdcon()
		     | (_,F.INfix(p1,_),F.INfix(_,p2)) =>
			 if p1 > p2 then prpardcon()
			 else prdcon()
	      end
      end

and ppList(obj:object, ty:T.ty, membersOp, depth:int, length: int,accu) =
    let fun list_case p =
	    case switch(p, listDcons)
	      of {domain=NONE,...} => NONE
	       | dcon => let val pair = Obj.toTuple(decon(p, dcon))
			  in SOME(V.sub(pair,0),V.sub(pair,1))
			 end
       
       fun ppTail(p, len) =
	   case list_case p
	     of NONE => ()
	      | SOME(hd,tl) => 
		  if len <= 0 then (add_string ppstrm  "...")
		  else (case list_case tl
			 of NONE => 
			      ppValShare (hd, ty, membersOp, depth-1,accu)
			  | _ =>
			      (ppValShare (hd, ty, membersOp, depth-1,accu);
			       add_string ppstrm  ",";
			       add_break ppstrm (0,0);
			       ppTail(tl,len-1)))

     in begin_block ppstrm INCONSISTENT 1;
        add_string ppstrm  "["; 
        ppTail(obj,length);
	add_string ppstrm  "]";
        end_block ppstrm
    end

and ppUrList(obj:object, ty:T.ty, membersOp, depth:int, length: int,accu) =
    let fun list_case p =
	    case switch(p, ulistDcons)
	      of {domain=NONE,...} => NONE
	       | dcon => let val pair = Obj.toTuple(decon(p, dcon))
			  in SOME(V.sub(pair,0),V.sub(pair,1))
			 end
       
        fun ppTail(p, len) =
	   case list_case p
	     of NONE => ()
	      | SOME(hd,tl) => 
		  if len <= 0 then (add_string ppstrm  "...")
		  else (case list_case tl
			 of NONE => 
			      ppValShare (hd, ty, membersOp, depth-1,accu)
			  | _ =>
			      (ppValShare (hd, ty, membersOp, depth-1,accu);
			       add_string ppstrm  ",";
	      	               add_break ppstrm (0,0);
			       ppTail(tl,len-1)))

     in begin_block ppstrm INCONSISTENT 1;
        add_string ppstrm  "[ unrolled list "; 
        (* ppTail(obj,length); *)
	add_string ppstrm  "]";
        end_block ppstrm
    end

and ppTuple(objs: object vector, tys: T.ty list, membersOp, depth:int, accu) : unit =
    let fun ppFields(nf,[ty]) =
	      ppValShare (V.sub(objs,nf),ty,membersOp,depth-1,accu)
	  | ppFields(nf, ty::restty) = 
	      (ppValShare (V.sub(objs,nf),ty,membersOp,depth-1,accu);
               add_string ppstrm (",");
               add_break ppstrm (0,0);
	       ppFields(nf+1,restty))
	  | ppFields(nf,[]) = ()
     in begin_block ppstrm INCONSISTENT 1;
        add_string ppstrm ("("); 
        ppFields(0,tys); 
        add_string ppstrm (")");
        end_block ppstrm
    end

and ppRecord(objs: object vector, labels: T.label list,
	     tys: T.ty list, membersOp, depth: int, accu) =
    let fun ppFields(nf,[l],[ty]) = 
	      (begin_block ppstrm CONSISTENT 2;
               add_string ppstrm (Symbol.name l); 
               add_string ppstrm ("="); 
               ppValShare (V.sub(objs,nf),ty,membersOp,depth-1,accu);
               end_block ppstrm)
	  | ppFields(nf, l::restl, ty::restty) = 
	      (begin_block ppstrm CONSISTENT 2;
               add_string ppstrm (Symbol.name l); 
               add_string ppstrm ("="); 
               ppValShare (V.sub(objs,nf),ty,membersOp,depth-1,accu);
               end_block ppstrm;
	       add_string ppstrm (","); 
               add_break ppstrm (0,0);
               ppFields(nf+1,restl,restty))
	  | ppFields(nf,[],[]) = ()
          | ppFields _ = bug "ppFields in ppval.sml"
     in begin_block ppstrm INCONSISTENT 1;
        add_string ppstrm ("{"); 
        ppFields(0,labels,tys); 
        add_string ppstrm ("}");
        end_block ppstrm
    end

and ppVector(objs:object vector, ty:T.ty, membersOp, depth:int, length,accu) =
      let val vectorLength  = V.length objs
          val (len, closing) = 
	        if length >= vectorLength then 
		  (vectorLength,fn _ => add_string ppstrm "]")
		else (length,fn sep => (add_string ppstrm sep; 
                                        add_string ppstrm "...]"))
          fun printRest(sep,breaker, index) =
	        if index >= len then closing sep
                else (add_string ppstrm  sep; breaker ();
		      ppValShare (V.sub (objs,index),ty,membersOp,
				  depth-1,accu);
		      printRest (",",fn () => add_break ppstrm (0,0), index + 1))
       in begin_block ppstrm INCONSISTENT 1;
	  add_string ppstrm "#["; printRest("",fn () => (), 0);
          end_block ppstrm
      end

and ppArray (objs : object array, ty:T.ty, membersOp, depth:int, length,accu) =
      let val vectorLength  = Array.length objs
          val (len, closing) = 
	        if length >= vectorLength then 
		  (vectorLength,fn _ => add_string ppstrm "|]")
		else (length,fn sep => (add_string ppstrm sep; 
                                        add_string ppstrm "...|]"))
          fun printRest(sep,breaker, index) =
	        if index >= len then closing sep
                else (add_string ppstrm  sep; breaker ();
		      ppValShare (Array.sub (objs,index),ty,membersOp,
				  depth-1,accu);
		      printRest (",",fn () => add_break ppstrm (0,0), index + 1))
       in begin_block ppstrm INCONSISTENT 1;
	  add_string ppstrm "[|"; printRest("",fn () => (), 0);
          end_block ppstrm
      end
 in ppValue
end (* fun ppObj *)

end (* structure PPObj *)



(*
 * $Log: ppobj.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:16  george
 * Version 110.5
 *
 *)
