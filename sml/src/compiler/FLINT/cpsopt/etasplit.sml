(* Copyright 1996 by Bell Laboratories *)
(* cps/etasplit.sml *)

(* 
 * Perform the eta-split transformation on cps expressions.  The
 * purpose of the eta split transformation is to give two entry points
 * to functions which both escape and which are called at known
 * points.  The function is split into two functions: a known function
 * that is used for calls; and a strictly escaping function that is
 * used for all escaping occurrences of the original function.  The
 * new escaping function simply calls the new known function.
 *
 * I do not bother to split known functions, or functions that only
 * escape.  Furthermore, no continuations are split.  I expect that
 * the majority of continuations are escaping, except for a few known
 * continuations that were created for reasons of space complexity (as
 * the join of two branches, for example).  I doubt there are many
 * continuations which both escape and have known calls. (Trevor Jim)
 *
 *)

signature ETASPLIT =
  sig val etasplit : {function: CPS.function,
		      table: LtyDef.lty IntHashTable.hash_table,
		      click: string -> unit} -> CPS.function
  end (* signature ETASPLIT *)

functor EtaSplit(MachSpec : MACH_SPEC) : ETASPLIT =
struct

local open CPS 
      structure LV = LambdaVar
in

fun sameName(x,VAR y) = LV.sameName(x,y) 
  | sameName _ = ()

fun etasplit{function=(fkind,fvar,fargs,ctyl,cexp),
	     table=typtable,
	     click} =
let

val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say s else ()
fun debugflush() = if debug then Control.Print.flush() else ()
val rep_flag = MachSpec.representations
val type_flag = (!Control.CG.checkcps1) andalso rep_flag


exception SPLIT1
fun getty v = 
  if type_flag 
  then (IntHashTable.lookup typtable v) handle _ =>
                (Control.Print.say ("SPLIT1: Can't find the variable "^
                            (Int.toString v)^" in the typtable ***** \n");
                 raise SPLIT1)
  else LtyExtern.ltc_void

fun addty(f,t) = if type_flag then IntHashTable.insert typtable (f,t) else ()
fun copyLvar v = let val x = LV.dupLvar(v)
                  in (addty(x,getty v); x)
                 end

local exception SPLIT2
      val m : value IntHashTable.hash_table = IntHashTable.mkTable(32, SPLIT2)
in  fun makealias x = (sameName x; IntHashTable.insert m x)
    fun alias (VAR v) = (SOME(IntHashTable.lookup m v) handle SPLIT2 => NONE)
      | alias _ = NONE
end

local exception SPLIT3
      val m : {used : int ref, called : int ref} IntHashTable.hash_table =
		 IntHashTable.mkTable(32,SPLIT3)
in  val get = IntHashTable.lookup m
    fun enterFN(_,f,_,_,_) =
	IntHashTable.insert m (f,{used=ref 0,called=ref 0})
      (* Perhaps I shouldn't bother to enterFN continuations... *)
    fun use (VAR v) =
      (let val {used=u,...} = get v
       in  u := !u+1
       end handle SPLIT3 => ())
      | use _ = ()
    fun call (VAR v) =
      (let val {used=u,called=c} = get v
       in  u := !u+1; c := !c+1
       end handle SPLIT3 => ())
      | call _ = ()
end

(* Get usage information and mark whether or not we will be doing
   any splits. *)
val found_split = ref false
val rec pass1 = 
 fn RECORD(_,vl,_,e) => (app (use o #1) vl; pass1 e)
  | SELECT(_,v,_,_,e) => (use v; pass1 e)
  | OFFSET(_,v,_,e) => (use v; pass1 e)
  | SWITCH(v,_,el) => (use v; app pass1 el)
  | BRANCH(_,vl,_,e1,e2) => (app use vl; pass1 e1; pass1 e2)
  | SETTER(_,vl,e) => (app use vl; pass1 e)
  | LOOKER(_,vl,_,_,e) => (app use vl; pass1 e)
  | ARITH(_,vl,_,_,e) => (app use vl; pass1 e)
  | PURE(_,vl,_,_,e) => (app use vl; pass1 e)
  | RCC(_,_,_,vl,_,e) => (app use vl; pass1 e)
  | APP(f, vl) => (call f; app use vl)
  | FIX(l, e) =>
      let (* Any changes to dosplit had better be reflected here. *)
	  fun checksplit nil = ()
	    | checksplit ((CONT,_,_,_,_)::tl) = checksplit tl
	    | checksplit ((_,f,_,_,_)::tl) =
	        let val {used=ref u,called=ref c} = get f
		in  if u<>c andalso c<>0
			then found_split := true
		    else checksplit tl
		end
      in  app enterFN l;
	  app (fn (_,_,_,_,body) => pass1 body) l;
	  pass1 e;
	  if !found_split then () else checksplit l
      end

val rec reduce = 
   fn RECORD(k,vl,w,e) => RECORD(k, vl, w, reduce e)
    | SELECT(i,v,w,t,e) => SELECT(i, v, w, t, reduce e)
    | OFFSET(i,v,w,e) => OFFSET(i, v, w, reduce e)
    | SWITCH(v,c,el) => SWITCH(v, c,map reduce el)
    | BRANCH(i,vl,c,e1,e2) =>
          BRANCH(i, vl, c, reduce e1, reduce e2)
    | LOOKER(i,vl,w,t,e) => LOOKER(i, vl, w, t, reduce e)
    | ARITH(i,vl,w,t,e) => ARITH(i, vl, w, t, reduce e)
    | PURE(i,vl,w,t,e) => PURE(i, vl, w, t, reduce e)
    | SETTER(i,vl,e) => SETTER(i, vl, reduce e)
    | RCC(k,l,p,vl,wtl,e) => RCC(k, l, p, vl, wtl, reduce e)
    | (e as APP(f,vl)) => 
       (case alias f
	  of NONE => e
	   | SOME f' => APP(f',vl))
    | FIX(l,e) =>
	let fun dosplit nil = nil
	      | dosplit ((hd as (ESCAPE,f,vl,cl,body))::tl) =
		  let val {used=ref u,called=ref c} = get f
		  in  if u<>c andalso c<>0
		      then (* Function escapes AND has known call sites *)
		      let val f' = copyLvar f
			  val vl' = map copyLvar vl
		      in  click "S";
			  makealias(f,VAR f');
			  (NO_INLINE_INTO,f,vl',cl,APP(VAR f',map VAR vl'))::
			  (ESCAPE,f',vl,cl,body)::
			  (dosplit tl)
		      end
		      else hd::(dosplit tl)
		  end
	      | dosplit (hd::tl) = hd::(dosplit tl)
	    val l' = dosplit l
		(* Could check for NO_INLINE_INTO in reduce_body, so
		   that we don't reduce in the body of something we've
		   just split; but we might be using NO_INLINE_INTO
		   for something else (e.g. UNCURRY). *)
	    fun reduce_body (fk,f,vl,cl,body) = (fk,f,vl,cl,reduce body)
	in  FIX(map reduce_body l',reduce e)
	end

in  (* body of etasplit *)

    debugprint "Etasplit: ";
    pass1 cexp;
    (if !found_split
	 then (fkind, fvar, fargs, ctyl, reduce cexp)
     else (fkind, fvar, fargs, ctyl, cexp))
    before debugprint "\n"

end (* fun etasplit *)

end (* toplevel local *)
end (* functor EtaSplit *)


