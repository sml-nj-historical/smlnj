(* Copyright 1996 by Bell Laboratories *)
(* uncurry.sml *)

functor Uncurry(MachSpec : MACH_SPEC) : ETASPLIT  =
struct

local open CPS 
      structure LT = LtyExtern
      structure LV = LambdaVar
in

fun bug s = ErrorMsg.impossible ("Uncurry: " ^ s)

fun freein v = 
  let fun try(VAR w) = v=w
	| try(LABEL w) = v=w
	| try _ = false

      fun any(w :: rest) = try w orelse any rest
	| any nil = false
      
      fun any1((w,_)::rest) = try w orelse any1 rest
	| any1 nil = false

      val rec g =
	 fn APP(f,args) => try f orelse any args
	  | SWITCH(v,c,l) => try v orelse List.exists g l
	  | RECORD(_,l,w,ce) => any1 l orelse g ce
	  | SELECT(_,v,w,_,ce) => try v orelse g ce
	  | OFFSET(_,v,w,ce) => try v orelse g ce
	  | SETTER(_,vl,e) => any vl orelse g e
	  | LOOKER(_,vl,w,_,e) => any vl orelse g e
	  | ARITH(_,vl,w,_,e) => any vl orelse g e
	  | PURE(_,vl,w,_,e) => any vl orelse g e
	  | BRANCH(_,vl,c,e1,e2) => any vl orelse g e1 orelse g e2
	  | FIX(fl, e) => List.exists (g o #5) fl  orelse  g e
   in g
  end

fun etasplit {function=(fkind,fvar,fargs,ctyl,cexp),
	      table=typtable, click} = 
let

val debug = !Control.CG.debugcps (* false *)
fun debugprint s = if debug then Control.Print.say s else ()
fun debugflush() = if debug then Control.Print.flush() else ()
val rep_flag = MachSpec.representations
val type_flag = (!Control.CG.checkcps1) andalso
                (!Control.CG.checkcps1) andalso rep_flag

val defaultArrow = LT.ltc_arw(LT.ltc_void,LT.ltc_void)

fun extendLty(t,[]) = t
  | extendLty(t,a) = defaultArrow
     (*
      (let val (t1, t2) = LT.lt_arrow t
        in (case LT.lt_out t1
	     of LT.LT_TYC tc =>
                 (case LT.tc_out tc
                   of LT.TC_TUPLE l => 
                       let val l' = map LT.ltc_tyc l
                        in LT.ltc_arw(LT.ltc_tup(l'@a),t2)
                       end
                    | _ => LT.ltc_arw(LT.ltc_tup(t1::a),t2))
             | _ => LT.ltc_arw(LT.ltc_tup(t1::a),t2))
       end) *)
(* handle _ => 
	     (if type_flag 
	      then bug "extendLty on non user fun"
	      else defaultArrow) *)


(* count the number of GP and FP registers needed for a list of lvars *)
val unboxedfloat = MachSpec.unboxedFloats

fun isFltCty FLTt = unboxedfloat 
  | isFltCty _ = false

val numCSgpregs = MachSpec.numCalleeSaves
val numCSfpregs = MachSpec.numFloatCalleeSaves
val maxgpregs = MachSpec.numRegs - numCSgpregs - 1
val maxfpregs = MachSpec.numFloatRegs - numCSfpregs - 2  

fun checklimit(cl) = 
  let fun h(FLTt::r, m, n) = if unboxedfloat then h(r,m,n+1) else h(r,m+1,n)
        | h(_::r, m, n) = h(r,m+1,n)
        | h([], m, n) = (m <= maxgpregs) andalso (n <= maxfpregs)
   in h(cl, 0, 0)
  end

exception NEWETA
fun getty v = 
  if type_flag 
  then (Intmap.map typtable v) handle _ =>
                (Control.Print.say ("NEWETA: Can't find the variable "^
                            (Int.toString v)^" in the typtable ***** \n");
                 raise NEWETA)
  else LT.ltc_void

fun addty(f,t) = if type_flag then Intmap.add typtable (f,t) else ()
fun mkv(t) = let val v = LV.mkLvar()
              in (addty(v,t); v)
             end
fun copyLvar v = let val x = LV.dupLvar(v)
                  in (addty(x,getty v); x)
                 end

(* fun userfun(f) = case LT.out(getty(f)) of LT.ARROW _ => true
                                | _ => false
 *)

val rec reduce = 
   fn RECORD(k,vl,w,e) => RECORD(k, vl, w, reduce e)
    | SELECT(i,v,w,t,e) => SELECT(i, v, w, t, reduce e)
    | OFFSET(i,v,w,e) => OFFSET(i, v, w, reduce e)
    | APP(f,vl) => APP(f, vl)
    | SWITCH(v,c,el) => SWITCH(v, c,map reduce el)
    | BRANCH(i,vl,c,e1,e2) => BRANCH(i, vl, c, reduce e1, reduce e2)
    | LOOKER(i,vl,w,t,e) => LOOKER(i, vl, w, t, reduce e)
    | ARITH(i,vl,w,t,e) => ARITH(i, vl, w, t, reduce e)
    | PURE(i,vl,w,t,e) => PURE(i, vl, w, t, reduce e)
    | SETTER(i,vl,e) => SETTER(i, vl, reduce e)
    | FIX(l,e) =>
       let fun uncurry(fd as (CONT,_,_,_,_)) = [reduce_body(fd)]
	     | uncurry(fd as 
		       (fk,f,k::vl,ct::cl,body as FIX([(gk,g,ul,cl',body2)],
						      APP(VAR c,[VAR g'])))) =
                if k=c andalso g=g' (* andalso userfun(g) *)
                   andalso  not (freein k body2)
		   andalso not (freein g body2)   (* g not recursive *)
   		   andalso checklimit(cl@cl')
   		   then let val ul' = map copyLvar ul
			    and vl' = map copyLvar vl
			    val k'= copyLvar k
			    and g'= copyLvar g
			    val newlt = extendLty(getty(g),(map getty vl))
			    val f' = mkv(newlt)
			in click "u";
			    (NO_INLINE_INTO,f,k'::vl',ct::cl,
			     FIX([(gk,g',ul',cl',APP(VAR f',
						     map VAR (ul' @ vl')))], 
				 APP(VAR(k'),[VAR g'])))
			    ::uncurry(fk,f',ul@vl,cl'@cl,body2)
			end
                     else [reduce_body(fd)]
   	     | uncurry fd = [reduce_body(fd)]

	   and reduce_body (fk,f,vl,cl,e) = (fk,f,vl,cl,reduce e)

        in FIX(foldr (fn (fd,r) => (uncurry fd) @ r) [] l,
	       reduce e)
       end

 in debugprint "Uncurry: ";
    debugflush();
    (fkind, fvar, fargs, ctyl, reduce cexp) before debugprint "\n"
end

end (* toplevel local *)
end (* functor Uncurry *)


(*
 * $Log: uncurry.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:32  george
 *   Version 109.24
 *
 *)
