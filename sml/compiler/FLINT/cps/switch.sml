(* switch.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO: use binary search!!!
 *)

signature SWITCH =
sig

 exception TooBig

 val switch:
     {E_int: int -> 'value,   (* may raise TooBig; not all ints need
			    be representable *)
      E_switchlimit : int,
      E_neq: 'comparison,
      E_w32neq: 'comparison,
      E_i32neq: 'comparison,
      E_word32: Word32.word -> 'value,
      E_int32: Word32.word -> 'value,
      E_wneq: 'comparison,
      E_word: word -> 'value,
      E_pneq: 'comparison,
      E_less: 'comparison,
      E_branch: 'comparison * 'value * 'value * 'cexp * 'cexp -> 'cexp,
      E_strneq: 'value * string * 'cexp * 'cexp -> 'cexp,
      E_switch: 'value * 'cexp list -> 'cexp,
      E_add : 'value * 'value * ('value->'cexp) -> 'cexp,
      E_gettag: 'value * ('value -> 'cexp) -> 'cexp,
      E_getexn: 'value * ('value -> 'cexp) -> 'cexp,
      E_length: 'value * ('value -> 'cexp) -> 'cexp,
      E_unwrap: 'value * ('value -> 'cexp) -> 'cexp,
      E_boxed:  'value * 'cexp * 'cexp -> 'cexp,
      E_path:  Access.access * ('value->'cexp) -> 'cexp
     } ->
     {exp: 'value,
      sign: Access.consig,
      cases: (FLINT.con * 'cexp) list,
      default: 'cexp}
       ->
       'cexp

end


structure Switch : SWITCH =
struct

structure L = FLINT
structure A = Access

fun bug s = ErrorMsg.impossible ("Switch: " ^ s)

exception TooBig

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl x = x
  in  subl
  end

fun nthcdr(l, 0) = l
  | nthcdr(a::r, n) = nthcdr(r, n-1)
  | nthcdr _ = bug "nthcdr in switch"

fun count test =
  let fun subl acc (a::r) = subl(if test a then 1+acc else acc) r
        | subl acc nil = acc
   in subl 0
  end

fun switch
     {E_int: int -> 'value,   (* may raise TooBig; not all ints need
			    be representable *)
      E_switchlimit : int,
      E_neq: 'comparison,
      E_w32neq: 'comparison,
      E_i32neq: 'comparison,
      E_word32: Word32.word -> 'value,
      E_int32: Word32.word -> 'value,
      E_wneq: 'comparison,
      E_word: word -> 'value,
      E_pneq: 'comparison,
      E_less: 'comparison,
      E_branch: 'comparison * 'value * 'value * 'cexp * 'cexp -> 'cexp,
      E_strneq: 'value * string * 'cexp * 'cexp -> 'cexp,
      E_switch: 'value * 'cexp list -> 'cexp,
      E_add : 'value * 'value * ('value->'cexp) -> 'cexp,
      E_gettag: 'value * ('value -> 'cexp) -> 'cexp,
      E_getexn: 'value * ('value -> 'cexp) -> 'cexp,
      E_length: 'value * ('value -> 'cexp) -> 'cexp,
      E_unwrap: 'value * ('value -> 'cexp) -> 'cexp,
      E_boxed:  'value * 'cexp * 'cexp -> 'cexp,
      E_path:  Access.access * ('value->'cexp) -> 'cexp
     } =
let

fun switch1(e : 'value, cases : (int*'cexp) list, default : 'cexp, (lo,hi)) =
  let val delta = 2
      fun collapse (l as (li,ui,ni,xi)::(lj,uj,nj,xj)::r ) =
	     if ((ni+nj) * delta > ui-lj) then collapse((lj,ui,ni+nj,xj)::r)
	     else l
	| collapse l = l
      fun f (z, x as (i,_)::r) = f(collapse((i,i,1,x)::z), r)
	| f (z, nil) = z
      fun tackon (stuff as (l,u,n,x)::r) =
	    if n*delta > u-l andalso n>E_switchlimit andalso hi>u
	    then tackon((l,u+1,n+1,x@[(u+1,default)])::r)
	    else stuff
        | tackon nil = bug "switch.3217"
      fun separate((z as (l,u,n,x))::r) =
	    if n<E_switchlimit andalso n>1
	    then let val ix as (i,_) = List.nth(x, (n-1))
		  in (i,i,1,[ix])::separate((l,l,n-1,x)::r)
		 end
	    else z :: separate r
	| separate nil = nil
      val chunks = rev (separate (tackon (f (nil,cases))))
      fun g(1,(l,h,1,(i,b)::_)::_,(lo,hi)) =
      	    if lo=i andalso hi=i then b
	    else E_branch(E_neq,e,E_int i,default,b)
	| g(1,(l,h,n,x)::_,(lo,hi)) =
	    let fun f(0,_,_) = nil
		  | f(n,i,l as (j,b)::r) =
		       if i+lo = j then b::f(n-1,i+1,r)
		       else (default::f(n,i+1,l))
                  | f _ = bug "switch.987"
		val list = f(n,0,x)
		val body = if lo=0 then E_switch(e, list)
			   else E_add(e, E_int(~lo),fn v =>E_switch(v,list))
		val a = if (lo<l) then E_branch(E_less,e,E_int l,default,body)
			else body
	        val b = if (hi > h) then E_branch(E_less,E_int h,e,default,a)
			else a
	     in b
	    end
	| g(n,cases,(lo,hi)) =
	    let val n2 = n div 2
		val c2 = nthcdr(cases, n2)
                val (l,r) = case c2 of (l1,_,_,_)::r1 => (l1,r1)
                                     | _ => bug "switch.111"
	     in E_branch(E_less,e,E_int l, g(n2,cases,(lo,l-1)), g(n-n2,c2,(l,hi)))
	    end
   in g (List.length chunks, chunks, (lo, hi))
  end

val sortcases = ListMergeSort.sort (fn ((i:int,_),(j,_)) => i>j)

fun int_switch(e: 'value, l, default, inrange) =
  let val len = List.length l

      fun isbig i = (E_int i; false) handle TooBig => true
      val anybig = List.exists (isbig o #1) l
      fun construct(i, c) =
	  if isbig i
	      then let val j = i div 2
		    in construct(j,fn j' =>
			construct(i-j, fn k' =>
			  E_add(j', k', c)))
		   end
	      else c(E_int i)

      fun ifelse nil = default
        | ifelse ((i,b)::r) =
	    construct(i, fn i' => E_branch(E_neq, i', e, ifelse r, b))

      fun ifelseN [(i,b)] = b
	| ifelseN ((i,b)::r) = E_branch(E_neq,E_int i, e, ifelseN r, b)
	| ifelseN _ = bug "switch.224"
      val l = sortcases l
   in case (anybig orelse len<E_switchlimit, inrange)
       of (true, NONE) => ifelse l
	| (true, SOME n) =>  if n+1=len then ifelseN l else ifelse l
	| (false, NONE) =>
	     let val (hi,_) = List.last l
                              handle List.Empty => bug "switch.last132"
                 val (low,r) = case l of (low',_)::r' => (low',r')
                                       | _ => bug "switch.23"
	      in E_branch(E_less,e,E_int low, default,
	       	   E_branch(E_less,E_int hi, e, default,
			       switch1(e, l, default, (low,hi))))
	     end
	| (false, SOME n) => switch1(e, l, default, (0,n))
  end

  fun isboxed (L.DATAcon((_,A.CONSTANT _, _),_,_)) = false
    | isboxed (L.DATAcon((_,A.LISTNIL,_),_,_)) = false
    | isboxed (L.DATAcon((_,rep,_),_,_)) = true
    | isboxed (L.STRINGcon s) = true
    | isboxed _ = false

  fun isexn (L.DATAcon((_,A.EXN _,_),_,_)) = true
    | isexn _ = false

 fun exn_switch(w,l,default) =
   E_getexn(w, fn u =>
      let fun g((L.DATAcon((_,A.EXN p,_),_,_),x)::r) =
                E_path(p, fn v => E_branch(E_pneq,u,v, g r, x))
	    | g nil = default
	    | g _ = bug "switch.21"
       in g l
      end)

 fun datacon_switch(w,sign,l: (L.con * 'cexp) list, default) =
   let
      fun tag (L.DATAcon((_,A.CONSTANT i,_),_,_)) = i
        | tag (L.DATAcon((_,A.TAGGED i,_),_,_)) = i
(*      | tag (L.DATAcon((_,A.TAGGEDREC(i,_),_),_,_)) = i *)
	| tag _ = 0

      fun tag'(c,e) = (tag c, e)

      val boxed = sublist (isboxed o #1) l
      val unboxed = sublist (not o isboxed o #1) l
      val b = map tag' boxed and u = map tag' unboxed

    in  case sign
	 of A.CSIG (0, n) =>
              E_unwrap(w,fn w' => int_switch(w',u,default,SOME(n-1)))
	  | A.CSIG (n, 0) =>
              E_gettag(w,fn w' => int_switch(w',b,default,SOME(n-1)))
	  | A.CSIG (1, nu) =>
	      E_boxed(w, int_switch(E_int 0, b, default,SOME 0),
	       E_unwrap(w, fn w' => int_switch(w',u,default,SOME(nu-1))))
	  | A.CSIG (nb,nu) =>
              E_boxed(w,
               E_gettag(w, fn w' => int_switch(w',b,default,SOME(nb-1))),
	        E_unwrap(w, fn w' => int_switch(w',u,default,SOME(nu-1))))
          | A.CNIL => bug "datacon_switch"
   end

 fun coalesce(l:(string * 'a)list) : (int * (string * 'a) list) list = let
     val l' = ListMergeSort.sort (fn ((s1,_),(s2,_)) => size s1 > size s2) l
     val s = #1 (List.hd l')
     fun gather(n,[],current,acc) = (n,current)::acc
       | gather(n,(x as (s,a))::rest,current,acc) = let val s1 = size s
	 in
	   if s1 = n then gather(n,rest,x::current,acc)
	   else gather(s1,rest,[x],(n,current)::acc)
         end
  in
    gather(size s,l',[],[])
  end

 fun string_switch(w,l,default) =
  let fun strip (L.STRINGcon s, x) = (s,x)
	| strip _ = bug "string_switch"
      val b = map strip l

      val bylength = coalesce b

      fun one_len(0,(_,e)::_) = (0,e)
	| one_len(len,l) =
	  let fun try nil = default
	        | try ((s,e)::r) = E_strneq(w,s, try r, e)
	   in (len,try l)
	  end

      val genbs =
             E_length(w,fn len =>
		      int_switch(len, map one_len bylength, default, NONE))

  in genbs
 end

 fun word_switch(w, (L.WORDcon wval,e)::r, default) =
       E_branch(E_wneq, w, E_word wval, word_switch(w,r,default), e)
   | word_switch(_, nil, default) = default
   | word_switch _ = bug "switch.88"

 fun word32_switch(w,(L.WORD32con i32val,e)::r,default) =
       E_branch(E_w32neq, w, E_word32 i32val, word32_switch(w,r,default), e)
   | word32_switch(_, nil, default) = default
   | word32_switch _ = bug "switch.78"

 fun int32_switch(w, (L.INT32con i32val, e)::r, default) = let
       val int32ToWord32 = Word32.fromLargeInt o Int32.toLarge
     in
       E_branch(E_i32neq, w, E_int32 (int32ToWord32 i32val),
		int32_switch(w, r, default), e)
     end
   | int32_switch(_, nil, default) = default
   | int32_switch _ = bug "switch.77"

 in fn {cases=nil,default,...} => default
     | {exp,sign,cases as (c,_)::_,default} =>
      case c
       of L.INTcon _ =>
	     let fun un_int(L.INTcon i, e) = (i,e)
		   | un_int _ = bug "un_int"
	      in int_switch(exp,map un_int cases,default,NONE)
	     end
        | L.STRINGcon _ => string_switch(exp,cases,default)
        | L.DATAcon((_,A.EXN _,_),_,_) => exn_switch(exp,cases,default)
        | L.DATAcon _ => datacon_switch(exp,sign,cases,default)
	| L.WORDcon _ => word_switch(exp, cases, default)
	| L.WORD32con _ => word32_switch(exp,cases,default)
	| L.INT32con _ => int32_switch(exp,cases,default)
        | _ => bug "unexpected datacon in genswitch"

end

end (* structure Switch *)

