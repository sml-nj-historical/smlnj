(* switch.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO: use binary search!!!
 * TODO: this code needs a complete rewrite!!!
 *	- better code for words, boxed ints, etc.
 *	- merge E_strneq and E_boxed with E_branch
 *	- switch to directly using CPS types?
 * TODO: we probably don't need both E_ineq and E_wneq, but they are different in CPS.
 *)

signature SWITCH =
  sig

    val switch: {
	(* lower-bound on size of E_switch *)
	  E_switchlimit : int,
	(* make a tagged integer literal *)
	  E_tagint : IntInf.int -> 'value,
	(* make a boxed integer literal of the specified size *)
	  E_boxint : int IntConst.t -> 'value,
	(* test integers of the given size for equality *)
	  E_ineq : int -> 'comparison,
	(* test unsigned of the given size integers for equality *)
	  E_wneq : int -> 'comparison,
	(* test pointers for equality *)
	  E_pneq : 'comparison,
	(* tagged integer comparison *)
	  E_less : 'comparison,
	(* conditional branch *)
	  E_branch : 'comparison * 'value * 'value * 'cexp * 'cexp -> 'cexp,
	(* test a string against a literal value *)
	  E_strneq : 'value * string * 'cexp * 'cexp -> 'cexp,
	(* multiway switch on tagged integer values (zero-based) *)
	  E_switch : 'value * 'cexp list -> 'cexp,
	(* tagged integer addition *)
	  E_add : 'value * 'value * ('value -> 'cexp) -> 'cexp,
	(* get tag value from datatype value *)
	  E_gettag : 'value * ('value -> 'cexp) -> 'cexp,
	(* get unique ID (i.e., reference) from exception value *)
	  E_getexn : 'value * ('value -> 'cexp) -> 'cexp,
	(* get the length of a string *)
	  E_length : 'value * ('value -> 'cexp) -> 'cexp,
	(* unwrap unary constructor *)
	  E_unwrap : 'value * ('value -> 'cexp) -> 'cexp,
	(* test if a value is boxed or not *)
	  E_boxed : 'value * 'cexp * 'cexp -> 'cexp,
	(* variable renaming *)
	  E_path : Access.access * ('value -> 'cexp) -> 'cexp
	} -> {
	  exp     : 'value,
	  sign    : Access.consig,
	  cases   : (FLINT.con * 'cexp) list,
	  default : 'cexp
	} -> 'cexp

  end


structure Switch : SWITCH =
  struct

    structure L = FLINT
    structure A = Access

    fun bug s = ErrorMsg.impossible ("Switch: " ^ s)

    val toII = IntInf.fromInt
    val toI = IntInf.toInt

    fun switch {
	  E_switchlimit : int,
	  E_tagint: IntInf.int -> 'value,
	  E_boxint: int IntConst.t -> 'value,
	  E_ineq: int -> 'comparison,
	  E_wneq: int -> 'comparison,
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
	} = let
	  val neq = E_ineq Target.defaultIntSz
	  fun int' n = E_tagint(toII n)
	  fun switch1 (e : 'value, cases : (IntInf.int * 'cexp) list, default : 'cexp, (lo, hi)) =
		let
		val delta = 2
		fun collapse (l as (li,ui,ni,xi)::(lj,uj,nj,xj)::r ) =
		      if (toII((ni+nj) * delta) > ui-lj)
			then collapse((lj,ui,ni+nj,xj)::r)
			else l
		  | collapse l = l
		fun f (z, x as (i,_)::r) = f(collapse((i,i,1,x)::z), r)
		  | f (z, nil) = z
		fun tackon (stuff as (l,u,n,x)::r) =
		      if toII(n*delta) > u-l andalso n>E_switchlimit andalso hi>u
			then tackon((l,u+1,n+1,x@[(u+1,default)])::r)
			else stuff
		  | tackon nil = bug "switch.3217"
		fun separate((z as (l,u,n,x))::r) =
		      if n<E_switchlimit andalso n>1
			then let
			  val ix as (i,_) = List.nth(x, n-1)
			  in
			    (i,i,1,[ix])::separate((l,l,n-1,x)::r)
			  end
			else z :: separate r
		  | separate nil = nil
		val chunks = rev (separate (tackon (f (nil, cases))))
		fun g (1, (l, h, 1, (i, b)::_)::_, (lo, hi)) =
		      if lo=i andalso hi=i then b
		      else E_branch(neq, e, E_tagint i, default, b)
		  | g (1, (l, h, n, x)::_, (lo, hi)) =
		      let fun f (0,_,_) = nil
			    | f (n,i,l as (j,b)::r) =
				 if i+lo = j then b::f(n-1,i+1,r)
				 else (default::f(n,i+1,l))
			    | f _ = bug "switch.987"
			  val list = f(n,0,x)
			  val body = if lo=0 then E_switch(e, list)
				     else E_add(e, E_tagint(~lo), fn v => E_switch(v, list))
			  val a = if (lo<l) then E_branch(E_less, e, E_tagint l, default, body)
				  else body
			  val b = if (hi > h) then E_branch(E_less, E_tagint h, e, default, a)
				  else a
		       in b
		      end
		  | g (n,cases,(lo,hi)) = let
			val n2 = n div 2
			val c2 = List.drop(cases, n2)
			val (l, r) = (case c2
			       of (l1,_,_,_)::r1 => (l1,r1)
				| _ => bug "switch.111"
			      (* end case *))
			in
			  E_branch(E_less, e, E_tagint l,
			    g(n2, cases, (lo, l-1)),
			    g(n-n2, c2, (l, hi)))
			end
		in
		  g (List.length chunks, chunks, (lo, hi))
		end

	(* switch for default tagged integer type *)
	  fun dflt_int_switch (e: 'value, l : (IntInf.int * 'cexp) list, default, inrange) = let
		val len = List.length l
		fun ifelse nil = default
		  | ifelse ((i, b)::r) = E_branch(neq, E_tagint i, e, ifelse r, b)
		fun ifelseN [(i, b)] = b
		  | ifelseN ((i, b)::r) = E_branch(neq, E_tagint i, e, ifelseN r, b)
		  | ifelseN _ = bug "switch.224"
		val l = ListMergeSort.sort (fn ((i,_), (j,_)) => IntInf.>(i, j)) l
		in
		  case (len < E_switchlimit, inrange)
		   of (true, NONE) => ifelse l
		    | (true, SOME n) => if n+1=len then ifelseN l else ifelse l
		    | (false, NONE) => let
			val (hi, _) = (List.last l handle List.Empty => bug "switch.last132")
			val (low, r) = (case l
			       of (low', _)::r' => (low', r')
				| _ => bug "switch.23"
			      (* end case *))
			in
			  E_branch(E_less, e, E_tagint low, default,
			    E_branch(E_less, E_tagint hi, e, default,
			      switch1(e, l, default, (low, hi))))
			 end
		    | (false, SOME n) => switch1(e, l, default, (0, IntInf.fromInt n))
		  (* end case *)
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

	  fun datacon_switch(w,sign,l: (L.con * 'cexp) list, default) = let
		fun tag (L.DATAcon((_,A.CONSTANT i,_),_,_)) = toII i
		  | tag (L.DATAcon((_,A.TAGGED i,_),_,_)) = toII i
	(*        | tag (L.DATAcon((_,A.TAGGEDREC(i,_),_),_,_)) = i *)
		  | tag _ = 0
		fun tag'(c, e) = (tag c, e)
		val (boxed, unboxed) = List.partition (isboxed o #1) l
		val b = map tag' boxed and u = map tag' unboxed
		in
		  case sign
		   of A.CSIG (0, n) =>
			E_unwrap(w, fn w' => dflt_int_switch(w',u,default,SOME(n-1)))
		    | A.CSIG (n, 0) =>
			E_gettag(w, fn w' => dflt_int_switch(w',b,default,SOME(n-1)))
		    | A.CSIG (1, nu) =>
			E_boxed(w, dflt_int_switch(E_tagint 0, b, default, SOME 0),
			  E_unwrap(w, fn w' => dflt_int_switch(w',u,default,SOME(nu-1))))
		    | A.CSIG (nb,nu) =>
			E_boxed(w,
			  E_gettag(w, fn w' => dflt_int_switch(w',b,default,SOME(nb-1))),
			    E_unwrap(w, fn w' => dflt_int_switch(w',u,default,SOME(nu-1))))
		    | A.CNIL => bug "datacon_switch"
		  (* end case *)
		end

	  fun coalesce (l : (string * 'a) list) : (IntInf.int * (string * 'a) list) list = let
		val l' = ListMergeSort.sort (fn ((s1,_),(s2,_)) => size s1 > size s2) l
		val s = #1 (List.hd l')
		fun gather (n, [], current, acc) = (n,current)::acc
		  | gather (n, (x as (s,a))::rest, current, acc) = let
		    val s1 = toII(size s)
		    in
		      if s1 = n then gather(n,rest,x::current,acc)
		      else gather(s1,rest,[x],(n,current)::acc)
		    end
		in
		  gather (toII(size s), l', [], [])
		end

	  fun string_switch(w,l,default) = let
		fun strip (L.STRINGcon s, x) = (s,x)
		  | strip _ = bug "string_switch"
		val b = map strip l
		val bylength = coalesce b
		fun one_len (0, (_,e)::_) = (0, e)
		  | one_len (len, l) = let
		      fun try nil = default
			| try ((s,e)::r) = E_strneq(w, s, try r, e)
		      in
			(len,try l)
		      end
		in
		  E_length(w ,fn len =>
		    dflt_int_switch(len, map one_len bylength, default, NONE))
		end

	(* switch for non-default integer types *)
	  fun int_switch(w, (L.INTcon i, e)::r, default) =
		E_branch(E_ineq(#ty i), w, E_boxint i, int_switch(w, r, default), e)
	    | int_switch(_, [], default) = default
	    | int_switch _ = bug "switch.88"

	(* switch for non-default word types *)
	  fun word_switch(w, (L.WORDcon wval, e)::r, default) =
		E_branch(E_wneq(#ty wval), w, E_boxint wval, word_switch(w,r,default), e)
	    | word_switch(_, nil, default) = default
	    | word_switch _ = bug "switch.89"

	  in fn {cases=[], default, ...} => default
	      | {exp, sign, cases as (c,_)::_, default} => (case c
		   of L.INTcon{ival, ty} => let
			fun un_int (L.INTcon{ival, ...}, e) = (ival, e)
			  | un_int _ = bug "un_int"
			in
			  if (ty = Target.defaultIntSz)
			    then dflt_int_switch(exp, map un_int cases, default, NONE)
			    else int_switch(exp, cases, default)
			end
		    | L.WORDcon{ival, ty} => let
			fun un_word (L.WORDcon{ival, ...}, e) = (ival, e)
			  | un_word _ = bug "un_int"
			in
			  if (ty = Target.defaultIntSz)
			    then dflt_int_switch(exp, map un_word cases, default, NONE)
			    else word_switch(exp, cases, default)
			end
		    | L.STRINGcon _ => string_switch(exp,cases,default)
		    | L.DATAcon((_,A.EXN _,_),_,_) => exn_switch(exp,cases,default)
		    | L.DATAcon _ => datacon_switch(exp,sign,cases,default)
		    | _ => bug "unexpected datacon in genswitch"
		 (* end case *))
	  end (* switch *)

  end (* structure Switch *)

