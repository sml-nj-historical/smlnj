(* Copyright 1996 by Bell Laboratories *)
(* expand.sml *)

functor ExpandNEW (MachSpec : MACH_SPEC) : EXPAND =
struct

local open CPS
      structure CG = Control.CG
      structure LV = LambdaVar
in

 fun inc r = (r := !r + 1)
 fun dec r = (r := !r - 1)

 fun map1 f (a,b) = (f a, b)

 fun sum f = let fun h [] = 0 
		   | h (a::r) = f a + h r
	     in h
	     end

 fun split pred (a::rest) = let val (t,f) = split pred rest
                             in if pred a then (a::t,f) else (t,a::f)
                            end
   | split pred nil = (nil,nil)

 fun muldiv(a,b,c) =   (* a*b/c, approximately, but guaranteed no overflow *)
     (a*b) div c
     handle Overflow => if a>b then muldiv(a div 2,b,c div 2)
			       else muldiv(a,b div 2,c div 2)

 fun sameName(x,VAR y) = LV.sameName(x,y) 
   | sameName(x,LABEL y) = LV.sameName(x,y) 
   | sameName _ = ()

 datatype mode = ALL | NO_UNROLL | UNROLL of int | HEADERS

fun expand{function=(fkind,fvar,fargs,ctyl,cexp),unroll,bodysize,click,
           afterClosure,table=typtable,do_headers} =
  let
   val clicked_any = ref false
   val debug = !CG.debugcps (* false *)
   val debugprint = if debug then Control.Print.say else fn _ => ()
   val debugflush = if debug then Control.Print.flush else fn _ => ()
   val click = fn z => (debugprint z;  (* temporary *)
			click z; clicked_any := true)
   val CGinvariant = !CG.invariant
   fun label v = if afterClosure then LABEL v else VAR v
   datatype info = 
       Fun of {escape: int ref, (* how many non-call uses *)
	       call: int ref,   (* how many calls to this func *)
	       size: int ref,   (* size of function body *)
	       args: lvar list, (* formal parameters *)
	       body: cexp,      (* function body *)
	       invariant: bool list ref, (* one for each arg *)
	       sibling_call: int ref, (* how many of calls are from
                    other functions defined in same FIX *)
	       unroll_call: int ref,  (* how many calls are from within
				       this func's body *)
	       level: int,      (* loop-nesting level of this function *)
	       within: bool ref,(* are we currently doing pass1 within this 
				    function's body? *)
	       within_sibling: bool ref
	                        (* are we currently doing passw within the
				  body of this function or any of the other
				  functions defined in the same FIX? *)
	      }
		 | Arg of {escape: int ref, savings: int ref,
			   record: (int * lvar) list ref}
		 | Sel of {savings: int ref}
		 | Rec of {escape: int ref, size: int,
			   vars: (value * accesspath) list}
		 | Real
		 | Const
		 | Other

   val rep_flag = MachSpec.representations
   val type_flag = (!Control.CG.checkcps1) andalso
                   (!Control.CG.checkcps1) andalso rep_flag

   local   
     exception NEXPAND
     fun getty v = 
       if type_flag
       then (Intmap.map typtable v) handle _ =>
                  (Control.Print.say ("NEXPAND: Can't find the variable "^
                            (Int.toString v)^" in the typtable ***** \n");
                   raise NEXPAND)
       else LtyExtern.ltc_void
     fun addty(f,t) = Intmap.add typtable (f,t)
   in

   fun mkv(t) = let val v = LV.mkLvar()
                    val _ = if type_flag then addty(v,t) else ()
                in  v
                end
 
   fun copyLvar v = let val x = LV.dupLvar(v)
                        val _ = if type_flag then addty(x,getty v) else ()
                    in  x
                    end

   end (* local *)


 local exception Expand
       val m : info Intmap.intmap = Intmap.new(128,Expand)
       val get' = Intmap.map m
 in    val note = Intmap.add m
       fun get i = get' i handle Expand => Other 
       fun discard_pass1_info() = Intmap.clear m
 end
   fun getval(VAR v) = get v
     | getval(LABEL v) = get v
     | getval(INT _) = Const
(*     | getval(REAL _) = Real*)
     | getval _ = Other
   fun call(v, args) = case getval v
			of Fun{call,within=ref false,
			       within_sibling=ref false,...} => inc call
			 | Fun{call,within=ref false,within_sibling=ref true,
			       sibling_call,...} => (inc call; 
						     inc sibling_call)
			 | Fun{call,within=ref true,unroll_call,
			       args=vl,invariant,...} => 
			     let fun g(VAR x :: args, x' :: vl, i::inv) =
				       (i andalso x=x') :: g(args,vl,inv)
				   | g( _ :: args, _ :: vl, i::inv) =
				       false :: g(args,vl,inv)
				   | g _ = nil
			     in  inc call; inc unroll_call;
				 invariant := g(args,vl,!invariant)
			     end
			 | Arg{savings,...} => inc savings
			 | Sel{savings} => inc savings
			 | _ => ()
   fun escape v = case getval v
		   of Fun{escape,...} => inc escape
		    | Arg{escape,...} => inc escape
		    | Rec{escape,...} => inc escape
		    | _ => ()
   fun escapeargs v = case getval v
		       of Fun{escape,...} => inc escape
			| Arg{escape,savings, ...} =>
				   (inc escape; inc savings)
			| Sel{savings} => inc savings
			| Rec{escape,...} => inc escape
			| _ => ()
   fun unescapeargs v = case getval v
			 of Fun{escape,...} => dec escape
			  | Arg{escape,savings, ...} =>
				      (dec escape; dec savings)
			  | Sel{savings} => dec savings
			  | Rec{escape,...} => dec escape
			  | _ => ()
   fun notearg v = (note (v,Arg{escape=ref 0,savings=ref 0, record=ref []}))
   fun noteother v = ()  (* note (v,Other) *)
   fun notereal v = noteother v  (* note (v,Real) *)
   fun enter level (_,f,vl,_,e) = 
              (note(f,Fun{escape=ref 0, call=ref 0, size=ref 0,
			  args=vl, body=e, within=ref false,
			  within_sibling=ref false,
			  unroll_call = ref 0, sibling_call=ref 0,
			  invariant = ref(map (fn _ => CGinvariant) vl),
			  level=level});
	       app notearg vl)
   fun noterec(w, vl, size) = note (w,Rec{size=size,escape=ref 0,vars=vl})
   fun notesel(i,v,w) = (note (w, Sel{savings=ref 0});
			 case getval v
			  of Arg{savings,record,...} => (inc savings;
						  record := (i,w)::(!record))
			   | _ => ())

   fun setsize(f,n) = case get f of Fun{size,...} => (size := n; n)

   fun incsave(v,k) = case getval v
		    of Arg{savings,...} => savings := !savings + k
		     | Sel{savings} => savings := !savings + k
		     | _ => ()
   fun setsave(v,k) = case getval v
			of Arg{savings,...} => savings := k
			 | Sel{savings} => savings := k
			 | _ => ()
   fun savesofar v = case getval v 
		      of Arg{savings,...} => !savings
		       | Sel{savings} => !savings
		       | _ => 0

   fun within_sibling fundefs func arg =
       (app (fn (_,f,_,_,_) =>
	     case get f of Fun{within_sibling=w,...} => w := true) fundefs;
	func arg before
        (app (fn (_,f,_,_,_) =>
	     case get f of Fun{within_sibling=w,...} => w := false) fundefs))

   fun within f func arg =
        case get f of Fun{within=w,...} => 
	    (w := true; func arg before (w := false))

   val rec prim = fn (level,vl,e) =>
       let fun vbl(VAR v) = (case get v of Rec _ => 0 | _ => 1)
	     | vbl _ = 0
	   val nonconst = sum vbl vl
	   val sl = map savesofar vl
	   val afterwards = pass1 level e
	   val zl = map savesofar vl
	   val overhead = length vl + 1
	   val potential = overhead
	   val savings = case nonconst of
			   1 => potential
			 | 2 => potential div 4
			 | _ => 0
	   fun app3 f = let fun loop (a::b,c::d,e::r) = (f(a,c,e); loop(b,d,r))
			      | loop _ = ()
			in  loop
			end
       in  app3(fn (v,s,z)=> setsave(v,s + savings + (z-s))) (vl,sl,zl);
	   overhead+afterwards
       end

   and primreal = fn (level,(_,vl,w,_,e)) =>
       (notereal w;
	app (fn v => incsave(v,1)) vl;
	2*(length vl + 1) + pass1 level e)

 (*******************************************************************)
 (* pass1: gather info on code.                                     *)
 (*******************************************************************) 
  and pass1 : int -> cexp -> int= fn level =>
    fn RECORD(_,vl,w,e) =>
        let val len = length vl
        in  app (escape o #1) vl;
	    noterec(w,vl,len);
	    2 + len + pass1 level e
	end
     | SELECT (i,v,w,_,e) => (notesel(i,v,w); 1 + pass1 level e)
     | OFFSET (i,v,w,e) => (noteother w; 1 + pass1 level e)
     | APP(f,vl) => (call(f,vl); 
		     app escapeargs vl; 
		     1 + ((length vl + 1) div 2))
     | FIX(l, e) => 
	  (app (enter level) l; 
	   within_sibling l
	     (fn () =>
	       (sum (fn (_,f,_,_,e) => setsize(f, within f (pass1 (level+1)) e)) l
                                             + length l + pass1 level e))
	       ())
     | SWITCH(v,_,el) => let val len = length el
			     val jumps = 4 + len
			     val branches = sum (pass1 level) el
			 in  incsave(v, muldiv(branches,len-1,len) + jumps);
			     jumps+branches
			 end
     | BRANCH(_,vl,c,e1,e2) =>
       let fun vbl(VAR v) = (case get v of Rec _ => 0 | _ => 1)
	     | vbl _ = 0
	   val nonconst = sum vbl vl
	   val sl = map savesofar vl
	   val branches = pass1 level e1 + pass1 level e2
	   val zl = map savesofar vl
	   val overhead = length vl
	   val potential = overhead + branches div 2
	   val savings = case nonconst of
			   1 => potential
			 | 2 => potential div 4
			 | _ => 0
	   fun app3 f = let fun loop (a::b,c::d,e::r) = (f(a,c,e); loop(b,d,r))
			      | loop _ = ()
			in  loop
			end
       in  app3(fn (v,s,z)=> setsave(v,s + savings + (z-s) div 2)) (vl,sl,zl);
	   overhead+branches
       end
     | LOOKER(_,vl,w,_,e) => (noteother w; prim(level,vl,e))
     | SETTER(_,vl,e) => prim(level,vl,e)
     | ARITH(args as (P.arith{kind=P.FLOAT 64,...},_,_,_,_)) =>
         primreal (level,args)
     | ARITH(args as (P.round _, _,_,_,_)) => primreal (level,args)
     | ARITH(_,vl,w,_,e) => (noteother w; prim(level,vl,e))
     | PURE(P.pure_arith{kind=P.FLOAT 64,...},[v],w,_,e) => 
	          (notereal w; incsave(v,1); 4+(pass1 level e))
     | PURE(P.real{tokind=P.FLOAT 64,...},vl,w,_,e) =>
		  (notereal w; prim(level,vl,e))
     | PURE(_,vl,w,_,e) => (noteother w; prim(level,vl,e))


   (*********************************************************************)
   (* substitute(args,wl,e,alpha) : substitute args for wl in e.        *)
   (* If alpha=true, also rename all bindings.                          *)
   (*********************************************************************)
   fun substitute(args,wl,e,alpha) =
    let 
	exception Alpha
        val vm : value Intmap.intmap = Intmap.new(16, Alpha)
        fun look (v,default) = Intmap.map vm v handle Alpha => default
        val enter = Intmap.add vm
	fun use(v0 as VAR v) = look(v,v0)
	  | use(v0 as LABEL v) = look(v,v0)
	  | use x = x
	fun def v = if alpha
	             then let val w = copyLvar v 
			  in  enter (v, VAR w); w
			  end
		     else v 
	fun defl v = if alpha
	             then let val w = copyLvar v 
			  in  enter (v, label w);
			       w
			  end
		     else v
	fun bind(a::args,w::wl) = 
	       (sameName(w,a); enter (w,a); bind(args,wl))
	  | bind _ = ()

	val rec g =
       fn RECORD(k,vl,w,ce) => RECORD(k,map (map1 use) vl, def w, g ce)
	| SELECT(i,v,w,t,ce) => SELECT(i, use v, def w, t, g ce)
	| OFFSET(i,v,w,ce) => OFFSET(i, use v, def w, g ce)
	| APP(v,vl) => APP(use v, map use vl)
	| FIX(l,ce) => 
	  let (* Careful: order of evaluation is important here. *)
	      fun h1(fk,f,vl,cl,e) = (fk,defl f,vl,cl,e)
	      fun h2(fk,f',vl,cl,e) =
		  let val vl' = map def vl
		      val e'= g e
		  in  (fk,f',vl',cl,e')
		  end
	  in  FIX(map h2(map h1 l), g ce)
	  end
	| SWITCH(v,c,l) => SWITCH(use v, def c, map g l)
	| LOOKER(i,vl,w,t,e) => LOOKER(i, map use vl, def w, t, g e)
	| ARITH(i,vl,w,t,e) => ARITH(i, map use vl, def w, t, g e)
	| PURE(i,vl,w,t,e) => PURE(i, map use vl, def w, t, g e)
	| SETTER(i,vl,e) => SETTER(i, map use vl, g e)
	| BRANCH(i,vl,c,e1,e2) => BRANCH(i, map use vl, def c, g e1, g e2)

    in  bind(args,wl); g e
    end

   fun whatsave(acc, size, (v:value)::vl, a::al) =
       if acc>=size
       then acc
       else
       (case get a of
	  Arg{escape=ref esc,savings=ref save,record=ref rl} =>
	  let val (this, nvl: value list, nal) =
	       case getval v
		of Fun{escape=ref 1,...} =>
			(if esc>0 then save else 6+save,vl,al)
		 | Fun _ => (save,vl,al)
		 | Rec{escape=ref ex,vars,size} =>
		      let exception Chase
			  fun chasepath(v,OFFp 0) = v
			    | chasepath(v, SELp(i,p)) =
			       (case getval v
				 of Rec{vars,...} =>
					chasepath(chasepath(List.nth(vars,i)),p)
				  | _ => raise Chase)
			    | chasepath _ = raise Chase
			  fun loop([],nvl,nal) = 
			      (if ex>1 orelse esc>0
			       then save
			       else save+size+2,nvl,nal)
			    | loop((i,w)::rl,nvl,nal) =
			       loop(rl,
				  chasepath(List.nth(vars,i))::nvl,
				    w::nal)
		      in  loop(rl,vl,al)
			  handle Chase => (0,vl,al)
			       | Subscript => (0,vl,al)
		      end 
		(* | Real => (save,vl,al)*)
		 | Const => (save,vl,al)
		 | _ => (0,vl,al)
	  in  whatsave(acc+this - muldiv(acc,this,size), size, nvl,nal)
	  end
	| Sel{savings=ref save} =>
	  let val this =
	      case v
	       of VAR v' => (case get v' of
			      Fun _ => save
			    | Rec _ => save
			    | _ => 0)
		| _ => save
	  in  whatsave(acc + this - muldiv(acc,this,size),size, vl,al)
	  end)
     | whatsave(acc,size,_,_) = acc

   
    (*************************************************************
     * should_expand: should a function application be inlined?  *
     *************************************************************)
    fun should_expand(d,  (* path length from entry to current function *)
		      u,  (* unroll level *)
		      e as APP(v,vl), 
		      Fun{escape,call,unroll_call,size=ref size,args,body,
			  level,within=ref within,...}) =
    if !call + !escape = 1 then false else
      let val stupidloop =  (* prevent infinite loops  at compile time *)
	    case (v,body) 
	     of (VAR vv, APP(VAR v',_)) => vv=v' 
	      | (LABEL vv, APP(LABEL v',_)) => vv=v' 
	      | _ => false
	val calls = case u of UNROLL _ => !unroll_call | _ => !call
	val small_fun_size = case u of UNROLL _ => 0 | _ => 50
	val savings = whatsave(0,size,vl,args)
	val predicted = 
	    let val real_increase = size-savings-(1+length vl)
	    in  real_increase * calls - 
		(* don't subtract off the original body if
		   the original body is huge (because we might
		   have guessed wrong and the consequences are
		   too nasty for big functions); or if we're
		   in unroll mode *)
		(if size < small_fun_size then size else 0)
	    end
	val depth = 2 and max = 2

    in  if false andalso debug
	    then (PPCps.prcps e;
		  debugprint(Int.toString predicted); debugprint "   "; 
		  debugprint(Int.toString bodysize); debugprint "\n")
	else ();

       not stupidloop
       andalso case u
	    of UNROLL lev => 
		 (* Unroll if: the loop body doesn't make function
		    calls orelse "unroll_recur" is turned on; andalso 
		    we are within the definition of the function; 
		    andalso it looks like things won't grow too much.
		  *)
		   (!CG.unroll_recur orelse level >= lev)
		   andalso within andalso predicted <= bodysize
	     | NO_UNROLL =>
		   !unroll_call = 0 andalso
		   not within andalso
		   (predicted <= bodysize  
		     orelse (!escape=0 andalso calls = 1))
	     | HEADERS => false  (* shouldn't get here *)
	     | ALL =>
		   (predicted <= bodysize  
		     orelse (!escape=0 andalso calls = 1))
  end

   datatype decision = YES of {formals: lvar list, body: cexp} 
                     | NO of int  (* how many no's in a row *)
   (* There is really no point in making decisions a ref.  This should
      be changed one day. *)
   val decisions : decision list ref = ref nil
   fun decide_yes x = decisions := YES x :: !decisions
   fun decide_no () = decisions :=
       (case !decisions
         of NO n :: rest => NO(n+1) :: rest
          | d => NO 1 :: d)


   (*********************************************************************)
   (* pass2: mark function applications to be inlined.                  *)
   (*********************************************************************)
   fun pass2(d, (* path length from start of current function *)
	     u, (* unroll-info *)
	     e (* expression to traverse *)
	     ) = case e

    of RECORD(k,vl,w,ce) => pass2(d+2+length vl,u,ce)
     | SELECT(i,v,w,t,ce) => pass2(d+1,u,ce)
     | OFFSET(i,v,w,ce) => pass2(d+1,u,ce)
     | APP(v,vl) => 
	 (case getval v
	   of info as Fun{args,body,...} =>
	       if should_expand(d,u,e,info)
		   then decide_yes{formals=args,body=body}
		    else decide_no()
	    | _ => decide_no())
     | FIX(l,ce) => 
	   let fun fundef (NO_INLINE_INTO,_,_,_,_) = ()
		 | fundef (fk,f,vl,cl,e) =
	       let val Fun{level,within,escape=ref escape,...} = get f

		   val u' = case u of UNROLL _ => UNROLL level | _ => u

		   fun conform((VAR x)::r,z::l) = (x=z) andalso conform(r,l)
		     | conform(_::r,_::l) = false
		     | conform([],[]) = true
		     | conform _ = false

	       in  within := true; 
		   pass2(0,u',e)
		   before within := false
	       end
	   in  app fundef l;
	       pass2(d+length l,u,ce)
	   end
     | SWITCH(v,c,l) => app (fn e => pass2(d+2,u,e)) l
     | LOOKER(i,vl,w,t,e) => pass2(d+2,u,e)
     | ARITH(i,vl,w,t,e) => pass2(d+2,u,e)
     | PURE(i,vl,w,t,e) => pass2(d+2,u,e)
     | SETTER(i,vl,e) => pass2(d+2,u,e)
     | BRANCH(i,vl,c,e1,e2) => (pass2(d+2,u,e1); 
				pass2(d+2,u,e2))


(* Do loop-header optimizations, elimination of invariant loop arguments,
    hoisting of invariant computations. *)
    

  fun from_outside (_,f,_,_,_) =
      case get f of Fun{escape,call,unroll_call,sibling_call,...} =>
          !escape > 0 orelse
  	   !call > !unroll_call + !sibling_call

  fun loop_opt(bigexp) =
   let exception Gamma_Levmap
     (* For each variable, tell what level of loop nesting at its definition*)
       val levmap : int Intmap.intmap = Intmap.new(16,Gamma_Levmap)
       val level_of' = Intmap.map levmap
       fun level_of(VAR v) = (level_of' v handle Gamma_Levmap => 0 )
	                               (* ^^^ clean this up *)
         | level_of(LABEL v) = level_of (VAR v)
         | level_of _ = 0
       val note_level = Intmap.add levmap

       val _ = app (fn v => note_level(v,0)) fargs

       exception Gamma_Hoistmap
     (* For each level, tell what expressions are hoisted there *)
       val hoistmap : (cexp->cexp) Intmap.intmap = Intmap.new(16,Gamma_Hoistmap)
       fun hoisted_here (lev) = Intmap.map hoistmap lev 
	                          handle Gamma_Hoistmap => (fn e=>e)
       fun any_hoisted_here (lev) = (Intmap.map hoistmap lev; true)
	                          handle Gamma_Hoistmap => false
       fun reset_hoist(lev) = Intmap.rmv hoistmap lev
       fun add_hoist(lev,f) = 
	   let val h = hoisted_here lev
	    in Intmap.add hoistmap (lev, h  o  f)
	   end

   fun gamma_lev(level,e) =
   let fun def w = note_level(w,level)
       fun formaldef wl = app (fn w => note_level(w,level+1)) wl
       fun gamma e = gamma_lev(level,e)
       fun tryhoist(vl,w,e,f) = 
	   let val minlev = foldr Int.min 1000000000 (map level_of vl)
            in if minlev < level
                then (add_hoist(minlev, f);
		      note_level(w, minlev);
		      click "#";
		      gamma e)
                else (def w; f(gamma e))
           end
	   
   in case e
    of RECORD(k,vl,w,ce) => tryhoist(map #1 vl, w, ce, 
				     fn e => RECORD(k,vl, w, e))
     | SELECT(i,v,w,t,ce) => tryhoist([v],w,ce, fn e=>SELECT(i, v, w, t,e))
     | OFFSET(i,v,w,ce) => tryhoist([v],w,ce, fn e=>OFFSET(i, v, w, e))
     | e as APP(v,vl) => e
     | SWITCH(v,c,l) => (def c; SWITCH(v, c, map gamma l))
     | LOOKER(i,vl,w,t,e) => (def w; LOOKER(i, vl, w, t, gamma e))
     | ARITH(i,vl,w,t,e) => (def w; ARITH(i, vl, w, t, gamma e))
     | PURE(i,vl,w,t,e) => tryhoist(vl,w,e, fn e=>PURE(i,vl,w,t,e))
     | SETTER(i,vl,e) => SETTER(i, vl, gamma e)
     | BRANCH(i,vl,c,e1,e2) => (def c; BRANCH(i, vl, c,gamma e1, gamma e2))
     | FIX(l,ce) =>
	   let fun fundef (z as (NO_INLINE_INTO,_,_,_,_)) = z
		 | fundef (fk,f,vl,cl,e) = 
	       let val Fun{escape=ref escape,call,unroll_call,
			   invariant=ref inv,...} = get f

		   val _ = app def vl

	       (* A "loop" is a function called from inside itself.
		  Here we will ensure that any loop has a unique entry
		  point; that is, any loop has only one call from
		  outside itself.  We do this by making a "header"
		  and "pre-header".  Also, any argument passed around
		  the loop but never used is hoisted out.  See also:
 
		   Loop Headers in Lambda-calculus or CPS. Andrew W. Appel.
		   CS-TR-460-94, Princeton University, June 15, 1994. To appear
	           in  _Lisp and Symbolic Computation_ 7, 337-343 (1994).
		   ftp://ftp.cs.princeton.edu/reports/1994/460.ps.Z 
	       *)

	       in  if escape = 0 andalso !unroll_call > 0
		   then let val e' = gamma_lev(level+1,e)
		         in if (!call - !unroll_call > 1 
				 orelse List.exists (fn t=>t) inv
				 orelse any_hoisted_here(level))
				then let val f'::vl' = map copyLvar (f::vl)
					 fun drop(false::r,a::s) = a::drop(r,s)
					   | drop(true::r,_::s) = drop(r,s)
					   | drop _ = nil
					 val newformals=label f' :: map VAR (drop(inv,vl'))
					 val e'' =substitute(newformals,
							     f :: drop(inv,vl),
							     e',
							     false) 
					 val hoisted = hoisted_here level
				     in  click "!"; debugprint(Int.toString f);
					 reset_hoist level;
					 (* app def (f'::vl'); Unnecessary *)
					 enter 0 (fk,f',vl',cl,e'');
					 (fk,f,vl,cl,
					  hoisted(FIX([(fk,f',vl',cl,e'')], 
						      APP(label f', map VAR vl))))
				     end
				else (fk,f,vl,cl,e')
		       end
		   else (fk,f,vl,cl,gamma e)
			
	       end
           in  
	       case split from_outside l
                of ([(fk,f,vl,cl,e)], others as _::_) =>
		  (* for any FIX containing more than one function,
		   but only one of them called from the body of the FIX
		   itself, split into two levels to hide the
		   "auxiliary" functions inside the externally called
		   function. *)
		  let val Fun{sibling_call as ref sib,
			      unroll_call as ref unr,...} = get f
		   in sibling_call := 0;
		      unroll_call := unr + sib;
		      def f;
		      click "`"; (* temporary: *) print "`";
		      app (fn (_,ff,_,_,_) => 
			   let val Fun{sibling_call,...} = get ff
			   in sibling_call := 0  (* this is a conservative
						   estimate, I hope. *)
			   end) 
		         others;
		      gamma(FIX([(fk,f,vl,cl, FIX(others,e))], ce))
		  end
	         (* for any other kind of FIX, proceed with
		    loop detection on each function individually*)
                 | _ =>  (app (def o #2) l;
			  FIX(map fundef l, gamma ce))
           end
     end

      val bigexp' = gamma_lev(1,bigexp)
  in hoisted_here 0 bigexp'
 end

   val rec beta =
    fn RECORD(k,vl,w,ce) => RECORD(k,vl,w,beta ce)
     | SELECT(i,v,w,t,ce) => SELECT(i,v,w,t,beta ce)
     | OFFSET(i,v,w,ce) => OFFSET(i,v,w,beta ce)
     | e as APP(v,vl) => 
	   (case !decisions
	     of YES{formals,body}::rest =>
		 (click "^";
                  case v of VAR vv => debugprint(Int.toString vv) | _ => ();
		  debugflush();
		  decisions := rest;
		  substitute(vl,formals,body,true))
              | NO 1::rest => (decisions := rest; e)
              | NO n :: rest => (decisions := NO(n-1)::rest; e))
     | FIX(l,ce) => 
	   let fun fundef (z as (NO_INLINE_INTO,_,_,_,_)) = z
		 | fundef (fk,f,vl,cl,e) = (fk,f,vl,cl,beta e)
	   in  FIX(map fundef l, beta ce)
	   end
     | SWITCH(v,c,l) => SWITCH(v,c,map beta l)
     | LOOKER(i,vl,w,t,e) => LOOKER(i,vl,w,t,beta e)
     | ARITH(i,vl,w,t,e) => ARITH(i,vl,w,t,beta e)
     | PURE(i,vl,w,t,e) => PURE(i,vl,w,t,beta e)
     | SETTER(i,vl,e) => SETTER(i,vl,beta e)
     | BRANCH(i,vl,c,e1,e2) => BRANCH(i,vl,c,beta e1,beta e2)



   fun pass2_beta(mode,e) =
       (pass2(0,mode,e);
	discard_pass1_info();
	debugprint "Expand: finishing pass2\n"; debugflush();
	case !decisions
         of [NO _] => (debugprint "No expansions to do.\n"; debugflush();
		       e)
	  | _ => (decisions := rev(!decisions);
		  debugprint "Beta: ";
		  beta e
		  before
		  (debugprint "\n"; debugflush())))

 fun prCexp cexp =
	 PPCps.printcps0(fkind,fvar,fargs,ctyl,cexp)


   val gamma = fn c =>
       (print "Before Gamma:\n";
        prCexp c;
        debugprint "Gamma: ";
	let val c' = loop_opt c
         in print "After Gamma:\n";
            prCexp c';
            c'
        end
	before
	(debugprint "\n"; debugflush()))

  in  (* body of expand *)
      notearg fvar;
      app notearg fargs;
(***>
      if !CG.printit then CPSprint.show Control.Print.say cexp
	  else ();
<***)
      debugprint("Expand: pass1: ");
      debugprint(Int.toString(pass1 0 cexp));
      debugprint "\n";
      debugflush();

      if unroll
	 then let val _ = (debugprint(" (unroll)\n"); debugflush());
		  val e' = pass2_beta(UNROLL 0,cexp)
	      in  if !clicked_any 
		      then expand{function=(fkind,fvar,fargs,ctyl,e'),
				  table=typtable,
				  bodysize=bodysize,click=click,unroll=unroll,
				  afterClosure=afterClosure,
				  do_headers=do_headers}
		  else ((*debugprint("\nExpand\n"); 
		         debugflush();
			 (fkind,fvar,fargs,ctyl,pass2_beta(ALL,cexp)) *)
			(fkind,fvar,fargs,ctyl,e'))
	      end
      else if !CG.unroll
	 then let val _ = (debugprint(" (headers)\n"); debugflush())
		  val e' = if do_headers then gamma cexp else cexp
	      in  if !clicked_any
		  then expand{function=(fkind,fvar,fargs,ctyl,e'),
                              table=typtable,bodysize=bodysize,click=click,
                              unroll=unroll,afterClosure=afterClosure, 
                              do_headers=false}
		  else (debugprint(" (non-unroll 1)\n"); debugflush();
			(fkind,fvar,fargs,ctyl,pass2_beta(NO_UNROLL,e')))
	      end
      else (debugprint(" (non-unroll 2)\n"); debugflush();
	    (fkind,fvar,fargs,ctyl,pass2_beta(ALL,cexp)))
  end

end (* local *)
end (* functor Expand *)

(*
 * $Log: expandNEW.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:47  george
 * Version 110.5
 *
 *)
