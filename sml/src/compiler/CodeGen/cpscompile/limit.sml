(* Copyright 1996 by Bell Laboratories *)
(* limit.sml *)

signature LIMIT = sig
  val nolimit : CPS.function list -> 
                  CPS.function list * (CPS.lvar -> (int * int))
end (* signature LIMIT *)

structure Limit : LIMIT = struct

local
  open CPS
in

val say = Control.Print.say
val error = ErrorMsg.impossible
structure CGoptions = Control.CG

val MAX_ALLOC = 1023  (* maximum number of words to allocate per check *)

fun findescapes fl =
  let exception Limit
      val m : fun_kind IntHashTable.hash_table = IntHashTable.mkTable(32,Limit)
      val _ = app (fn (k,f,_,_,_) => IntHashTable.insert m (f,k)) fl
      val escapes = IntHashTable.lookup m 
   in {escapes = escapes,
       check = fn f => case escapes f of
			   KNOWN => IntHashTable.insert m (f,KNOWN_CHECK)
                         | _ => ()}
  end

(* path now counts instructions as well as allocations, for polling *)
fun path escapes fl = 
  let exception Limit'
      val b : cexp IntHashTable.hash_table = IntHashTable.mkTable(32,Limit')
      val _ = app (IntHashTable.insert b o (fn (_,f,_,_,body) => (f,body))) fl
      val body = IntHashTable.lookup b

      val m : {known: fun_kind, alloc: int, instrs: int}
		  IntHashTable.hash_table = 
	          IntHashTable.mkTable(32,Limit')
      val look = IntHashTable.lookup m
      val storeListSz = 2  (* size of store list entry *)
      fun g(d, RECORD(RK_FBLOCK,vl,_,e)) = g(d + (length(vl) * 2) + 2,e)
        | g(d, RECORD(RK_FCONT,vl,_,e)) = g(d + (length(vl) * 2) + 2,e)
	| g(d, RECORD(RK_VECTOR,vl,_,e)) = g(d + length(vl) + 4, e)
	| g(d, RECORD(_,vl,_,e)) = g(d+length(vl)+1, e)
        | g(d, SELECT(_,_,_,_,e)) = g(d, e)
        | g(d, OFFSET(_,_,_,e)) = g(d, e)
        | g(d, SWITCH(_,_,el)) = foldr Int.max 0 (map (fn e => g(d,e)) el)
	| g(d, SETTER(P.assign, _, e)) = g(d+storeListSz, e)
        | g(d, SETTER(P.update,_,e)) = g(d+storeListSz, e)
        | g(d, SETTER(P.boxedupdate,_,e)) = g(d+storeListSz, e)
            (*** should be +0 when unboxedfloat is turned on ***)   
        | g(d, ARITH(P.arith{kind=P.FLOAT 64,...},_,_,_,e)) = g(d+3, e)   
        | g(d, ARITH(P.arith{kind=P.INT _,...},_,_,_,e)) = g(d+1, e)   
	| g(d, ARITH(P.testu _, _, _, _, e)) = g(d+1, e)
	| g(d, ARITH(P.test _, _, _, _, e)) = g(d+1, e)
        | g(d, ARITH(_,_,_,_,e)) = g(d,e)
        | g(d, PURE(P.pure_arith{kind=P.FLOAT 64,...},_,_,_,e)) = g(d+3, e)
        | g(d, PURE(P.real{tokind=P.FLOAT 64,...},_,_,_,e)) = g(d+3, e)       
        | g(d, PURE(P.fwrap,_,_,_,e)) = g(d+4, e)     
        | g(d, PURE(P.iwrap,_,_,_,e)) = g(d+2, e)     
        | g(d, PURE(P.i32wrap,_,_,_,e)) = g(d+2, e)     
	| g(d, PURE(P.newarray0,_,_,_,e)) = g(d+5, e)
	| g(d, PURE(P.makeref, _, _, _, e)) = g(d+2, e)
	| g(d, PURE(P.mkspecial, _, _, _, e)) = g(d+2, e)
        | g(d, LOOKER(P.numsubscript{kind=P.FLOAT 64},_,_,_,e)) = g(d+3, e)
        | g(d, SETTER(_,_,e)) = g(d,e)
        | g(d, LOOKER(_,_,_,_,e)) = g(d,e)
        | g(d, PURE(_,_,_,_,e)) = g(d,e)
        | g(d, BRANCH(_,_,_,a,b)) = Int.max(g(d,a), g(d,b))
        | g(d, APP(LABEL w, _)) = 
             (case maxpath w
	       of {known=KNOWN, alloc=n, instrs=i} => 
		     if d+n > MAX_ALLOC
		     then (IntHashTable.insert m (w,{known=KNOWN_CHECK,
						     alloc=n,
						     instrs=i});
			   d)
                     else d+n
	        | _ => d)
        | g(d, APP(_, _)) = d
(*      | g(d, RECORD(RK_SPILL,vl,_,e)) = g(d + (length(vl) * 4) + 1,e) *)
        | g(d, FIX _) = error "8932 in limit"

      and h(d, RECORD(_,_,_,e)) = h(d+1, e)
        | h(d, SELECT(_,_,_,_,e)) = h(d+1, e)
        | h(d, OFFSET(_,_,_,e)) = h(d+1, e)
        | h(d, SWITCH(_,_,el)) = foldr Int.max 1 (map (fn e => g(d,e)) el)
        | h(d, SETTER(_,_,e)) = h(d+1, e)
        | h(d, ARITH(_,_,_,_,e)) = h(d+1, e)      
        | h(d, PURE(_,_,_,_,e)) = h(d+1, e)      
        | h(d, LOOKER(_,_,_,_,e)) = h(d+1, e)
        | h(d, BRANCH(_,_,_,a,b)) = Int.max(h(d,a), h(d,b)) + 1
        | h(d, APP(LABEL w, _)) = 
             (case maxpath w of 
		  {known=KNOWN, alloc, instrs=i} => d+i
		| _ => d)
        | h(d, APP(_, _)) = d
        | h(d, FIX _) = error "8932.1 in limit"

      and maxpath w = look w handle Limit' =>
	   (* Note that the heap may need to be aligned so g is
	    * called with g(1, bod). Be conservative.
	    *)
            (case escapes w
              of KNOWN => let val bod = body w
			      val n = g(1, bod)
		              val i = h(0, bod)
		              val z = if n>MAX_ALLOC
				      then {known=KNOWN_CHECK,alloc=n,instrs=i}
				      else {known=KNOWN,alloc=n,instrs=i}
		           in IntHashTable.insert m (w,z);
			      z
                          end
               | kind =>  let val bod = body w
			      val z = (IntHashTable.insert m (w,{known=kind,
								 alloc=0,
								 instrs=0});
				       {known=kind,
					alloc=g(1,bod),
					instrs=h(0,bod)})
                          in IntHashTable.insert m (w,z); z
		         end)

      val _ = app (fn (_, x, _, _, _) => (maxpath x; ())) fl;
      val nfl = map (fn (fk,v,args,cl,ce) => (#known(look v),v,args,cl,ce)) fl
   in (nfl, fn x => (let val f = look x in (#alloc f,#instrs f) end))
  end
		         
fun nolimit fl =
  let val {escapes, check} = findescapes fl
      fun makenode (_,f,vl,_,body) =
	  let fun edges (RECORD(_,_,_,e)) = edges e
		| edges (SELECT(_,_,_,_,e)) = edges e
		| edges (OFFSET(_,_,_,e)) = edges e
		| edges (SWITCH(_,_,el)) = List.concat (map edges el)
		| edges (SETTER(_,_,e)) = edges e
		| edges (LOOKER(_,_,_,_,e)) = edges e
		| edges (ARITH(_,_,_,_,e)) = edges e
		| edges (PURE(_,_,_,_,e)) = edges e
		| edges (BRANCH(_,_,_,a,b)) = edges a @ edges b
                | edges (APP(LABEL w, _)) = (case escapes w of KNOWN => [w] 
		                                             | _ => nil)
                | edges (APP _) = nil
		| edges (FIX _) = error "8933 in limit"
	   in (f, edges body)
	  end
   in if !CGoptions.printit
         then (say "Starting feedback..."; Control.Print.flush()) else ();
      app check (Feedback.feedback (map makenode fl));
      if !CGoptions.printit
         then (say "Finished\n"; Control.Print.flush()) else ();
      path escapes fl
  end

val nolimit = fn fl =>
  if !CGoptions.printit
  then let val info as (newfl,limits) = nolimit fl
	   fun showinfo (k,f,_,_,_) = 
	     let val (alloc,instrs) = limits f
		 val s = Int.toString alloc
		 val i = Int.toString instrs
                 val _ = (say (LambdaVar.lvarName f); say "\t")
		 val _ = case k
		          of KNOWN => say "K  "
		           | KNOWN_CHECK => say "H  "
 		           | ESCAPE => say "E  "
                           | CONT => say "C  "
                           | _ => error "nolimit 323 in limit.sml"
	     in (say s; say "\t"; say i; say "\n")
             end

        in app showinfo newfl;
	   info
       end
  else nolimit fl

end (* local *)
end (* structure Limit *)

