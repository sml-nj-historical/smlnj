(* uses a union-find data structure to compute clusters *)
(* First function in the function list must be the first function 
 * in the first cluster. This is achieved by ensuring that the first  
 * function is mapped to the smallest id in a dense enumeration. 
 * This function id will map to the smallest cluster id. 
 * The function ids are then iterated in descending order.
 *)
structure Cluster : 
  sig
     val cluster : CPS.function list -> CPS.function list list
  end = 
struct
  fun error msg = ErrorMsg.impossible ("Cluster." ^ msg)

  fun cluster funcs = let
    val numOfFuncs = length funcs

    (* mapping of function names to a dense integer range *)
    exception FuncId
    val funcToIdTbl : int IntHashTable.hash_table =
	IntHashTable.mkTable(numOfFuncs, FuncId)
    val lookup = IntHashTable.lookup funcToIdTbl 

    (* mapping of ids to functions *)
    val idToFuncTbl = Array.array(numOfFuncs, hd funcs)
    local
      val add = IntHashTable.insert funcToIdTbl
      fun mkFuncIdTbl ([], _) = ()
	| mkFuncIdTbl ((func as (_,f,_,_,_))::rest, id) = 
	    (add (f, id);  
	     Array.update(idToFuncTbl, id, func); 
	     mkFuncIdTbl(rest, id+1))
    in
      val _ = mkFuncIdTbl(funcs, 0)
    end

    (* union-find structure -- initially each function in its own cluster *)
    val trees = Array.tabulate(numOfFuncs, fn i => i)

    fun ascend u = let
      val v = Array.sub(trees, u)
    in if v = u then u else ascend(v)
    end
 
    fun union(t1, t2) = let
      val r1 = ascend t1
      val r2 = ascend t2
    in
      if r1 = r2 then ()		(* already in the same set *)
      else if r1 < r2 then Array.update(trees, r2, r1)
	   else Array.update(trees, r1, r2)
    end

    (* build union-find structure *)
    fun build [] = ()
      | build ((_,f,_,_,body)::rest) = let
          val fId = lookup f
	  fun forall []      = ()
	    | forall (e::es) = (calls e; forall es)

	  and calls (CPS.APP(CPS.LABEL l,_))  = union(fId, lookup l)
	    | calls (CPS.APP _)	 	      = ()
	    | calls (CPS.RECORD(_,_,_,e))     = calls e
	    | calls (CPS.SELECT(_,_,_,_,e))   = calls e
	    | calls (CPS.OFFSET(_,_,_,e))     = calls e
	    | calls (CPS.SWITCH(_,_,es))      = forall(es)
	    | calls (CPS.BRANCH(_,_,_,e1,e2)) = (calls e1; calls e2)
	    | calls (CPS.SETTER(_,_,e))       = calls e
	    | calls (CPS.LOOKER(_,_,_,_,e))   = calls e
	    | calls (CPS.ARITH(_,_,_,_,e))    = calls e
	    | calls (CPS.PURE(_,_,_,_,e))     = calls e
	    | calls (CPS.RCC(_,_,_,_,_,e))  = calls e
	    | calls (CPS.FIX _)               = error "calls.f:FIX"
        in 
	  calls body; build rest
	end (* build *)

    (* extract the clusters. 
     * The first func in the funcs list must be the first function
     * in the first cluster.
     *)
    fun extract() = let
      val clusters = Array.array(numOfFuncs, [])
      fun collect n = let
	val root = ascend(n)
	val func = Array.sub(idToFuncTbl, n)
	val cluster = Array.sub(clusters, root)
      in 
	Array.update(clusters, root, func::cluster); 
	collect (n-1)
      end

      fun finish(~1, acc) = acc
	| finish(n, acc) = 
	  (case Array.sub(clusters, n)
	    of [] => finish (n-1, acc)
             | cluster => finish(n-1, cluster::acc)
	   (*esac*))
    in
      collect (numOfFuncs-1) handle _ => ();
      finish (numOfFuncs-1, [])
    end
  in
    build funcs;
    extract()
  end (* cluster *)
end

