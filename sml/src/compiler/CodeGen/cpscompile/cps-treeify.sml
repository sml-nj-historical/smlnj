signature CPS_TREEIFY = sig
  datatype treeify = TREEIFY | COMPUTE | DEAD

  val usage : CPS.function list -> (CPS.lvar -> treeify)
end


structure CpsNoTreeify : CPS_TREEIFY = 
struct
  datatype treeify = TREEIFY | COMPUTE | DEAD 
  val usage = fn _ => fn _ => COMPUTE
end



structure CpsTreeify : CPS_TREEIFY = 
struct
  structure C = CPS

  datatype treeify = TREEIFY | COMPUTE | DEAD

  fun error msg = ErrorMsg.impossible ("FPStack." ^ msg)

  fun usage fl = let
   (* Table to record number of uses *)
    exception UseCntTbl
    val useCntTbl : treeify  Intmap.intmap = Intmap.new(32, UseCntTbl)
    fun uses v = (Intmap.map useCntTbl v) handle UseCntTbl => DEAD
    fun addUse v = let
      val add = Intmap.add useCntTbl
    in
      case uses v
       of DEAD => add(v, TREEIFY)
        | TREEIFY => add(v, COMPUTE)
	| _ => ()
    end
    fun addValue(C.VAR v) = addUse v
      | addValue _ = ()

    fun cntUsesCps(C.RECORD(_, vl, w, e)) =
	 (app addValue (map #1 vl); cntUsesCps e)
      | cntUsesCps(C.SELECT(i, v, x, _, e)) = (addValue v; cntUsesCps e)
      | cntUsesCps(C.OFFSET(i, v, x, e)) = (addValue v; cntUsesCps e)
      | cntUsesCps(C.APP(v, vl)) = (addValue v; app addValue vl)
      | cntUsesCps(C.FIX _) = error "pass1: FIX"
      | cntUsesCps(C.SWITCH(v, _, el)) = (addValue v; app cntUsesCps el)
      | cntUsesCps(C.BRANCH(_, vl, _, c1, c2)) =
	 (app addValue vl; cntUsesCps c1; cntUsesCps c2)
      | cntUsesCps(C.SETTER(_, vl, e)) = (app addValue vl; cntUsesCps e)
      | cntUsesCps(C.LOOKER(looker, vl, x, _, e)) = 
	 (app addValue vl; 
	  (* floating subscript cannot move past a floating update.
	   * For now subscript operations cannot be treeified.
	   * This is hacked by making it (falsely) used more than once.
	   *)
	  case looker
	   of C.P.numsubscript{kind=C.P.FLOAT _} => (addUse x; addUse x)
	    | _ => ()
          (*esac*);
	  cntUsesCps e)
      | cntUsesCps(C.ARITH(_, vl, _, _, e)) = (app addValue vl; cntUsesCps e)
      | cntUsesCps(C.PURE(_, vl, _, _, e)) = (app addValue vl; cntUsesCps e)
  in 
    app (fn (_, _, _, _, e) => cntUsesCps e) fl;
    uses

  end
end
