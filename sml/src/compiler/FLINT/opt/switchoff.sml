(* is there a copyright of some kind ? Well, anyway : 2000 YALE FLINT PROJECT*)
(* dtelle@ens-lyon.fr / teller-david@cs.yale.edu *)

signature SWITCHOFF =
sig
    val switchoff : FLINT.prog -> FLINT.prog
end

structure Switchoff :> SWITCHOFF =
struct
local
    structure F  = FLINT
    structure O  = Option
    structure S  = IntRedBlackSet
    structure I  = Intmap
    structure X  = TextIO
in

fun bug msg = ErrorMsg.impossible ("Switchoff: "^msg)
exception NotFound
(*this function is supposed to remove as many embedded witches
 *as possible, by rewriting the program into a continuation-style
 *and letting the inliner do the tough job*)

fun switchoff (prog as (progkind, progname, progargs, progbody)) = let
val mklv = LambdaVar.mkLvar
val {getLty,...} = Recover.recover (prog, false)

(*does the set contain this variable ?
 *contains : 'a intmap -> int -> bool*)
fun contains set var =
    (I.map set var;
     true) handle NotFound => false

(*put source into destination
 *setUnion : 'a intmap -> 'a intmap -> 'a intmap*)
and setUnion destination source =
	I.app (I.add destination) source

(*remove all members of elements from from
 setRemove : 'a intmap -> int list -> 'a intmap*)
and setRemove from elements =
	app (I.rmv from) elements

(**)
and inCase set (pat, body) =
    let val (result, variables) = inside body
    in 
	I.app (I.add set) variables;
	case pat of
	    F.DATAcon(_,_,var) => I.rmv variables var
	  |_               => ();
	(pat, result)	 
    end

and inDefault set default = 
    case default of
	SOME expression =>
	let val (result, variables) = inside expression
	in
	    I.app (I.add set) variables;
	    SOME result
	end
      | _ => NONE

and putFunInExpr funLVar (expr : F.lexp) =
    let val newVar = mklv()
    in
	F.LET([newVar], expr, F.APP(funLVar, [F.VAR newVar]))
    end

and putFunInCase funLVar (pat, body : F.lexp) = 
    (pat, putFunInExpr funLVar body)

and putFunInDefault funLVar expr =
    case expr of
	SOME expression => SOME (putFunInExpr funLVar expression)
      | _ => NONE

and valueAdder set value = case value of
	F.VAR v => I.add set (v, true)
	| _   => ()

and fst (a,_) = a

(*
 * inside : tree -> (tree, bool intmap)
 *                  (rewritten expression, set of free variables)
 *)
and inside tree =
case tree of
    F.LET (assign as [x], 
	   F.SWITCH(name as F.VAR(y), conds, handles, default), cont) =>
    let val (newCont, nextVariables) = inside cont
	val optHandles = map (inCase nextVariables) handles
	val optDefault = inDefault nextVariables default
    in
	if contains nextVariables x
	then (
	      (*cannot optimize*)
	      I.rmv nextVariables x;
	      (F.LET(assign,
		     F.SWITCH
			 (
			  name,
			  conds,
			  optHandles,
			  optDefault
			  ),
			 newCont),
	       nextVariables))
	else (
	      (*optimize*)
	      I.add nextVariables (y, true);
	      let val newFunID = (mklv())
		  val newFunction = (F.VAR newFunID)
		  val optimized =
		      F.FIX
			  (
			   [({
			     inline = F.IH_MAYBE(1, [2]),
			     known  = true, (*?*)
			     cconv  = F.CC_FUN LtyKernel.FF_FIXED,
			     isrec  = NONE
			     },
			     newFunID, [(x, getLty (F.VAR x))], newCont)],
			   F.SWITCH(name,
				    conds,
				    map (putFunInCase newFunction) handles,
				    putFunInDefault newFunction default))
	      in
		  (optimized, nextVariables)
	      end)
    end
  | F.LET (vars, exp, block) =>
    let val (optExpr, varExpr) = inside exp
	val (optBlock, varBlock) = inside block
    in
	setUnion  varExpr varBlock;
	setRemove varExpr vars;
	(F.LET(vars, optExpr, optBlock), varExpr)
    end
  | F.RET values => 
    let val vars = I.new(8, NotFound)
    in
	map (valueAdder vars) values;
	(tree, vars)
    end
  | F.FIX (funs, block) => 
    let val (optBlock, varBlock) = inside block
    in
	let fun aux (kind, name, args, body) = 
		let val (optBody, varBody) = inside body
		in  
		    (*first remove local "variables"*)
		    setRemove varBody (map fst args);
		    (*then join the sets*)
		    setUnion varBlock varBody;
		    (kind, name, args, optBody)		    
		end
	    fun removeFuns (_, name, _, _) =
		(*now, remove function names*)
		I.rmv varBlock name;
	in
	    let val l = map aux funs
	    in
		app removeFuns l;
		(F.FIX(l, optBlock), varBlock)
	    end
	end
    end
  | F.APP (applied, appliedOn) =>
    let val vars = I.new (8, NotFound)
    in
	let val adder = valueAdder vars
	in
	    adder applied;
	    app adder appliedOn;
	    (tree, vars)
	end
    end
  | F.TFN (dec, block) => 
    let val (kind, var, cons, exp) = dec
    in
	let val (optExp, varExp)     = inside exp
	    val (optBlock, varBlock) = inside block
	in
	    setUnion varExp varBlock;
	    I.rmv varBlock var;
	    (F.TFN((kind, var, cons, optExp), optBlock), varBlock)
	end
    end
  | F.TAPP (value, _) =>
    let val vars = I.new(8, NotFound)
    in
	valueAdder vars value;
	(tree, vars)
    end
  | F.SWITCH (value, conds, handles, default) =>
    let val vars = I.new(8, NotFound)
	val optDefault = inDefault vars default
	val optHandles = map (inCase vars) handles
    in
	(F.SWITCH (value, conds, optHandles, optDefault), vars)
    end
  | F.CON (dbg, cons, value, var, block) =>
    let val (optBlock, varBlock) = inside block in 
	valueAdder varBlock value;
	I.rmv varBlock var;
	(F.CON(dbg, cons, value, var, optBlock), varBlock)	
    end
  | F.RECORD (kind, values, var, block) =>
    let val (optBlock, varBlock) = inside block
    in
	let val adder = valueAdder varBlock
	in
	    app adder values;
	    I.rmv varBlock var;
	    (F.RECORD(kind, values, var, optBlock), varBlock)
	end
    end
  | F.SELECT (value, index, var, block) =>
    let val (optBlock, varBlock) = inside block
    in
	valueAdder varBlock value;
	I.rmv varBlock var;
	(F.SELECT (value, index, var, optBlock), varBlock)
    end
  | F.RAISE  (value, _) =>
    let val vars = I.new(8, NotFound)
    in
	valueAdder vars value;
	(tree, vars)
    end 
  | F.HANDLE (exp, value) =>
    let val (optExp, varExp) = inside exp
    in
	valueAdder varExp value;
	(F.HANDLE (optExp, value), varExp)
    end
  | F.BRANCH (operation, values, expr, block) =>
    let val (optBlock, varBlock) = inside block
	val (optExp, varExp)     = inside expr
    in
	let val adder = valueAdder varBlock
	in
	    app adder values
	end;
	setUnion varBlock varExp;
	(F.BRANCH (operation, values, optExp, optBlock), varBlock)
    end
  |  F.PRIMOP (operation, values, variable, block) =>
     let val (optBlock, varBlock) = inside block
	 val adder = valueAdder varBlock
     in
	 app adder values;
	 I.add varBlock (variable, true);
	 (F.PRIMOP (operation, values, variable, optBlock), varBlock)
     end
	 
in 
    let val (result, _) = inside progbody in
	(progkind, progname, progargs, result)
    end
end
end
end
