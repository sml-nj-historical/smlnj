(* Copyright 1996 by AT&T Bell Laboratories *)
(* conrep.sml *)

signature CONREP = 
sig

val infer : bool -> (Symbol.symbol * bool * Types.ty) list
                    -> (Access.conrep list * Access.consig)

end (* signature CONREP *)


structure ConRep : CONREP =
struct

local open Access Types
in 

fun err s = ErrorMsg.impossible ("Conrep: "^s)

fun count predicate l =
  let fun test (a::rest,acc) = test (rest,if predicate a then 1+acc else acc)
        | test (nil,acc) = acc
   in test (l,0)
  end

fun reduce ty =
  case TypesUtil.headReduceType ty
   of POLYty{tyfun=TYFUN{body,...},...} => reduce body
    | ty => ty

fun notconst(_,true,_) = false
  | notconst(_,_,CONty(_,[t,_])) = 
      (case (reduce t) 
        of CONty(RECORDtyc nil,_) => false
         | _ => true)
  | notconst _ = true

(* 
 * fun show((sym,_,_)::syms, r::rs) = 
 *      (print(Symbol.name sym); print ":   "; 
 *      PPBasics.ppRep r; print "\n"; show(syms,rs))
 *   | show _ = (print "\n")
 *)

(* the first argument indicates whether this is a recursive datatypes *)
fun infer false ([(_, false, CONty(_,[ty,_]))]) = 
      (case (reduce ty) 
        of (CONty(RECORDtyc nil, _)) => ([CONSTANT 0], CSIG (0,1))
         | _ => ([UNTAGGED], CSIG(1,0)) (* [TRANSPARENT] *)) 
      (* The TRANSPARENT conrep is temporarily turned off;
         it should be working very soon. Ask zsh. *)

  | infer _ cons =
      let val multiple = (count notconst cons) > 1

	  fun decide (ctag,vtag, (_,true,_)::rest, reps) = 
                if multiple andalso !Control.CG.boxedconstconreps
                then decide(ctag, vtag+1, rest, (TAGGED vtag) :: reps)
                else decide(ctag+1, vtag, rest, (CONSTANT ctag) :: reps)

	    | decide (ctag,vtag, (_,false,CONty(_,[ty,_]))::rest, reps) =
		(case (reduce ty, multiple)
		  of (CONty(RECORDtyc nil,_),_) => 
		       decide(ctag+1, vtag, rest, (CONSTANT ctag) :: reps)
                   | (_, true) =>  
                       decide(ctag, vtag+1, rest, (TAGGED vtag) :: reps)
                   | (_, false) => 
                       decide(ctag, vtag+1, rest, (UNTAGGED :: reps)))
            | decide (_, _, _::_, _) = err "unexpected conrep-decide"
            | decide (ctag, vtag, [], reps) = (rev reps, CSIG(vtag,ctag))

       in decide(0, 0, cons, [])
      end

(*** val infer = fn l => let val l' = infer l in show(l,l'); l' end ***)

end (* local *)
end (* structure ConRep *)


(*
 * $Log: conrep.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:09  george
 *   Version 109.24
 *
 *)
