(* copyright 1998 YALE FLINT PROJECT *)

signature COLLECT =
sig
    
    (* Collect information about variables and function uses
     * the info is accumulated in the map `m' *)
    val collect : FLINT.fundec -> unit

    (* query functions *)
    val recursive : FLINT.lvar -> bool
    val escaping  : FLINT.lvar -> bool	(* non-call uses *)
    val usenb     : FLINT.lvar -> int	(* nb of non-recursive uses *)
    (* val callnb    : FLINT.lvar -> int *)

    (* fix up function to keep counts up-to-date *)
    val unuselexp : (FLINT.lvar -> unit) -> FLINT.lexp -> unit

    (* inc the "true=call,false=use" count *)
    val use    : bool -> FLINT.lvar -> unit
    (* dec the "true=call,false=use" count and call the function if zero *)
    val unuse  : (FLINT.lvar -> unit) -> bool -> FLINT.lvar -> unit
    (* transfer the counts of var1 to var2 *)
    val transfer : FLINT.lvar * FLINT.lvar -> unit

    (* This allows to execute some code and have all the resulting
     * changes made to the internal (for recursion) counters instead
     * of the external ones. For instance:
     *     inside f (fn () => call ~1 f)
     * would decrement the count of recursive function calls of f *)
    val inside : FLINT.lvar -> (unit -> 'a) -> 'a

    (* mostly useful for PPFlint *)
    val LVarString : FLINT.lvar -> string
end

structure Collect :> COLLECT =
struct
local
    structure F  = FLINT
    structure M  = Intmap
    structure LV = LambdaVar
    structure PP = PPFlint
in

val say = Control.Print.say
fun bug msg = ErrorMsg.impossible ("Collect: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; say " "; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; say " "; bug msg)
fun ASSERT (true,_) = ()
  | ASSERT (FALSE,msg) = bug ("assertion "^msg^" failed")

	
datatype info
  (* for functions we keep track of calls and escaping uses
   * and separately for external and internal (recursive) references *)
  = Fun of {ecalls: int ref, euses: int ref,
	    inside: bool ref,
	    icalls: int ref, iuses: int ref}
  | Var of int ref	(* for other vars, a simple use count is kept *)
    
exception NotFound
	      
val m : info Intmap.intmap = M.new(128, NotFound)

(* map related helper functions *)
val get = M.map m
fun new true lv = M.add m (lv, Fun{ecalls=ref 0, euses=ref 0,
				   inside=ref false,
				   icalls=ref 0, iuses=ref 0})
  | new false lv = M.add m (lv, Var(ref 0))

fun LVarString lv =
    (LV.lvarName lv)^
    ((case get lv of
	 Var uses => "{"^(Int.toString (!uses))^"}"
       | Fun {ecalls,euses,icalls,iuses,...} => 
	 concat
	     ["{(", Int.toString (!ecalls), ",", Int.toString (!euses),
	      "),(", Int.toString (!icalls), ",", Int.toString (!iuses), ")}"])
	 handle NotFound => "{?}")

fun transfer (lv1,lv2) =
    case get lv1
     of Var uses1 =>
	(case get lv2
	  of (Var uses2 | Fun {euses=uses2,...}) =>
	     (uses2 := !uses2 + !uses1; uses1 := 0))
      | Fun {inside=i1,euses=eu1,iuses=iu1,ecalls=ec1,icalls=ic1,...} =>
	(ASSERT(!iu1 + !ic1 = 0 andalso not(!i1), "improper fun transfer");
	 case get lv2
	  of Fun {inside=i2,euses=eu2,iuses=iu2,ecalls=ec2,icalls=ic2,...} =>
	     if !i2 then
		 (iu2 := !iu2 + !eu1; eu1 := 0;
		  ic2 := !ic2 + !ec1; ec1 := 0)
	     else
		 (eu2 := !eu2 + !eu1; eu1 := 0;
		  ec2 := !ec2 + !ec1; ec1 := 0)
	   | Var uses =>
	     (uses := !uses + !eu1;
	      eu1 := 0; ec1 := 0))
	       
fun inc ri = (ri := !ri + 1)
fun dec ri = (ri := !ri - 1)

fun use call lv =
    inc (case get lv
	  of Var uses => uses
	   | (Fun {inside=ref true, iuses=uses,icalls=calls,...} |
	      Fun {inside=ref false,euses=uses,ecalls=calls,...}) =>
	     (if call then inc calls else (); uses))

fun unuse undertaker call lv =
    let fun check uses =
	    if !uses < 0 then
		bugval ("decrementing too much", F.VAR lv)
	    else if !uses = 0 then
		undertaker lv
	    else ()
    in case get lv
	of Var uses => (dec uses; check uses)
	 | Fun {inside=ref false,euses=uses,ecalls=calls,...} =>
	   (if call then dec calls else (); dec uses; check uses)
	 | Fun {inside=ref true, iuses=uses,icalls=calls,...} =>
	   (if call then dec calls else (); dec uses)
    end

fun usenb lv     = case get lv of (Fun{euses=uses,...} | Var uses) => !uses
fun used lv      = usenb lv > 0
fun recursive lv = case get lv of (Fun{iuses=uses,...} | Var uses) => !uses > 0
(* fun callnb lv    = case get lv of Fun{ecalls,...} => !ecalls | Var us => !us *)
fun escaping lv =
    case get lv
     of Fun{iuses,euses,icalls,ecalls,...}
	=> !euses + !iuses > !ecalls + !icalls
      | Var us => !us > 0 (* arbitrary, but I opted for the "safe" choice *)

(* census of the internal part *)    
fun inside f thunk =
    case get f
     of Fun{inside=inside as ref false,...} =>
	(inside := true; thunk() before inside := false)
      | Fun _ => (say "\nalready inside "; PP.printSval(F.VAR f); thunk())
      | _ => bugval("trying to get inside a non-function", F.VAR f)

fun census new use = let
    (* val use = if inc then use else unuse *)
    fun call lv = use true lv
    val use = fn F.VAR lv => use false lv | _ => ()
    val newv = new false
    val newf = new true
    fun id x = x

    fun impurePO po = true		(* if a PrimOP is pure or not *)

    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(call default; app (call o #2) table)

    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) = call lv
      | cdcon _ = ()

    (* the actual function:
     * `uvs' is an optional list of booleans representing which of 
     * the return values are actually used *)
    fun cexp uvs lexp =
	case lexp
	 of F.RET vs => app use vs
(* 	    (case uvs *)
(* 	      of SOME uvs => (* only count vals that are actually used *) *)
(* 		 app (fn(v,uv)=>if uv then use v else ()) (ListPair.zip(vs,uvs)) *)
(* 	       | NONE => app use vs) *)

	  | F.LET (lvs,le1,le2) =>
	    (app newv lvs; cexp uvs le2; cexp (SOME(map used lvs)) le1)

	  | F.FIX (fs,le) =>
	    let fun cfun ((_,f,args,body):F.fundec) = (* census of a fundec *)
		    (app (newv o #1) args; inside f (fn()=> cexp NONE body))
		fun cfix fs = let	(* census of a list of fundecs *)
		    val (ufs,nfs) = List.partition (used o #2) fs
		in if List.null ufs then ()
		   else (app cfun ufs; cfix nfs)
		end
	    in app (newf o #2) fs; cexp uvs le; cfix fs
	    end
	       
	  | F.APP (F.VAR f,vs) =>
	    (call f; app use vs)

	  | F.TFN ((tf,args,body),le) =>
	    (newf tf; cexp uvs le;
	     if used tf then inside tf (fn()=> cexp NONE body) else ())

	  | F.TAPP (F.VAR tf,tycs) => call tf

	  | F.SWITCH (v,cs,arms,def) =>
	    (use v; Option.map (cexp uvs) def;
	     (* here we don't absolutely have to keep track of vars bound within
	      * each arm since these vars can't be eliminated anyway *)
	     app (fn (F.DATAcon(dc,_,lv),le) => (cdcon dc; newv lv; cexp uvs le)
		   | (_,le) => cexp uvs le)
		 arms)
		
	  | F.CON (dc,_,v,lv,le) =>
	    (cdcon dc; newv lv; cexp uvs le; if used lv then use v else ())

	  | F.RECORD (_,vs,lv,le) =>
	    (newv lv; cexp uvs le; if used lv then app use vs else ())

	  | F.SELECT (v,_,lv,le) =>
	    (newv lv; cexp uvs le; if used lv then use v else ())

	  | F.RAISE (v,_) => use v
	  | F.HANDLE (le,v) => (use v; cexp uvs le)
	  
	  | F.BRANCH (po,vs,le1,le2) =>
	    (app use vs; cpo po; cexp uvs le1; cexp uvs le2)
	  
	  | F.PRIMOP (po,vs,lv,le) =>
	    (newv lv; cexp uvs le;
	     if impurePO po orelse used lv then (cpo po; app use vs) else ())
	  
	  | le => buglexp("unexpected lexp", le)
in
    cexp
end

(* fun update 1 lexp    = census (fn _ => fn _ =>()) use NONE lexp *)
(*   | update (~1) lexp = census (fn _ => fn _ =>()) unuse NONE lexp *)
(*   | update _ lexp = bug ("non-unary update", lexp) *)

fun unuselexp undertaker lexp =
    census (fn _ => fn _ =>()) (unuse undertaker) NONE lexp

fun collect (fdec as (_,f,_,_)) =
    (M.clear m;		(* start from a fresh state *)
     census new use NONE (F.FIX([fdec], F.RET[F.VAR f])))

end
end
