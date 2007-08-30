(* Copyright 1996 by Bell Laboratories *)
(* ppcps.sml *)

signature PPCPS =
sig 
  val printcps : (CPS.function * LtyExtern.lty IntHashTable.hash_table) -> unit
  val printcps0: CPS.function -> unit
  val prcps : CPS.cexp -> unit

end (* signature PPCPS *)

structure PPCps : PPCPS =
struct

local open CPS
      structure LV = LambdaVar
in

val say = Control.Print.say

val sayt = say o CPS.ctyToString

fun numkindName (P.INT bits) = "i" ^ Int.toString bits
  | numkindName (P.UINT bits) = "u" ^ Int.toString bits
  | numkindName (P.FLOAT bits) = "f" ^ Int.toString bits

fun lookerName P.! = "!"
  | lookerName P.gethdlr = "gethdlr"
  | lookerName P.subscript = "subscript"
  | lookerName (P.numsubscript{kind}) = ("numsubscript" ^ numkindName kind)
  | lookerName P.getrunvec = "getrunvec"
  | lookerName P.getvar = "getvar"
  | lookerName P.deflvar = "deflvar"
  | lookerName P.getspecial = "getspecial"
  | lookerName P.getpseudo = "getpseudo"
  | lookerName (P.rawload {kind}) = ("rawload" ^ numkindName kind)

fun branchName P.boxed = "boxed"
  | branchName P.unboxed = "unboxed"
  | branchName (P.cmp{oper, kind}) =
    (numkindName kind ^
     (case oper 
      of P.>   => ">"  
       | P.<   => "<"
       | P.>=  => ">=" 
       | P.<=  => "<="
       | P.eql => "="
       | P.neq => "<>" 
      (*esac*)))
  | branchName(P.fcmp{oper, size}) = 
    (numkindName (P.FLOAT size) ^
     (case oper 
      of P.fEQ   => "="
       | P.fULG  => "?<>"
       | P.fGT   => ">"
       | P.fGE   => ">="
       | P.fLT   => "<"
       | P.fLE   => "<="
       | P.fLG   => "<>"
       | P.fLEG  => "<=>"
       | P.fUGT  => "?>"
       | P.fUGE  => "?>="
       | P.fULT  => "?<"
       | P.fULE  => "?<="
       | P.fUE   => "?="
       | P.fUN   => "?"
	   | P.fsgn  => "sgn"
     (*esac*)))  
  | branchName P.pneq = "pneq"
  | branchName P.peql = "peql"
  | branchName P.streq = "streq"
  | branchName P.strneq = "strneq"

fun setterName P.unboxedupdate = "unboxedupdate"
  | setterName P.boxedupdate = "boxedupdate"
  | setterName P.update = "update"
  | setterName (P.numupdate{kind}) = ("numupdate" ^ numkindName kind)
  | setterName P.unboxedassign = "unboxedassign"
  | setterName P.assign = "assign"
  | setterName P.sethdlr = "sethdlr"
  | setterName P.setvar = "setvar"
  | setterName P.uselvar = "uselvar"
  | setterName P.free = "free"
  | setterName P.setspecial = "setspecial"
  | setterName P.setpseudo = "setpseudo"
  | setterName P.setmark = "setmark"
  | setterName P.acclink = "acclink"
  | setterName (P.rawstore {kind}) = ("rawstore" ^ numkindName kind)
  | setterName (P.rawupdate cty) = ("rawupdate" ^ CPS.ctyToString cty)

val cvtParam = Int.toString
fun cvtParams(from, to) = concat [cvtParam from, "_", cvtParam to]

fun arithName (P.arith{oper,kind}) =
    ((case oper of  P.+ => "+" |  P.- => "-" |  P.* => "*"
	          | P./ => "/" |  P.~ => "~" | P.abs => "abs" 
	          | P.fsqrt => "fsqrt"
		  | P.fsin => "sin" | P.fcos => "cos" | P.ftan => "tan"
		  | P.rshift => "rshift" | P.rshiftl => "rshiftl"
	          | P.lshift => "lshift" | P.andb => "andb"
		  | P.orb => "orb" | P.xorb => "xorb" | P.notb => "notb"
		  | P.rem => "rem" | P.div => "div" | P.mod => "mod")
     ^ numkindName kind)
  | arithName(P.test arg) = "test_" ^ cvtParams arg
  | arithName(P.testu arg) = "testu_" ^ cvtParams arg
  | arithName(P.test_inf i) = "test_inf_" ^ cvtParam i
  | arithName (P.round{floor=true,fromkind=P.FLOAT 64,tokind=P.INT 31}) =
      "floor"
  | arithName (P.round{floor=false,fromkind=P.FLOAT 64,tokind=P.INT 31}) =
      "round"
  | arithName (P.round{floor,fromkind,tokind}) =
      ((if floor then "floor" else "round")
       ^ numkindName fromkind ^ "_" ^ numkindName tokind)

fun pureName P.length = "length"
  | pureName (P.pure_arith x) = arithName(P.arith x)
  | pureName P.objlength = "objlength"
  | pureName P.makeref = "makeref"
  | pureName (P.extend arg) = "extend_" ^ cvtParams arg
  | pureName (P.copy arg) = "copy_" ^ cvtParams arg
  | pureName (P.trunc arg) = "trunc_" ^ cvtParams arg
  | pureName (P.trunc_inf i) = "trunc_inf_" ^ cvtParam i
  | pureName (P.copy_inf i) = concat ["copy_", cvtParam i, "_inf"]
  | pureName (P.extend_inf i) =  concat ["extend_", cvtParam i, "_inf"]
  | pureName (P.real{fromkind=P.FLOAT 64,tokind=P.INT 31}) = "real"
  | pureName (P.real{fromkind,tokind}) =
    ("real" ^ numkindName fromkind ^ "_" ^ numkindName tokind)
  | pureName P.subscriptv = "subscriptv"
  | pureName (P.pure_numsubscript{kind}) = ("numsubscriptv" ^ numkindName kind)
  | pureName P.gettag = "gettag"
  | pureName P.mkspecial = "mkspecial"
  | pureName P.wrap = "wrap"
  | pureName P.unwrap = "unwrap"
  | pureName P.cast = "cast"
  | pureName P.getcon = "getcon"
  | pureName P.getexn = "getexn"
  | pureName P.fwrap = "fwrap"
  | pureName P.funwrap = "funwrap"
  | pureName P.iwrap = "iwrap"
  | pureName P.iunwrap = "iunwrap"
  | pureName P.i32wrap = "i32wrap"
  | pureName P.i32unwrap = "i32unwrap"
  | pureName P.getseqdata = "getseqdata"
  | pureName P.recsubscript = "recsubscript"
  | pureName P.raw64subscript = "raw64subscript"
  | pureName P.newarray0 = "newarray0"
  | pureName (P.rawrecord rk) =
    "rawrecord_"^getOpt(Option.map rkstring rk, "notag")
  | pureName (P.condmove b) = "condmove "^branchName b

and rkstring rk = (case rk 
        of RK_VECTOR => "RK_VECTOR"
         | RK_RECORD => "RK_RECORD"
         | RK_SPILL => "RK_SPILL"
         | RK_ESCAPE => "RK_ESCAPE"
         | RK_EXN => "RK_EXN"
         | RK_CONT => "RK_CONT"
         | RK_FCONT => "RK_FCONT"
         | RK_KNOWN => "RK_KNOWN"
         | RK_BLOCK => "RK_BLOCK"
         | RK_FBLOCK => "RK_FBLOCK"
         | RK_I32BLOCK => "RK_I32BLOCK")


fun show0 say =
  let fun sayc (#"\n") = say "\\n"
        | sayc c = say(String.str c)
      
      fun sayv(VAR v) = say(LV.lvarName v)
        | sayv(LABEL v) = say("(L)" ^ LV.lvarName v)
	| sayv(INT i) = say("(I)" ^ Int.toString i)
	| sayv(INT32 i) = say("(I32)" ^ Word32.toString i)
	| sayv(REAL r) = say r
	| sayv(STRING s) = (say "\""; app sayc (explode s); say "\"")
        | sayv(OBJECT _) = say("(object)")
        | sayv(VOID) = say("(void)")

      fun sayvlist [v] = sayv v
        | sayvlist nil = ()
	| sayvlist (v::vl) = (sayv v; say ","; sayvlist vl)


      fun sayrk(RK_RECORD,n) = ()
        | sayrk(RK_VECTOR,n) = ()
        | sayrk(k,n : int) = (say (rkstring k); say " ";
                              say (Int.toString n); say ",")

      fun sayparam ([v],[ct]) = (sayv v; sayt ct)
        | sayparam (nil,nil) = ()
	| sayparam (v::vl,ct::cl) = (sayv v; sayt ct; say ","; sayparam(vl,cl))
        | sayparam _ = ErrorMsg.impossible "sayparam in ppcps.sml"

      fun saypath(OFFp 0) = ()
	| saypath(OFFp i) = (say "+"; say(Int.toString i))
	| saypath(SELp(j,p)) = (say "."; say(Int.toString j); saypath p)
      fun sayvp (v,path) = (sayv v; saypath path)
      fun saylist f [x] = f x | saylist f nil = () 
	| saylist f (x::r) = (f x; say ","; saylist f r)
      fun indent n =
	let fun space 0 = () | space k = (say " "; space(k-1))
	    fun nl() = say "\n"
    	    val rec f =
	     fn RECORD(k,vl,v,c) => (
		  space n;
		  case k of RK_VECTOR => say "#{" | _ => say "{";
                  sayrk(k,length vl);
		  saylist sayvp vl; say "} -> ";
		  sayv(VAR v);
		  nl(); f c)
	      | SELECT(i,v,w,t,c) =>
		    (space n; sayv v; say "."; say(Int.toString i); say " -> ";
		     sayv(VAR w); sayt(t); nl(); f c)
	      | OFFSET(i,v,w,c) =>
		    (space n; sayv v; say "+"; say(Int.toString i); say " -> ";
		    sayv(VAR w); nl(); f c)
	      | APP(w,vl) => 
		    (space n; sayv w; say "("; sayvlist vl; say ")\n")
	      | FIX(bl,c) =>
		    let fun g(_,v,wl,cl,d) = 
			    (space n; sayv(VAR v); say "("; 
			     sayparam (map VAR wl,cl);
			     say ") =\n"; 
                             indent (n+3) d)
		     in app g bl; f c
		    end
	      | SWITCH(v,c,cl) =>
		   let fun g(i,c::cl) =
			(space(n+1); say(Int.toString(i:int));
			 say " =>\n"; indent (n+3) c; g(i+1,cl))
			 | g(_,nil) = ()
		    in space n; say "case "; sayv v; say "  ["; 
		       say(Int.toString(c));
		       say "] of\n"; 
		       g(0,cl)
		   end
	      | LOOKER(i,vl,w,t,e) =>
		   (space n; say(lookerName i); say "("; sayvlist vl;
		    say ") -> "; sayv(VAR w); sayt(t); nl(); f e)
	      | ARITH(i,vl,w,t,e) =>
		   (space n; say(arithName i); say "("; sayvlist vl;
		    say ") -> "; sayv(VAR w); sayt(t); nl(); f e)
	      | PURE(i,vl,w,t,e) =>
		   (space n; say(pureName i); say "("; sayvlist vl;
		    say ") -> "; sayv(VAR w); sayt(t); nl(); f e)
	      | SETTER(i,vl,e) =>
		   (space n; say(setterName i); say "("; sayvlist vl;
		    say ")"; nl(); f e)
	      | BRANCH(i,vl,c,e1,e2) =>
	           (space n; say "if "; say(branchName i);
			 say "("; sayvlist vl; say ") ["; 
                         sayv(VAR c); say "] then\n";
		    indent (n+3) e1;
		    space n; say "else\n";
		    indent (n+3) e2)
	      | RCC(k,l,p,vl,wtl,e) =>
		   (space n; 
                    if k = REENTRANT_RCC then say "reentrant " else ();
                    if l = "" then () else (say l; say " ");
                    say "rcc("; sayvlist vl; say ") -> ";
		    app (fn (w, t) => (sayv (VAR w); sayt(t))) wtl;
		    nl(); f e)
         in f
        end
 in  indent
 end

fun printcps((_,f,vl,cl,e),m)=
let fun ptv(v,t) = (say(LV.lvarName v); say " type ===>>>";
                    say(LtyToString.lt_print t); say "\n")
   
    val _ = if (!Control.CG.debugRep)
            then (say "************************************************\n";
                  IntHashTable.appi ptv m;
                  say "************************************************\n")
            else ()

    fun sayv(v) = say(LV.lvarName v)
    fun sayparam ([v],[ct]) = (sayv v; sayt ct)
      | sayparam (nil,nil) = ()
      | sayparam (v::vl,ct::cl) = (sayv v; sayt ct; say ","; sayparam(vl,cl))
      | sayparam _ = ErrorMsg.impossible "sayparam in ppcps.sml 3435"

 in 
    (say(LV.lvarName f); say "("; sayparam(vl,cl); say ") =\n";  
     show0 say 3 e)
end

exception NULLTABLE
val nulltable : LtyExtern.lty IntHashTable.hash_table =
    IntHashTable.mkTable(8,NULLTABLE)

fun printcps0 f = printcps(f,nulltable)

fun prcps(ce) = show0 (Control.Print.say) 1 ce

end (* toplevel local *)
end (* structure PPCps *)

