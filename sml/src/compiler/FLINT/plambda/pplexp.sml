(* Copyright 1997 by Bell Laboratories *)
(* pplexp.sml *)

signature PPLEXP =
sig

  val printCon : PLambda.con -> unit
  val printLexp : PLambda.lexp -> unit
  val printMatch : StaticEnv.staticEnv ->  
                       (Absyn.pat * PLambda.lexp) list -> unit
  val printFun : PLambda.lexp -> LambdaVar.lvar -> unit

  val stringTag : PLambda.lexp -> string

end (* signature PPLEXP *)


structure PPLexp : PPLEXP = 
struct

local structure A = Absyn
      structure DA = Access
      structure S = Symbol
      structure PP = PrettyPrint
      structure PU = PrintUtil
      structure LT = PLambdaType
      open PLambda PrintUtil 
in 

val say = Control.Print.say
fun sayrep rep = say (DA.prRep rep)
val lvarName = LambdaVar.lvarName

fun bug s = ErrorMsg.impossible ("MCprint: "^s)

fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"
  
val margin = ref 0
fun indent i = margin := !margin + i

exception Undent
  
fun undent i = 
  (margin := !margin - i; if !margin < 0 then raise Undent else ())

fun dent () = tab(!margin)

fun whitespace() =
  let fun ws(n) =
        if n < 0 then raise Undent
        else if n >= 8 then "\t" :: ws(n-8)
             else let val str = case n of 0 => "" | 1 => " " | 2 => "  "
                                        | 3 => "   " | 4 => "    " 
                                        | 5 => "     " | 6 => "      " 
                                        | _ => "       "
                   in [str]
                  end
   in concat(ws(!margin))
  end

fun prCon (DATAcon((sym, _, _), _, v)) = ((S.name sym) ^ " " ^ (lvarName v))
  | prCon (INTcon i) = Int.toString i
  | prCon (INT32con i) = "(I32)" ^ (Int32.toString i)
  | prCon (WORDcon i) = "(W)" ^ (Word.toString i)
  | prCon (WORD32con i) = "(W32)" ^ (Word32.toString i)
  | prCon (REALcon r) = r
  | prCon (STRINGcon s) = PU.mlstr s (* was PU.pr_mlstr s *)
  | prCon (VLENcon n) = Int.toString n

fun printCon x = say (prCon x)

(** use of complex in printLexp may lead to stupid n^2 behavior. *)
fun complex le = 
  let fun h [] = false
        | h (a::r) = g a orelse h r

      and g (FN(_, _, b)) = g b
        | g (FIX(vl, _, ll, b)) = true
        | g (APP(FN _, _)) = true
        | g (APP(l, r)) = g l orelse g r

        | g (LET _) = true
        | g (TFN(_, b)) = g b
        | g (TAPP(l, [])) = g l 
        | g (TAPP(l, _)) = true
        | g (GENOP(_,_,_,_)) = true
        | g (PACK(_, _, _, l)) = g l
       
        | g (RECORD l) = h l
        | g (SRECORD l) = h l
        | g (VECTOR (l, _)) = h l
        | g (SELECT(_, l)) = g l

        | g (SWITCH _) = true
        | g (CON(_, _, l)) = true
(*      | g (DECON(_, _, l)) = true *)

        | g (HANDLE _) = true 
        | g (RAISE(l, _)) = g l
        | g (ETAG (l, _)) = g l

        | g (WRAP(_, _, l)) = g l
        | g (UNWRAP(_, _, l)) = g l
        | g _ = false

   in g le
  end

fun printLexp l = 
  let fun prLty t = say (LT.lt_print t)
      fun prTyc t = say (LT.tc_print t)
      fun prKnd k = say (LT.tk_print k)

      fun plist (p, [], sep) = ()
        | plist (p, a::r, sep) = 
           (p a; app (fn x => (say sep; p x)) r)

      fun g (VAR v) = say(lvarName v)
        | g (INT i) = say(Int.toString i)
        | g (WORD i) = (say "(W)"; say(Word.toString i))
        | g (INT32 i) = (say "(I32)"; say(Int32.toString i))
        | g (WORD32 i) = (say "(W32)"; say(Word32.toString i))
        | g (REAL s) = say s
        | g (STRING s) = say (mlstr s)
        | g (ETAG (l,_)) = g l

        | g (r as RECORD l) =
            if complex r
            then (say "RECORD";
                 indent 7;
                 PU.printClosedSequence ("(",",\n"^whitespace(),")") g l;
                 undent 7)
            else (say "RECORD"; PU.printClosedSequence ("(", ",", ")") g l)

        | g (r as SRECORD l) =
            if complex r
            then (say "SRECORD";
                 indent 7;
                 PU.printClosedSequence ("(",",\n"^whitespace(),")") g l;
                 undent 7)
            else (say "SRECORD"; PU.printClosedSequence ("(", ",", ")") g l)

        | g (r as VECTOR (l, _)) =
            if complex r
            then (say "VECTOR";
                 indent 7;
                 PU.printClosedSequence ("(",",\n"^whitespace(),")") g l;
                 undent 7)
            else (say "VECTOR"; PU.printClosedSequence ("(", ",", ")") g l)

        | g (PRIM(p,t,ts)) = 
              (say ("PRIM (" ^ (PrimOp.prPrimop p) ^ ", "); prLty t; 
               say ", ["; plist(prTyc, ts, ","); say "])")

        | g (l as SELECT(i, _)) =
            let fun gather(SELECT(i,l)) =
                      let val (more,root) = gather l
                       in  (i :: more,root)
                      end
                  | gather l = (nil, l)

                val (path,root) = gather l
                fun ipr (i:int) = say(Int.toString i)
             in g root;
                PU.printClosedSequence ("[",",","]") ipr (rev path)
            end

        | g (FN(v,t,l)) = 
            (say "FN("; say(lvarName v); say " : "; prLty t; say ", ";
             if complex l then (newline(); indent 3; dent();
                                g l; say ")"; undent 3)
             else (g l; say ")"))

        | g (CON((s, c, lt), ts, l)) = 
            (say "CON(("; say(S.name s); say ","; sayrep c; say ",";
             prLty lt; say "), ["; plist(prTyc, ts, ","); say "], ";
             if complex l then (indent 4; g l; say ")"; undent 4)
             else (g l; say ")"))
(*
        | g (DECON((s, c, lt), ts, l)) = 
            (say "DECON(("; say(S.name s); say ","; sayrep c; say ",";
             prLty lt; say "), ["; plist(prTyc, ts, ","); say "], ";
             if complex l then (indent 4; g l; say ")"; undent 4)
             else (g l; say ")"))
*)
        | g (APP(FN(v,_,l),r)) = (say "(APP) "; g (LET(v, r, l)))
        
        | g (LET(v, r, l)) = 
            let val lv = lvarName v
                val len = size lv + 3
             in say lv; say " = ";
                if complex r
                then (indent 2; newline(); dent(); g r; undent 2)
                else (indent len ; g r; undent len);
                newline(); dent(); g l
            end

        | g (APP(l, r)) = 
            (say "APP(";
             if complex l orelse complex r
             then (indent 4; g l; say ",\n"; dent();
                   g r; say ")"; undent 4)
             else (g l; say ",";
                   g r; say ")"))

        | g (TFN(ks, b)) = 
            (say "TFN("; app (fn k => (prKnd k; say ",")) ks; 
             if complex b 
             then (newline(); indent 3; dent(); g b; say ")"; undent 3)
             else (g b; say ")"))
                  
        | g (TAPP(l, ts)) = 
            (say "TAPP("; 
             if complex l 
             then (indent 4; g l; say ",\n"; dent(); say "[";
                   plist(prTyc, ts, ","); say "])"; undent 4)
             else (g l; say ", ["; plist(prTyc, ts, ","); say "])"))

        | g (GENOP(dict, p, t, ts)) = 
              (say ("GENOP (" ^ (PrimOp.prPrimop p) ^ ", "); prLty t; 
               say ", ["; plist(prTyc, ts, ","); say "])")

        | g (PACK(lt, ts, nts, l)) = 
            (say "PACK("; 
             app2 (fn (tc,ntc) => (say "<"; prTyc tc; say ","; prTyc ntc;
                                 say ">,"), ts, nts);
             say " "; prLty lt; say ", ";
             if complex l 
             then (newline(); indent 3; dent(); g l; say ")"; undent 3)
             else (g l; say ")"))

        | g (SWITCH (l,_,llist,default)) =
            let fun switch [(c,l)] =
                      (printCon c; say " => "; indent 8; g l; undent 8)
                  | switch ((c,l)::more) = 
                      (printCon c; say " => ";
                       indent 8; g l; undent 8; newline(); dent(); switch more)
                  | switch [] = bug "unexpected case in switch" 

             in say "SWITCH ";
                indent 7; g l; undent 6; newline(); dent();
                say "of "; indent 3; switch llist;

                case (default,llist)
                 of (NONE,_) => ()
                  | (SOME l,nil) => (say "_ => "; indent 5; g l; undent 5)
                  | (SOME l,_) => (newline(); dent(); say "_ => ";
                                   indent 5; g l; undent 5);

                undent 4
            end

        | g (FIX(varlist,ltylist,lexplist,lexp)) =
            let fun flist([v],[t],[l]) =
                      let val lv = lvarName v
                          val len = size lv + 2
                       in say lv; say " : ";prLty t;say " :: ";
                          indent len ; g l; undent len
                      end
                  | flist(v::vs,t::ts,l::ls) =
                      let val lv = lvarName v
                          val len = size lv + 2
                       in say lv; say " : "; prLty t; say " :: ";
                          indent len ; g l; undent len;
                          newline(); dent(); flist(vs,ts,ls)
                      end
                  | flist(nil,nil,nil) = ()
                  | flist _ = bug "unexpected cases in flist"

             in say "FIX("; indent 4; flist(varlist,ltylist,lexplist); 
                undent 4; newline(); dent(); say "IN  ";
                indent 4; g lexp; say ")"; undent 4
            end

        | g (RAISE(l,t)) = 
            (say "RAISE("; prLty t; say ", "; indent 6; g l; say ")"; undent 6)

        | g (HANDLE (lexp,withlexp)) =
            (say "HANDLE "; indent 7; g lexp; undent 5; newline(); dent();
             say "WITH "; indent 5; g withlexp; undent 7)

        | g (WRAP(t, _, l)) = 
            (say "WRAP("; prTyc t; say ","; indent 5; newline(); dent(); g l; 
             say ")"; undent 5)

        | g (UNWRAP(t, _, l)) = 
            (say "UNWRAP("; prTyc t; say ","; indent 7; 
             newline(); dent(); g l; say ")"; undent 7)

   in g l; newline(); newline()
  end

fun printMatch env ((p,r)::more) =
      (PP.with_pp (ErrorMsg.defaultConsumer())
       (fn ppstrm =>
        (PPAbsyn.ppPat env ppstrm (p,!Control.Print.printDepth);
         PP.newline ppstrm));
       say " => "; printLexp r; printMatch env more)
  | printMatch _ [] = ()

fun printFun l v =
  let fun last (DA.LVAR x) = x 
        | last (DA.PATH(r,_)) = last r
        | last _ = bug "unexpected access in last"

      val rec find =
        fn VAR w => if (v=w)
             then (say("VAR " ^ lvarName v ^ " is free in <lexp>\n");())
             else ()
         | l as FN(w,_,b) => if v=w then printLexp l else find b
         | l as FIX(vl,_,ll,b) => 
             if List.exists (fn w => v=w) vl then printLexp l
             else (app find ll; find b)
         | APP(l,r) => (find l; find r)
         | LET(w,l,r) => (if v=w then printLexp l else find l; find r)
         | PACK(_,_,_,r) => find r
         | TFN(_, r) => find r
         | TAPP(l, _) => find l
         | SWITCH (l,_,ls,d) =>
             (find l; app (fn(_,l) => find l) ls;
              case d of NONE => () | SOME l => find l)
         | RECORD l => app find l 
         | SRECORD l => app find l 
         | VECTOR (l, t) => app find l 
         | SELECT(_,l) => find l
         | CON((_, DA.EXN p, _), _, e) => (find(VAR(last p)); find e)
         | CON(_,_,e) => find e
(*
         | DECON((_, DA.EXN p, _), _, e) => (find(VAR(last p)); find e)
         | DECON(_,_,e) => find e  
*)
         | HANDLE(e,h) => (find e; find h) 
         | RAISE(l,_) => find l
         | INT _ => () | WORD _ => () 
         | INT32 _ => () | WORD32 _ => () 
         | STRING _ => () | REAL _ => ()
         | ETAG (e,_) => find e
         | PRIM _ => ()
         | GENOP ({default=e1,table=es}, _, _, _) => 
             (find e1; app (fn (_, x) => find x) es)
         | WRAP(_, _, e) => find e
         | UNWRAP(_, _, e) => find e

   in find l
  end

fun stringTag (VAR _) = "VAR"
  | stringTag (INT _) = "INT"
  | stringTag (INT32 _) = "INT32"
  | stringTag (WORD _) = "WORD"
  | stringTag (WORD32 _) = "WORD32"
  | stringTag (REAL _) = "REAL"
  | stringTag (STRING _) = "STRING"
  | stringTag (PRIM _) = "PRIM"
  | stringTag (GENOP _) = "GENOP"
  | stringTag (FN _) = "FN"
  | stringTag (FIX _) = "FIX"
  | stringTag (APP _) = "APP"
  | stringTag (LET _) = "LET"
  | stringTag (TFN _) = "TFN"
  | stringTag (TAPP _) = "TAPP"
  | stringTag (ETAG _) = "ETAG"
  | stringTag (RAISE _) = "RAISE"
  | stringTag (HANDLE _) = "HANDLE"
  | stringTag (CON _) = "CON"
  | stringTag (SWITCH _) = "SWITCH"
  | stringTag (VECTOR _) = "VECTOR"
  | stringTag (RECORD _) = "RECORD"
  | stringTag (SRECORD _) = "SRECORD"
  | stringTag (SELECT _) = "SELECT"
  | stringTag (PACK _) = "PACK"
  | stringTag (WRAP _) = "WRAP"
  | stringTag (UNWRAP _) = "UNWRAP"

end (* toplevel local *)
end (* struct PPLexp *)


