(* ppnflint.sml *)

structure ppNFlint = struct

local

    open NFlint 

    val say = print 

    exception ppNFlint
    fun ppfbug s = (say ("ppNFlint[" ^ s ^ "]: unknown type/term\n");
                    raise ppNFlint)

    fun ppWhite num =
        let fun white 0 = ""
              | white n = " " ^ white (n - 1)
        in  say(white num)
        end

    val INDENT = 2
    fun indent x = x + INDENT

in

fun ppMult f i [] = ()
  | ppMult f i [x] = f x
  | ppMult f i (x::xs) = (f x; say i; ppMult f i xs)

fun ppVar lv = (*(say "v"; say (Int.toString lv))*)
    let val a = lv div 26;
        val b = lv mod 26;
    in  say (str(chr(ord(#"a") + b)));
        if a = 0 then () else say (Int.toString (a mod 10))
    end
val ppVarSeq = ppMult ppVar ", "

fun ppNty (t : nty) =
    case t of
      NT_VAR lv   => (say "NT_VAR "; ppVar lv)
    | NT_INT      => say "NT_INT"
    | NT_SINT i   => say ("NT_SINT " ^ Int.toString i) 
    | NT_ARRAY t' => (say "NT_ARRAY ("; ppNty t'; say ")")
    | NT_REF t'   => (say "NT_REF ("; ppNty t'; say ")")
    | NT_FIX (tv, i, ts)     => (say "NT_FIX ("; ppVar tv; say (", " ^ (Int.toString i)
                                 ^ ", ["); ppNtySeq ts; say "])")
    | NT_TUPLE ts => (say "NT_TUPLE ["; ppNtySeq ts; say "]")
    | NT_EXIST (tv, t')      => (say "NT_EXIST ["; ppVar tv; say "]("; ppNty t'; say ")")
    | NT_CODE (tvs, ts, ts') => (say "NT_CODE ["; ppVarSeq tvs; say "] ("; ppNtySeq ts;
                                 say ") : ("; ppNtySeq ts'; say ")")
and ppNtySeq ts = ppMult ppNty ", " ts

fun ppVal (v : value) =
    case v of
      VAR lv  => ppVar lv
    | LABEL l => (say "@"; ppVar l)
    | INT i   => say (Int.toString i)
    | SINT i  => say ("SINT(" ^ (Int.toString i) ^ ")")
val ppValSeq = ppMult ppVal ", "

fun ppVp (v, ap) =
    let fun ppAp a =
        case a of
          OFFp i => say ("OFFp " ^ (Int.toString i))
        | SELp (i, a') => (say ("SELp " ^ (Int.toString i)); ppAp a')
    in  say "("; ppVal v; say ", "; ppAp ap; say ")"
    end    
val ppVpSeq = ppMult ppVp ", "

fun ppCop co =
    case co of
      IEQ  => say " == "
    | INEQ => say " != "
    | ILEQ => say " <= "
    | IGEQ => say " >= "
    | ILT  => say " < "
    | IGT  => say " > "

fun ppAop ao =
    case ao of
      MUL => say " * "
    | DIV => say " / "
    | MOD => say " % "
    | SUB => say " - "
    | ADD => say " + "
    | _   => ppfbug "arith op expected"

fun ppNaop po =
    case po of
      MKARRAY   => say "MKARRAY"
    | SUBSCRIPT => say "SUBSCRIPT"
    | UPDATE    => say "UPDATE"
    | REF       => say "REF"
    | DEREF     => say "DEREF"
    | ASSIGN    => say "ASSIGN"
    | _         => ppfbug "non-arith op expected"

fun ppFkind fk =
    case fk of
      KNOWN            => say "KNOWN"
    | CONT             => say "CONT"
    | ESCAPE (SOME ts) => (say "ESCAPE("; app ppNty ts; say ")")
    | ESCAPE NONE      => say "ESCAPE(void)"

fun ppExp (e : lexp) pos =
    let fun ppBoxedSeq [] p = ()
          | ppBoxedSeq [(lv, e1)] p = (ppWhite p; say "("; ppVar lv; say ",\n"; ppExp e1 (indent p); say ")\n")
          | ppBoxedSeq ((lv, e1)::lves) p = (ppWhite p; say "("; ppVar lv; say ",\n"; ppExp e1 (indent p); say "),\n";
                                             ppBoxedSeq lves p)
     in case e of
          RET vs                  => (ppWhite pos; say "return("; ppValSeq vs; say ")")
        | LET (lvs, e1, e2)       => (ppWhite pos; say "let* "; ppVarSeq lvs; say " =\n"; ppExp e1 (indent pos);
                                      say "\n"; ppWhite pos; say "in\n"; ppExp e2 (indent pos))
        | FIX (fds, e1)           => (ppWhite pos; say "fix([\n"; ppFunSeq (indent pos) fds; ppWhite pos;
                                      say "],\n"; ppExp e1 (indent pos); say "\n"; ppWhite pos; say ")")
        | APP (v, ts, vs)         => (ppWhite pos; say "app "; ppVal v; say "["; ppNtySeq ts; say "]";
                                      say " ("; ppValSeq vs; say ")")
        | PTAPP (v, ts, lv, e1)   => (ppWhite pos; say "let "; ppVar lv; say " = app "; ppVal v; say "[";
                                      ppNtySeq ts; say "]"; say " in\n"; ppExp e1 (indent pos))
        | PACK (t, v, t', lv, e1) => (ppWhite pos; say "let "; ppVar lv; say " = pack ["; ppNty t; say ", ";
                                      ppVal v; say "] as "; ppNty t'; say " in\n"; ppExp e1 (indent pos))
        | UNPACK (tv, lv, v, e1)  => (ppWhite pos; say "let ["; ppVar tv; say ", "; ppVar lv;
                                      say "] = unpack "; ppVal v; say " in\n"; ppExp e1 (indent pos))
        | SWITCH (v, es, lves)    => (ppWhite pos; say "switch "; ppVal v; say " :\n";
                                      ppExpSeq (indent pos) es; say "\n";
                                      ppBoxedSeq lves (indent pos))
        | CON (v, t, lv, e1)      => (ppWhite pos; say "let "; ppVar lv; say " = con["; ppNty t; say "](";
                                      ppVal v; say ") in\n"; ppExp e1 (indent pos))
        | RECORD (vps, lv, e1)    => (ppWhite pos; say "let "; ppVar lv; say " = record("; ppVpSeq vps;
                                      say ") in\n"; ppExp e1 (indent pos))
        | SELECT (v, i, lv, e1)   => (ppWhite pos; say "let "; ppVar lv; say " = #"; say (Int.toString i);
                                      say "("; ppVal v; say ") in\n"; ppExp e1 (indent pos))
        | BRANCH (co, vs, e1, e2) => (ppWhite pos; say "if "; ppVal (hd vs); ppCop co; ppVal (hd (tl vs));
                                      say " then\n";
                                      ppExp e1 (indent pos); say "\n"; ppWhite pos; say"else\n";
                                      ppExp e2 (indent pos); say "\n") 
        | PRIMOP (po, vs, lv, e1) => (ppWhite pos; say "let "; ppVar lv; say " = ";
                                      (case po of
                                         (MUL | DIV | MOD | SUB | ADD) =>
                                           (ppVal (hd vs); ppAop po; ppVal (hd (tl vs)))
                                       | _ => (ppNaop po; say "("; ppValSeq vs; say ")"));
                                      say " in\n"; ppExp e1 (indent pos))
     end

and ppExpSeq pos = ppMult (fn x => ppExp x pos) ",\n"

and ppFun (f : fundec) pos =
    let val (fk, lv, ts, lvts, e) = f
        fun ppArgSeq [] = ()
          | ppArgSeq [(v, t)] = (ppVar v; say ":"; ppNty t)
          | ppArgSeq ((v, t)::args) =
              (ppVar v; say ":"; ppNty t; say ", "; ppArgSeq args)
    in  ppWhite pos; ppVar lv;
        say "["; ppVarSeq ts; say "]<"; ppArgSeq lvts; say ">:";
        ppFkind fk; say "(\n";
        ppExp e (indent pos); say "\n"; ppWhite pos; say ")\n"
    end 

and ppFunSeq pos = ppMult (fn x => ppFun x pos) ",\n"

fun ppProg (p : prog) = (say "program [\n"; ppFun p (indent 0); say "]\n")

end (* top-level local *)
end (* strcuture ppNFlint *)
