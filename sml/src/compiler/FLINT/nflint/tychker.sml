(* tychker.sml *)

structure NFlintChecker = struct

local
    open NFlint
    open ppNFlint

    structure S  = IntRedBlackSet
    structure M  = IntRedBlackMap
    structure L  = List
    structure LP = ListPair
    structure LV = LambdaVar

    exception NFCHECK
    fun bug p s = (print ("\n" ^ p ^ "typechecking error : " ^ s ^ "!\n"); raise NFCHECK)
in

type tyenv = nty M.map

exception envUnbound

val initTenv : tyenv = M.empty
val initVenv : tyenv = M.empty
  
val envAdd = M.insert

fun envAddM (env, xs, vs) =
    let fun add ((x, v), e) = envAdd (e, x, v)
    in  foldl add env (LP.zip (xs, vs))
    end

fun envLookup (env, lv) =
    case M.find (env, lv) of
      NONE   => (print "\nunbound var ===> "; ppVar lv; print "\n"; raise envUnbound)
    | SOME v => v

(*
fun envLookupF (env, lv, fail) =
    case M.find (env, lv) of
      NONE   => fail
    | SOME v => v

fun envLookup (env, lv) =
      envLookupF (env, lv, (print "\nunbound var ===> "; ppVar lv;
                            print "\n"; raise envUnbound))
*)

(*******************************
 * utility functions for types *
 *******************************)

fun isInt (NT_INT | (NT_SINT _)) = true
  | isInt _ = false

fun norm t = t

val mkv = LV.mkLvar

fun rename (tv', tv, env) =
    let val v = envLookup (env, tv)
    in  envAdd (env, tv', v)
    end
 
fun tySubst (tn, tv, t') =
    if tn = tv then t' else
      case t' of
        NT_VAR t             => if t = tv then NT_VAR tn else t'  (* checkTy?? *)
      | (NT_INT | NT_SINT _) => t'
      | NT_ARRAY t           => NT_ARRAY (tySubst (tn, tv, t))
      | NT_REF t             => NT_REF (tySubst (tn, tv, t))
      | NT_FIX (x, i, ts)    =>
          if tn = x
          then let val x' = mkv()
               in NT_FIX (x', i, map (fn t => tySubst (tn, tv, tySubst (x', x, t))) ts)
               end
          else if tv = x then NT_FIX (tn, i, map (fn t => tySubst (tn,tv, t)) ts) 
          else NT_FIX (x, i, map (fn t => tySubst (tn, tv, t)) ts)
      | NT_TUPLE ts          => NT_TUPLE (map (fn t => tySubst(tn, tv, t)) ts)
      | NT_EXIST (x, t)      =>
          if tn = x
          then let val x' = mkv()
               in NT_EXIST (x', tySubst (tn, tv, tySubst (x', x, t)))
               end
          else if tv = x then NT_EXIST (tn, tySubst (tn,tv, t)) 
          else NT_EXIST (x, tySubst (tn, tv, t))
      | NT_CODE (tvs, ts1, ts2) =>
          if L.exists (fn x => x = tn) tvs
          then let val x' = mkv()
               in  NT_CODE (map (fn t => if t = tn then x' else t) tvs,
                            map (fn t => tySubst (tn, tv, tySubst (x', tn, t))) ts1,
                            map (fn t => tySubst (tn, tv, tySubst (x', tn, t))) ts2)
               end
          else if L.exists (fn x => x = tv) tvs then
                 NT_CODE (map (fn t => if t = tv then tn else t) tvs,
                          map (fn t => tySubst (tn, tv, t)) ts1,
                          map (fn t => tySubst (tn, tv, t)) ts2)
          else NT_CODE (tvs, map (fn t => tySubst (tn, tv, t)) ts1,
                             map (fn t => tySubst (tn, tv, t)) ts2)

fun tyEq env (t, t', isnorm) =
    let fun tyEq' (t, t') =
        case (t, t') of
          (NT_VAR tv, NT_VAR tv') => tv = tv' (* ??? *)
        | (NT_VAR tv, _) => tyEq' (norm (envLookup (env, tv)), t')
        | (_, NT_VAR tv) => tyEq' (t, norm (envLookup (env, tv)))
        | (NT_INT, NT_INT) => true
        | (NT_SINT i, NT_SINT i') => i = i'
        | (NT_ARRAY t1, NT_ARRAY t2) => tyEq' (t1, t2)
        | (NT_REF t1, NT_REF t2) => tyEq' (t1, t2)
        | (NT_FIX (tv, i, ts), NT_FIX (tv', i', ts')) =>
            if i = i' then
              let fun tySub x y = tySubst (mkv(), x, y)
              in  if tv = tv' then tyEqM env (ts, ts', true)
                  else tyEqM env ((map (tySub tv) ts), (map (tySub tv') ts'), true)
              end
            else false
        | (NT_TUPLE ts, NT_TUPLE ts') => tyEqM env (ts, ts', true)
        | (NT_EXIST (tv, t1), NT_EXIST (tv', t2)) =>
            let val tvn = mkv() 
            in  tyEq (*rename(tvn, tv, env)*)env (tySubst (tvn, tv, t1),
                  tySubst (tvn, tv', t2), true)
            end
        | (NT_CODE (tvs, ts1, ts2), NT_CODE (tvs', ts1', ts2')) =>
            if length tvs = length tvs' andalso length ts1 = length ts1' andalso length ts2 = length ts2' then
              let fun tySubM ((tv, tv'), (ts, ts')) =
                      let val tn = mkv()
                          fun subt t = tySubst (tn, tv, t)
                      in  (map (fn t => tySubst (tn, tv, t)) ts, map (fn t => tySubst (tn, tv', t)) ts')
                      end
                  val (ts1n, ts1n') = foldl tySubM (ts1, ts1') (LP.zip (tvs, tvs'))
                  val (ts2n, ts2n') = foldl tySubM (ts2, ts2') (LP.zip (tvs, tvs'))
              in  tyEqM env (ts1n, ts1n', false) andalso tyEqM env (ts2n, ts2n', false)
              end
            else false
        | (_, _) => false
    in  if isnorm then tyEq' (t, t')
        else tyEq' (norm t, norm t')
    end 

and tyEqM env (ts, ts', isnorm) =
    length ts = length ts' andalso
      L.all (fn (t, t') => tyEq env (t, t', isnorm)) (LP.zip (ts, ts'))

fun tyInst (nt, tv, t') =
    case t' of
      NT_VAR tv'           => if tv = tv' then nt else t' 
    | (NT_INT | NT_SINT _) => t'
    | NT_ARRAY t           => NT_ARRAY (tyInst (nt, tv, t))
    | NT_REF t             => NT_REF (tyInst (nt, tv, t))
    | NT_FIX (x, i, ts)    => if tv = x then t'
                              else NT_FIX (x, i, map (fn t => tyInst (nt, tv, t)) ts)
    | NT_TUPLE ts          => NT_TUPLE (map (fn t => tyInst(nt, tv, t)) ts)
    | NT_EXIST (x, t)      => if tv = x then t' else NT_EXIST (x, tyInst (nt, tv, t))
    | NT_CODE (tvs, ts1, ts2) =>
        if L.exists (fn x => x = tv) tvs then t'
        else NT_CODE (tvs, map (fn t => tyInst (nt, tv, t)) ts1,
                           map (fn t => tyInst (nt, tv, t)) ts2) 

and tyInstM (nts, tvs, t') =
    if length nts = length tvs
    then foldl (fn ((nt, tv), t) => tyInst (nt, tv, t)) t' (LP.zip (nts, tvs))
    else raise NFCHECK

(**************************
 * typechecking the types *
 **************************)

fun checkTy env t =  (* need to add checkTy in every case of checkExp? *)
    let fun check xs ty =
            case ty of
              NT_VAR tv => L.exists (fn x => x = tv) xs orelse
                           let val _ = envLookup (env, tv) in true end
            | (NT_INT | NT_SINT _) => true
            | NT_ARRAY t' => check xs t'
            | NT_REF t' => check xs t'
            | NT_FIX (x, i, ts) => L.all (check (x::xs)) ts
            | NT_TUPLE ts => L.all (check xs) ts
            | NT_EXIST (x, t') => check (x::xs) t'
            | NT_CODE (tvs, ts1, ts2) => L.all (check (tvs @ xs)) (ts1 @ ts2)
    in  print "checking type ("; ppNty t; print ")... ";
        if check [] t then (print "ok!\n"; norm t) else raise NFCHECK
    end

(*************************
 * typechecking the exps *
 *************************)

datatype checkphase = FLINT | CPS | CLOS 

fun ppp FLINT = "FLINT "
  | ppp CPS = "CPS "
  | ppp CLOS = "CLOS "

(* checkExp: checkphase -> tyenv -> tyenv -> lexp -> nty list *)
fun checkExp phase tenv venv exp =
    let val check = checkExp phase (* should we do norm in this level? *)
        val pbug = bug (ppp phase)
        fun valTy (VAR x) = norm (envLookup (venv, x))
          | valTy (LABEL l) = norm (envLookup (venv, l))
          | valTy (INT i) = NT_INT
          | valTy (SINT i) = NT_SINT i
        fun expTy ep =
            case ep of
              RET vs => (
                case phase of
                  (CPS | CLOS) => pbug "unexpected RET in this phase"
                | FLINT => map valTy vs)
            | LET (lvs, e, e') => (
                case phase of
                  (CPS | CLOS) => pbug "unexpected LET in this phase"
                | FLINT =>
                    let val t = check tenv venv e
                    in  if length lvs = length t
                        then check tenv (envAddM (venv, lvs, t)) e'
                        else pbug "number of arguments mismatch in LET" 
                    end)
            | FIX (fds, e) =>
                let fun addFnTy ((fk, lv, tvs, lvts : (lvar * nty) list, _), ve) =
                        case (phase, fk) of
                          (FLINT, ESCAPE (SOME ts)) =>
                            envAdd (ve, lv, NT_CODE (tvs, map #2 lvts, ts))
                        | (FLINT, _) => (* pbug "invalid function kind" *) 
                            (print "invalid function kind\n"; raise NFCHECK)
                        | (_, ESCAPE (SOME _)) =>
                            (* pbug "unexpected return values in this phase" *)
                            (print "unexpected return values in this phase\n"; raise NFCHECK)
                        | _ => envAdd (ve, lv, NT_CODE (tvs, map #2 lvts, []))
                    val venv' = foldl addFnTy venv fds
                in  app (checkFun phase tenv venv') fds;
                    check tenv venv' e
                end
            | APP (v, ts, vs) =>
                let val vt = valTy v
                in  print "-- checking app...\n";
                    case vt of
                      NT_CODE (tvs, ts', ts'') =>
                        if length ts = length tvs andalso length vs = length ts' then
                          let val tenv' = envAddM (tenv, tvs, ts)
                              val vts = map ((*(checkTy tenv') o*) valTy) vs
                              val nts = map (fn t => tyInstM (ts, tvs, t)) ts'
                          in  print "vts ===> "; ppNtySeq vts; print "\n";
                              print "ts  ===> "; ppNtySeq ts;  print "\n";
                              print "ts' ===> "; ppNtySeq ts'; print "\n";
                              print "nts ===> "; ppNtySeq nts; print "\n";
                              if tyEqM tenv' (vts, nts, false) then  
                                case phase of
                                  (CPS | CLOS) =>
                                    if length ts'' > 0
                                    then pbug "unexpected return values for APP"
                                    else (print "app ok!\n"; []) (* ?? *) 
                                | FLINT => map (fn t => tyInstM (ts, tvs, t)) ts'' (* ts'' *)
                              else (print "\n====> "; ppNtySeq vts; print "\n====> "; 
                                    ppNtySeq ts'; print "\n"; pbug "type mismatch for APP")
                          end
                        else pbug "number of paramaters mismatch in APP"
                    | _ => pbug "NT_CODE expected for APP"
                end
            | PTAPP (v, ts, lv, e) =>
                let val vt = valTy v
                    val l = length ts
                in  case vt of
                      NT_CODE (tvs, ts', ts'') =>
                        if l <= length tvs then
                          let val (tvs1, tvs2) = (L.take (tvs, l), L.drop (tvs, l))
                          in  check (envAddM (tenv, tvs1, ts))
                              (envAdd (venv, lv, NT_CODE (tvs2, ts', ts''))) e
                          end
                        else pbug "number of paramaters overflow in PTAPP"
                    | _ => pbug "NT_CODE expected for APP"
                end
            | PACK (t, v, t', lv, e) =>
                (case t' of
                   NT_EXIST (tv, t'') => (
                     print "-- checking pack ("; ppVar lv; print ") ...\n";
                     (*
                     print "t   ===> "; ppNty t; print "\n";
                     print "vt  ===> "; ppNty (valTy v); print "\n";
                     print "t'  ===> "; ppNty t'; print "\n";
                     print "t'' ===> "; ppNty t''; print "\n";
                     print "nt  ===> "; ppNty (tyInst (t, tv, t''));
                     print "\n";*)
                     if tyEq tenv (valTy v, tyInst (t, tv, t''), true)
                     then check tenv (envAdd (venv, lv, t')) e
                     else pbug "type mismatch in PACK")
                 | _ => pbug "NT_EXIST expected for PACK")
            | UNPACK (tv, lv, v, e) => (
                print "-- checking unpack ("; ppVar lv; print ")...\n";
                print "vt ===> "; ppNty (valTy v); print "\n";
                case valTy v of
                  NT_EXIST (tv', t) => check (envAdd (tenv, tv, NT_VAR tv(* ?? *)))
                                             (envAdd (venv, lv, t)) e
                | _ => pbug "NT_EXIST expected for UNPACK")
            | SWITCH (v, es, lves) =>
                let val vt = valTy v 
                in  case vt of
                      NT_FIX (tv, n, ts) =>
                        if n = length es andalso length ts = length lves then
                          let fun unboxedTy e = norm (check tenv venv e)
                              fun boxedTy ((lv, e), t) =
                                  norm (check tenv (envAdd (venv, lv, tyInst (vt, tv, t))) e)
                              fun checkBr [] = pbug "no switch branches for SWITCH"
                                | checkBr [t] = t
                                | checkBr (t::t'::ts) =
                                    if tyEqM tenv (t, t', true) then checkBr(t::ts)
                                    else pbug "type mismatch in SWITCH" 
                          in  checkBr ((map unboxedTy es) @ (map boxedTy (LP.zip (lves, ts)))) 
                          end 
                        else pbug "number of cases mismatch in SWITCH"
                    | _ => pbug "NT_FIX expected for SWITCH"
                end
            | CON (v, t, lv, e) => (
                case t of
                  NT_FIX (tv, n, ts) =>
                    let val vt = valTy v
                    in  case vt of
                          NT_SINT i =>
                            if i <= n then check tenv (envAdd (venv, lv, t)) e
                            else pbug "invalid unboxed value for CON"
                        | NT_TUPLE ((NT_SINT i)::ts) =>
                            if i <= length ts
                            then if tyEq tenv (tyInst (t, tv, L.nth (ts, i)), vt, false)
                                 then check tenv (envAdd (venv, lv, t)) e
                                 else pbug "the type of boxed value mismatch for CON"
                            else pbug "invalid boxed value for CON"
                        | NT_TUPLE _ => (
                            case ts of
                              [tt] => ((*print "\nvt ===> "; ppNty vt; print "\n";
                                       print "\ntt ===> "; ppNty tt; print "\n";
                                       print "\nt  ===> "; ppNty t; print "\n";
                                       print "\nnt ===> "; ppNty (tyInst (t, tv, tt));
                                       print "\n";*)
                                      if tyEq tenv (tyInst (t, tv, tt), vt, false)
                                      then check tenv (envAdd (venv, lv, t)) e
                                      else pbug "the type of boxed value mismatch for CON (2)")
                            | _ => pbug "invalid boxed value for CON (2)")
                        | _ => pbug "NT_SINT/NT_TUPLE expected for CON"
                    end
                | _ => pbug "NT_FIX expected for CON")
            | RECORD (vps, lv, e) =>
                let fun apTy (t, ap) = (
                        case ap of
                          OFFp i =>
                            if i = 0 then t
                            else (print "non-zero offset is not supported now :-(\n";
                                  raise NFCHECK)
                        | SELp (i, ap') => (
                            case t of
                              NT_TUPLE ts => apTy (L.nth (ts, i), ap')
                            | _ => (print "NT_TUPLE expected for RECORD\n"; raise NFCHECK)))
(*
                        case t of
                          NT_TUPLE ts => (
                            case ap of
                              OFFp i => (
                                L.nth (ts, i)
                                * handle _ => pbug "unexisted field for RECORD"* )
                            | SELp (i, ap') => (
                                apTy (L.nth (ts, i), ap')
                                *handle _ => pbug "unexisted field for RECORD"* ))
                        | _ => *pbug "NT_TUPLE expected for RECORD"*
                          (print "NT_TUPLE expected for RECORD\n"; raise NFCHECK))
*)
                    fun vpTy (v, ap) = apTy (valTy v, ap)
                in  print "-- checking record ("; ppVar lv; print ")...\n";
                    check tenv (envAdd (venv, lv, NT_TUPLE (map vpTy vps))) e
                end
            | SELECT (v, i, lv, e) => (print "-- checking select...\n";
                case valTy v of
                  NT_TUPLE ts => (
                    check tenv (envAdd (venv, lv, L.nth (ts, i))) e(*
                    handle _ => pbug ("unexisted field (#" ^ (Int.toString i)
                                      ^ ") for SELECT")*))
                | _ => pbug "NT_TUPLE expected for SELECT")
            | BRANCH (c, vs, e, e') => (
                print "-- checking branch...\n"; 
                if length vs = 2 (* currently we only support binary cmpop *)
                then if isInt (valTy (hd vs)) andalso isInt (valTy (hd (tl vs)))
                     then let val t = norm (expTy e)
                              val t' = norm (expTy e')
                          in  if tyEqM tenv (t, t', true) then t
                              else pbug "branch types mismatch"
                          end
                     else pbug "INT expected for binary comparison"
                else pbug (Int.toString (length vs) ^
                     "instead of 2 arguemnts are provided to the binary cmpop"))
            | PRIMOP (a, vs, lv, e) => (
                case a of
                  (MUL | DIV | MOD | SUB | ADD) =>
                    if length vs = 2
                    then if isInt (valTy (hd vs)) andalso isInt (valTy (hd (tl vs)))
                         then check tenv (envAdd (venv, lv, NT_INT)) e
                         else pbug "NT_INT/NT_SINT expected for arithop" 
                    else pbug (Int.toString (length vs) ^
                               "instead of 2 arguemnts are provided to the binary arithop")
                | MKARRAY =>
                    if length vs = 2 then
                      if isInt (valTy (hd vs))
                      then check tenv (envAdd (venv, lv, NT_ARRAY (valTy (hd (tl vs))))) e
                      else pbug "NT_INT expected for the size of MKARRAY"
                    else pbug (Int.toString (length vs) ^
                               "instead of 2 arguemnts are provided to MKARRAY")
                | SUBSCRIPT =>
                    if length vs = 2 then
                      if isInt (valTy (hd (tl vs))) then
                        case valTy (hd vs) of
                          NT_ARRAY t => check tenv (envAdd (venv, lv,  t)) e
                        | _ => pbug "NT_ARRAY expected for the array of SUBSCRIPT"
                      else pbug "NT_INT expected for the index of SUBSCRIPT"
                    else pbug (Int.toString (length vs) ^
                               "instead of 2 arguemnts are provided to SUBSCRIPT")
                | UPDATE =>
                    if length vs = 3 then
                      if isInt (valTy (hd (tl vs))) then
                        case valTy (hd vs) of
                          NT_ARRAY t =>
                            if valTy (hd (tl (tl vs))) = t
                            then check tenv venv e
                            else pbug "type mismatch in UPDATE"
                        | _ => pbug "NT_ARRAY expected for the array of UPDATE"
                      else pbug "NT_INT expected for the index of UPDATE"
                    else pbug (Int.toString (length vs) ^
                               "instead of 3 arguemnts are provided to UPDATE")
                | REF =>
                    if length vs = 1
                    then check tenv (envAdd (venv, lv, NT_REF (valTy (hd vs)))) e
                    else pbug (Int.toString (length vs) ^
                               "instead of 1 arguemnts are provided to REF")
                | DEREF =>
                    if length vs = 1 then
                      case valTy (hd vs) of
                        NT_REF t => check tenv (envAdd (venv, lv, t)) e
                      | _ => pbug "NT_REF expected for DEREF"
                    else pbug (Int.toString (length vs) ^
                               "instead of 1 arguemnts are provided to DEREF")
                | ASSIGN =>
                    if length vs = 2 then
                      case valTy (hd vs) of
                        NT_REF t =>
                          if valTy (hd (tl vs)) = t
                          then check tenv venv e
                          else pbug "type mismatch in ASSIGN"
                      | _ => pbug "NT_REF expected for ASSIGN"
                    else pbug (Int.toString (length vs) ^
                               "instead of 2 arguemnts are provided to ASSIGN")) 
    in  expTy exp
    end

and checkFun phase tenv venv (fk, lv, tvs, lvts, e) =
    let val pbug = bug (ppp phase)
        val ts = checkExp phase tenv (envAddM (venv, map #1 lvts, map #2 lvts)) e
    in  case (phase, fk) of
          (FLINT, ESCAPE (SOME ts')) =>
            if ts = ts' then () else pbug "return type mismatch for FIX"
        | _ => ()
    end

fun chk ph (p as (fk, lv, tvs, lvts : (lvar * nty) list, e)) =
    let val initVenv' =
        case (ph, fk) of
          (FLINT, ESCAPE (SOME ts)) =>
            envAdd (initVenv, lv, NT_CODE (tvs, map #2 lvts, ts))
        | (FLINT, _) => (* pbug "invalid function kind" *) 
            (print "invalid function kind\n"; raise NFCHECK)
        | (_, ESCAPE (SOME _)) =>
            (* pbug "unexpected return values in this phase" *)
            (print "unexpected return values in this phase\n"; raise NFCHECK)
        | _ => envAdd (initVenv, lv, NT_CODE (tvs, map #2 lvts, []))
    in  print "\nstart type checking...\n";
        checkFun ph initTenv initVenv' p;
        print "\ntypechecking done!\n" 
    end

fun chkpgf (codes : fundec list) =   (* post global-fix checking *)
    let val initVenv' = foldl
                        (fn (c as (_, lv, tvs, lvts : (lvar * nty) list, _) , e)
                         => envAdd (e, lv, NT_CODE (tvs, map #2 lvts, []))) initVenv codes
    in  print "start type checking...\n";
        app (fn c => (checkFun CLOS initTenv initVenv' c; print "got one!!!!!!!\n\n")) codes;
        print "typechecking done!\n"
    end

end (* toplevel local *)
end (* structure typeChecker *)
