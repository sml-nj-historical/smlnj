(* toclos.sml *)

structure CPS2Clos = struct

local
    open NFlint
    open NFlintChecker
    open ppNFlint

    structure U  = NFlintUtil

    structure S  = IntRedBlackSet
    structure T  = IntHashTable
    structure L  = List
    structure LP = ListPair

    exception CLOSTRANS
    fun kbug p s = (print ("\n" ^ p ^ " translation error : " ^ s ^ "!\n"); raise CLOSTRANS)
    fun kbug1 s = kbug "CPS2CLOS" s
in

(********************************************************
 *                                                      *
 * functions for KNOWN/ESCAPE analysis                  *
 *                                                      *
 * NOTE:                                                *
 *       1. we drop the info for CONT                   *
 *       2. the mutable PRIMOPs may need some changes?? *
 *                                                      *
 ********************************************************)

val escapes = ref S.empty
fun escapesP f = S.member (!escapes, f)
fun escapesM (VAR v) = escapes := S.add (!escapes, v)
  | escapesM _ = ()

fun fixkind (fk, lv, tvs, lvts, e) = 
    if escapesP lv then (ESCAPE NONE, lv, tvs, lvts, e) else (KNOWN, lv, tvs, lvts, e)

fun procfix (fk, lv, tvs, lvts : (lvar * nty) list, e) =
    (app (escapesM o VAR o #1) lvts; (fk, lv, tvs, lvts, proc e))
and proc exp =
    case exp of
      RET _ => kbug "CPS2CLOS" "unexpected RET in CPS"
    | LET _ => kbug "CPS2CLOS" "unexpected LET in CPS"
    | FIX (fds, e) =>
        let val e' = proc e
            (* val fds' = map fixkind (map procfix fds) *)   (* NOTE: we need to finish all procfixs before fixkind *)
            (* CURRENTLY, we treat all funs as ESCAPE ones whenever there is at least one escapes to make it eaiser *)
            (* so the result of the mutually-recursive funs are all ESCAPEs or all KNOWNs *)
            val fds' = map procfix fds
            val allknown = foldl (fn (f, b) => b andalso not (escapesP f)) true (map #2 fds')
            val fds'' = if allknown then map (fn (fk, lv, tvs, lvts, e) => (KNOWN, lv, tvs, lvts, e)) fds'
                        else (app (fn f => if escapesP f then () else escapesM (VAR f)) (map #2 fds');
                              map (fn (fk, lv, tvs, lvts, e) => (ESCAPE NONE, lv, tvs, lvts, e)) fds')
        in  FIX (fds'', e')
        end
    | APP (v, ts, vs)       => (app escapesM vs; exp)
    | PTAPP _               => kbug "CPS2CLOS" "PTAPP is not supported for CPS"
    | PACK _                => kbug "CPS2CLOS" "PACK is not supported for CPS"
    | UNPACK _              => kbug "CPS2CLOS" "UNPACK is not supported for CPS"
    | SWITCH (v, es, lves)  => SWITCH (v, map proc es, map (fn (lv, e) => (lv, proc e)) lves)
    | CON (v, t, lv, e)     => ((* escapesM ? *) v; CON (v, t, lv, proc e))
    | RECORD (vps, lv, e)   => (app (escapesM o #1) vps; RECORD (vps, lv, proc e))
    | SELECT (v, i, lv, e)  => SELECT (v, i, lv, proc e)
    | BRANCH (c, vs, e, e') => (app escapesM vs; BRANCH (c, vs, proc e, proc e'))
    | PRIMOP (a, vs, lv, e) => (app escapesM vs; PRIMOP (a, vs, lv, proc e))

fun procprog p = (escapes := S.empty; procfix p)

fun ppEscFuns _ =
    let val efs = S.listItems (!escapes)
    in  print "\nEscape Functions :\n\n\t"; app (fn lv => (ppVar lv; print "  ")) efs; print "\n\n"
    end 

(**************************************************************
 * calculating free variables with type & free type variables *
 **************************************************************)

fun inSet (x, l) = L.exists (fn y => y = x) l

fun mergeSet (l, [])      = l
  | mergeSet (l, (x::xs)) = mergeSet ((if inSet (x, l) then l else x::l), xs)

fun mergeSP ((l1, l1'), (l2, l2')) = (mergeSet (l1, l2), mergeSet (l1', l2'))

fun minusSet (l, l') = L.filter (fn x => not (inSet (x, l'))) l

fun minusSP ((l1, l1'), (l2, l2')) = (minusSet (l1, l2), minusSet (l1', l2'))

fun uniq [] = []
  | uniq (x::xs) = let val xs' = uniq xs in if inSet (x, xs') then xs' else x::xs' end 

fun flvVars []             = []
  | flvVars ((VAR lv)::vs) = lv :: (flvVars vs)
  | flvVars (_::vs)        = flvVars vs

val flvVals = uniq o flvVars

fun ftvNty' (t, l) =
    case t of
      NT_VAR tv              => mergeSet ([tv], l)
    | NT_INT                 => l
    | NT_SINT i              => l
    | NT_ARRAY t'            => ftvNty' (t', l)
    | NT_REF t'              => ftvNty' (t', l)
    | NT_FIX (tv, i, ts)     => minusSet (ftvNtyM' (ts, l), [tv])
    | NT_TUPLE ts            => ftvNtyM' (ts, l)
    | NT_EXIST (tv, t')      => minusSet (ftvNty' (t', l), [tv])
    | NT_CODE (tvs, ts, ts') => minusSet (ftvNtyM' (mergeSet (ts, ts'), l), tvs)

and ftvNtyM' (ts, l) = foldl (fn (t, l') => ftvNty' (t, l')) l ts 

fun ftvNty t  = ftvNty' (t, [])
fun ftvNtyM t = ftvNtyM' (t, [])

exception Freevar
val fvT : (lvar list * tvar list) T.hash_table = T.mkTable (32, Freevar)

val addFv = T.insert fvT

fun getFv f = T.lookup fvT f handle Freevar => (print "could not find fv for function "; ppVar f; print "\n"; ([], []))

fun fvExp exp =
    case exp of
      RET _ => kbug "CPS2CLOS" "unexpected RET in CPS"
    | LET _ => kbug "CPS2CLOS" "unexpected LET in CPS"
    | FIX (fds, e) =>   (* we should add the analysis for mutually-recursive functions *)
                        (* currently we put all the fvs together but the (mutually-recursive) fun names *)
        let val (fv, f) = foldl (fn (fd, (fv', f')) => (mergeSP (fvFun fd, fv'), (#2 fd) :: f')) (([], []), []) fds
        in  mergeSP (fv, minusSP (fvExp e, (f, [])))
        end
    | APP (v, ts, vs)       => mergeSP ((flvVals [v], []), (flvVals vs, ftvNtyM ts))
    | PTAPP _               => kbug "CPS2CLOS" "PTAPP is not supported for CPS"
    | PACK _                => kbug "CPS2CLOS" "PACK is not supported for CPS"
    | UNPACK _              => kbug "CPS2CLOS" "UNPACK is not supported for CPS"
    | SWITCH (v, es, lves)  =>
        let val fv  = foldr (fn (e, fvs) => mergeSP (fvExp e, fvs)) ([], []) es
            val fv' = foldr (fn ((lv, e), fvs) => mergeSP (minusSP (fvExp e, ([lv], [])), fvs)) ([], []) lves
        in  mergeSP ((flvVals [v], []), mergeSP (fv, fv'))
        end
    | CON (v, t, lv, e)     => mergeSP ((flvVals [v], ftvNty t), minusSP (fvExp e, ([lv], [])))
    | RECORD (vps, lv, e)   => mergeSP ((flvVals (map #1 vps), []), minusSP (fvExp e, ([lv], [])))
    | SELECT (v, i, lv, e)  => mergeSP ((flvVals [v], []), minusSP (fvExp e, ([lv], [])))
    | BRANCH (c, vs, e, e') => mergeSP ((flvVals vs, []), mergeSP (fvExp e, fvExp e'))
    | PRIMOP (a, vs, lv, e) => mergeSP ((flvVals vs, []), minusSP (fvExp e, ([lv], [])))

and fvFun (fk, lv, tvs, lvts : (lvar * nty) list, e) = (* mutually-recursive functions!! *)
    let val fvs = minusSP (mergeSP (fvExp e, ([], ftvNtyM (map #2 lvts))), (lv :: (map #1 lvts), tvs))
    in  addFv (lv, fvs);
        fvs
    end

fun fvProg p = (T.clear fvT; fvFun p)

fun ppFvTable _ =
    let val fvs = T.listItemsi fvT
    in  print "\n>---------- ***  free vars table  *** ----------<\n";
        print ">                                               <\n";
        app (fn (lv, (flvs, ftvs)) => (print ">\t"; ppVar lv; print " : { "; ppVarSeq flvs;
             print " };\t{ "; ppVarSeq ftvs; print " }\t<\n")) fvs;
        print ">                                               <\n";
        print ">-----------------------------------------------<\n"
    end

(********************************
 * closure conversion functions *
 ********************************)

fun transTy t =
    case t of
      NT_VAR tv            => t
    | (NT_INT | NT_SINT _) => t
    | NT_ARRAY t'          => NT_ARRAY (transTy t')
    | NT_REF t'            => NT_REF (transTy t')
    | NT_FIX (tv, i, ts)   => NT_FIX (tv, i, map transTy ts)
    | NT_TUPLE ts          => NT_TUPLE (map transTy ts)
    | NT_EXIST (tv, t')    => kbug "CPS2CLOS" "NT_EXIST is not supported for CPS phase"
    | NT_CODE (tvs, ts1, ts2) =>
        if null ts2 then
          let val tv = mkv()
          in  NT_EXIST (tv, NT_TUPLE [NT_CODE (tvs, ((NT_VAR tv)::(map transTy ts1)), []), NT_VAR tv])
          end
        else kbug "CPS2CLOS" "unexpected return type for CPS function"

type clos = lvar

fun addIndex from l =
    let fun fromto f n sl = if n > 0 then (n + f) :: (fromto f (n - 1) sl) else [f]
    in  LP.zip (l, rev (fromto from (length l - 1) []))
    end

fun mergeFv (flvs, ftvs) = flvs @ ftvs

exception Closure
val clT : clos T.hash_table = T.mkTable (32, Closure)

val addCl = T.insert clT

fun getCl f = T.lookup clT f handle Closure => (print "could not find the closure for function "; ppVar f; f)

fun getNewName f fs =
    case L.find (fn (x, _) => x = f) fs of SOME (_, f') => f' | NONE => kbug "CPS2CLOS" "nonexisted (new) function"

fun transVal v =
    case v of
      VAR lv           => v (* ?? *)
    | LABEL lv         => kbug "CPS2CLOS" "unexpected LABEL for CPS"
    | (INT _ | SINT _) => v  

fun transVp (v, ap) = (transVal v, ap)

fun transExp exp =
    case exp of
      RET _ => kbug "CPS2CLOS" "unexpected RET in CLOS"
    | LET _ => kbug "CPS2CLOS" "unexpected LET in CLOS"
    | FIX (fds, e) =>
        if escapesP ((#2 o hd) fds) then
          let val fs = map #2 fds
              val fs' = map (fn _ => mkv()) fs
              val fs'' = LP.zip (fs, fs')
              val fvs = foldl (fn (f, l) => mergeSP(getFv f, l)) ([], []) fs
              val fds' = map (transEscFun fs'' fvs) fds
              val (flvs, ftvs) = fvs
              val flvts = NT_TUPLE (map (transTy o U.getTy) flvs)
              val (fr, fenv, f'') = (mkv(), mkv(), mkv())
              val e' = foldl
                         (fn ((f, f'), se) =>
                           if null ftvs then
                             RECORD (map (fn x => (VAR x, OFFp 0)) flvs, fenv,
                             RECORD ([(VAR f', OFFp 0), (VAR fenv, OFFp 0)], fr,
                             PACK (flvts, VAR fr, transTy (U.getTy f), f, se)))
                           else
                             RECORD (map (fn x => (VAR x, OFFp 0)) flvs, fenv,
                             PTAPP (VAR f', map NT_VAR ftvs, f'',
                             RECORD ([(VAR f'', OFFp 0), (VAR fenv, OFFp 0)], fr,
                             PACK (flvts, VAR fr, transTy (U.getTy f), f, se)))))
                         (transExp e) fs''
          in  FIX (fds', e')
          end
        else (* known functions *)
          FIX (map transKnownFun fds, transExp e)
    | APP (v, ts, vs) =>
      (case v of
         VAR f =>
           if escapesP f then
             let val (tv, f', fcode, fenv) = (mkv(), mkv(), mkv(), mkv())
             in  UNPACK (tv, f', VAR f,
                   SELECT (VAR f', 0, fcode,
                     SELECT (VAR f', 1, fenv,
                       APP (VAR fcode, map transTy ts, (VAR fenv) :: (map transVal vs)))))
             end
           else (* known function *)
             let val fvs = getFv f
             in  APP (v, ts @ (map NT_VAR (#2 fvs)), (map transVal vs) @ (map VAR (#1 fvs)))
             end
       | _ => kbug "CPS2CLOS" "unexpected function name for APP")
    | PTAPP _               => kbug "CPS2CLOS" "PTAPP is not supported during this phase"
    | PACK _                => kbug "CPS2CLOS" "PACK is not supported during this phase"
    | UNPACK _              => kbug "CPS2CLOS" "UNPACK is not supported during this phase"      
    | SWITCH (v, es, lves)  => SWITCH (transVal v, map transExp es, map (fn (lv, e) => (lv, transExp e)) lves)
    | CON (v, t, lv, e)     => CON (transVal v, transTy t, lv, transExp e)
    | RECORD (vps, lv, e)   => RECORD (map transVp vps, lv, transExp e)
    | SELECT (v, i, lv, e)  => SELECT (transVal v, i, lv, transExp e)
    | BRANCH (c, vs, e, e') => BRANCH (c, map transVal vs, transExp e, transExp e')
    | PRIMOP (a, vs, lv, e) => PRIMOP (a, map transVal vs, lv, transExp e)

and transEscFun fs fvs (fk, lv, tvs, lvts, e) =
                          (* under current impl, it indicates that all funs are ESCAPE ones in the same FIX *)
    let val (flvs, ftvs) = fvs
        val flvts = NT_TUPLE (map (transTy o U.getTy) flvs)
        val (gr, env, g'') = (mkv(), mkv(), mkv())
        val e' = foldl
                   (fn ((g, g'), se) =>
                        if null ftvs then
                          RECORD ([(VAR g', OFFp 0), (VAR env, OFFp 0)], gr,
                          PACK (flvts, VAR gr, transTy (U.getTy g), g, se))
                        else
                          PTAPP (VAR g', map NT_VAR ftvs, g'',
                          RECORD ([(VAR g'', OFFp 0), (VAR env, OFFp 0)], gr,
                          PACK (flvts, VAR gr, transTy (U.getTy g), g, se))))
                   (foldl (fn ((fv, i), se) => SELECT (VAR env, i, fv, se)) (transExp e) (addIndex 0 flvs))
                   fs
    in  (fk, getNewName lv fs, ftvs @ tvs, (env, flvts) :: (map (fn (lv, t) => (lv, transTy t)) lvts), e')
    end

and transKnownFun (fk, lv, tvs, lvts, e) = 
                                 (* under the current KNOWN/ESCAPE analysis, it can not call an ESCAPE fun *)
    let val (flvs, ftvs) = getFv lv
    in  (fk, lv, tvs @ ftvs,
         (map (fn (lv, t) => (lv, transTy t)) lvts) @ (map (fn lv' => (lv', (transTy o U.getTy) lv')) flvs),
         transExp e)
    end

fun transProg p =
    let val lv = #2 p
    in  transEscFun [(lv, mkv())] (getFv lv) p
    end


(****************************************
 * unrebind function (alpha conversion) *
 ****************************************)

val labels = ref S.empty
fun labelsP lv = S.member (!labels, lv)
fun labelsM lv = labels := (print "\tadd label "; ppVar lv; print "\n"; S.add (!labels, lv))

fun unrebind (fk, lv, tvs, lvts, e) =
    let fun rename rebind (VAR lv) =
            let fun r [] = VAR lv
                  | r ((lv', v)::bs) = if lv = lv' then v else r bs
            in  if labelsP lv then LABEL lv else r rebind
            end
          | rename _ v = v

        fun f (fk, lv, tvs, lvts, e) =
            let fun f' (lv, (a, rb)) =
                    let val lv' = mkv()
                    in  (lv'::a, (lv, VAR lv')::rb)
                    end
                val (args, rebind) = foldr f' ([], []) (map #1 lvts)
            in  (fk, lv, tvs, LP.zip (args, map #2 lvts), g rebind e)
            end

        and g (rebind : (lvar * value) list) =
            let val rename' = rename rebind
                fun h e =
                    case e of
                      RET _ => kbug "CPS2CLOS" "unexpected RET in CLOS"
                    | LET _ => kbug "CPS2CLOS" "unexpected LET in CLOS"
                    | FIX (fds, e)           => ((map (labelsM o #2) fds); FIX (map f fds, h e))
                    | APP (v, ts, vs)        => APP (rename' v, ts, map rename' vs)
                    | PTAPP (v, ts, lv, e)   => PTAPP (rename' v, ts, lv, h e)
                    | PACK (t, v, t', lv, e) => PACK (t, rename' v, t', lv, h e) (* ?? *)
                    | UNPACK (tv, lv, v, e)  => UNPACK (tv, lv, rename' v, h e)  (* ?? *)
                    | SWITCH (v, es, lves)   => SWITCH (rename' v, map h es, map (fn (lv, e) => (lv, h e)) lves)
                    | CON (v, t, lv, e)      => CON (rename' v, t, lv, h e)
                    | RECORD (vps, lv, e)    =>
                        let val lv' = mkv()
                        in  RECORD (map (fn (v, ap) => (rename' v, ap)) vps, lv', g ((lv, VAR lv')::rebind) e)
                        end
                    | SELECT (v, i, lv, e)   =>
                        let val lv' = mkv()
                        in  SELECT (rename' v, i, lv', g ((lv, VAR lv')::rebind) e)
                        end
                    | BRANCH (c, vs, e, e')  => BRANCH (c, map rename' vs, h e, h e')
                    | PRIMOP (a, vs, lv, e)  => PRIMOP (a, map rename' vs, lv, h e)
        in  h
        end
    in  labels := S.empty;
        labelsM lv;
        (fk, lv, tvs, lvts, g [] e)
    end

(***********************
 * global-fix function *
 ***********************)

fun globalfix (fk, lv, tvs, lvts, e) =
    let fun gfix exp =
            case exp of
              RET _ => kbug "CPS2CLOS" "unexpected RET in CLOS"
            | LET _ => kbug "CPS2CLOS" "unexpected LET in CLOS"
            | FIX (fds, e)           =>
                let val (fl, e') = gfix e
                    val f' = foldl (fn ((fk, lv, tvs, lvts, e), f) =>
                                    let val (g, e') = gfix e in (fk, lv, tvs, lvts, e')::g@f end) fl fds
                in  (f', e')
                end 
            | APP (v, ts, vs)        => ([], exp)
            | PTAPP (v, ts, lv, e)   => let val (f, ge) = gfix e in (f, PTAPP (v, ts, lv, ge)) end
            | PACK (t, v, t', lv, e) => let val (f, ge) = gfix e in (f, PACK (t, v, t', lv, ge)) end (* ?? *)
            | UNPACK (tv, lv, v, e)  => let val (f, ge) = gfix e in (f, UNPACK (tv, lv, v, ge)) end (* ?? *)
            | SWITCH (v, es, lves)   =>
                let val (f, ges) =
                      foldr (fn (e, (fl, gl)) => let val (f, ge) = gfix e in (f@fl, ge::gl) end) ([], []) es
                    val (f', glves) =
                      foldr (fn ((lv, e), (fl, gl)) => let val (f, ge) = gfix e in (f@fl, (lv, ge)::gl) end)
                            ([], []) lves
                in  (f@f', SWITCH (v, ges, glves))
                end
            | CON (v, t, lv, e)      => let val (f, ge) = gfix e in (f, CON (v, t, lv, ge)) end
            | RECORD (vps, lv, e)    => let val (f, ge) = gfix e in (f, RECORD (vps, lv, ge)) end
            | SELECT (v, i, lv, e)   => let val (f, ge) = gfix e in (f, SELECT (v, i, lv, ge)) end
            | BRANCH (c, vs, e, e')  =>
                let val (f,  ge)  = gfix e
                    val (f', ge') = gfix e'
                in (f@f', BRANCH (c, vs, ge, ge'))
                end
            | PRIMOP (a, vs, lv, e)  => let val (f, ge) = gfix e in (f, PRIMOP (a, vs, lv, ge)) end
        val (l, body) = gfix e
    in  (fk, lv, tvs, lvts, body) :: l
    end

end (* toplevel local *)
end (* structure CPS2Clos *)
