(* utils.sml *)

structure NFlintUtil = struct

local
    open NFlint
    open ppNFlint

    structure L  = List
    structure LP = ListPair
    structure T  = IntHashTable
    structure LV = LambdaVar

    exception UTIL
    fun ubug p s = (print ("\n" ^ p ^ " : " ^ s ^ "!\n"); raise UTIL)
in

(******************************************************************************
 *                                                                            *
 * utility functions for recover type information of a closed NFLINT program  *
 *                                                                            *
 * NOTE:                                                                      *
 *       1. We need to keep the type info of each lvar/tvar, since the query  *
 *          may come from the middle of a closed expression; and we do not to *
 *          calculate on the fly, instead, we did record all info in advance. *
 *       2. On the other hand, we do not double-check the type correctness    *
 *          here, which is the work of the type checker.                      *
 *                                                                            *
 ******************************************************************************)

val mkv = LV.mkLvar

exception RecoverNty
val tyT : nty T.hash_table = T.mkTable (32, RecoverNty)  (* we keep tv and lv in the same table *)

fun cleanup _ = T.clear tyT

fun getTy lv = T.lookup tyT lv handle RecoverNty => (print "could not find the var "; ppVar lv; NT_VAR lv)

val addTy = T.insert tyT

val addTyM = app addTy

fun ppNtyTable _ =
    let val lvts = T.listItemsi tyT
    in  print "\n>---------- ***  type info table  *** ----------<\n";
        print ">                                               <\n";
        app (fn (lv, t) => (print ">\t"; ppVar lv; print " : "; ppNty t; print "\t<\n")) lvts;
        print ">                                               <\n";
        print ">-----------------------------------------------<\n"
    end

fun getTyNty t =
    let fun getTyNty' r t =
            case t of
              NT_VAR tv              => if L.exists (fn x => x = tv) r then t else getTy tv
            | (NT_INT | NT_SINT _)   => t
            | NT_ARRAY t'            => NT_ARRAY (getTyNty' r t')
            | NT_REF t'              => NT_REF (getTyNty' r t')
            | NT_FIX (tv, i, ts)     => NT_FIX (tv, i, map (getTyNty' ((*tv ::*) r)) ts)
            | NT_TUPLE ts            => NT_TUPLE (map (getTyNty' r) ts)
            | NT_EXIST (tv, t')      => NT_EXIST (tv, getTyNty' ((*tv ::*) r) t')
            | NT_CODE (tvs, ts, ts') => NT_CODE (tvs, map (getTyNty' ((*tvs @ *)r)) ts, map (getTyNty' ((*tvs @ *) r)) ts')
    in  getTyNty' [] t
    end

fun getTyVal (VAR lv | LABEL lv) = getTy lv
  | getTyVal (INT _) = NT_INT
  | getTyVal (SINT i) = NT_SINT i

fun getTyVp (v, ap) =
    let fun getTyAp (t, OFFp i) = if i = 0 then getTyNty t else ubug "getTy" "unexpected nonzero offset for record"
          | getTyAp (t, SELp (i, ap')) =
              (case t of NT_TUPLE ts => getTyAp (L.nth (ts, i), ap')
                       | _ => ubug "getTy" "NT_TUPLE expected for SELp")
    in  getTyAp (getTyVal v, ap)
    end

fun getTyExp exp = 
    case exp of
      RET vs                 => map getTyVal vs 
    | LET (lvs, e, e')       => (addTyM (LP.zip (lvs, getTyExp e)); getTyExp e')
    | FIX (fds, e)           => (app getTyFun fds; getTyExp e)
    | APP (v, ts, vs)        => 
        (case getTyVal v of
           NT_CODE (tvs, ts1, ts2) => (addTyM (LP.zip (tvs, ts)); map getTyNty ts2)
         | _ => ubug "getTy" "NT_CODE expected for the type of function")
    | PTAPP (v, ts, lv, e)   => 
        (case getTyVal v of
           NT_CODE (tvs, ts1, ts2) =>
             let val l = length ts
                 val (tvs1, tvs2) = (L.take (tvs, l), L.drop (tvs, l))
             in  addTyM (LP.zip (tvs1, ts));
                 addTy (lv, getTyNty (NT_CODE (tvs2, ts1, ts2)));
                 getTyExp e
             end
         | _ => ubug "getTy" "NT_CODE expected for the type of function")
    | PACK (t, v, t', lv, e) =>
        let val tv = mkv()
        in  addTy (tv, getTyNty t); addTy (lv, NT_EXIST (tv, getTyNty t')); getTyExp e
        end
    | UNPACK (tv, lv, v, e)  =>
        (case getTyVal v of
           NT_EXIST (tv', t) => (addTy (tv, getTy tv'); addTy (lv, getTyNty t); getTyExp e) 
         | _ => ubug "getTy" "NT_EXIST expected for the type of pack")
    | SWITCH (v, es, lves)   =>
        (case getTyVal v of
           NT_FIX (tv, i, ts) =>
             let val ts1 = map getTyExp es
                 fun f ((lv, e), t) = (addTy (lv, getTyNty t); getTyExp e)
                 val ts2 = map f (LP.zip (lves, ts))
             in  if not (L.null ts1) then hd ts1 else hd ts2
             end
         | _ => ubug "getTy" "NT_FIX expected for the type of con")
    | CON (v, t, lv, e)      =>
        (case (getTyVal v, t) of
           ((NT_SINT _ | NT_TUPLE _), NT_FIX (tv, i, ts))
             => (addTy (tv, t); addTy (lv, getTyNty t); getTyExp e)
         | _ => ubug "getTy" "NT_SINT/NT_TUPLE + NT_FIX expected for the type of con")
    | RECORD (vps, lv, e)    => (addTy (lv, NT_TUPLE (map getTyVp vps)); getTyExp e)
    | SELECT (v, i, lv, e)   =>
        (case getTyVal v of
           NT_TUPLE ts => (addTy (lv, L.nth (ts, i)); getTyExp e)
         | _ => ubug "getTy" "NT_TUPLE expected for the type of record")
    | BRANCH (c, vs, e, e')  => (getTyExp e; getTyExp e')
    | PRIMOP (a, vs, lv, e)  =>
        (case a of
           (MUL | DIV | MOD | SUB | ADD) => (addTy (lv, NT_INT); getTyExp e)
         | MKARRAY   => (addTy (lv, NT_ARRAY (getTyVal (hd (tl vs)))); getTyExp e)
         | SUBSCRIPT =>
             (case getTyVal (hd vs) of
                NT_ARRAY t => (addTy (lv, getTyNty t); getTyExp e)
              | _ => ubug "getTy" "NT_ARRAY expected for the type of array")
         | UPDATE    => getTyExp e
         | REF       => (addTy (lv, NT_REF (getTyVal (hd vs))); getTyExp e)
         | DEREF     =>
             (case getTyVal (hd vs) of
                NT_REF t => (addTy (lv, getTyNty t); getTyExp e)
              | _ => ubug "getTy" "NT_REF expected for the type of ref")
         | ASSIGN    => getTyExp e)

and getTyFun (fk, lv, tvs, lvts, e) =
    let val rts = case fk of ESCAPE (SOME ts) => ts | _ => []
    in  addTyM lvts; addTy (lv, NT_CODE (tvs, map (getTyNty o #2) lvts, rts)); getTyExp e
    end

fun getTyProg p = (cleanup (); getTyFun p)

end (* toplevel local *)
end (* structure NFlintUtil *)
