(* tocps.sml *)

structure Flint2CPS = struct

local
    open NFlint
    
    structure U  = NFlintUtil

    structure LP = ListPair
    structure T  = IntHashTable
    structure LV = LambdaVar

    exception CPSTRANS
    fun fbug p s = (print ("\n" ^ p ^ " translation error : " ^ s ^ "!\n"); raise CPSTRANS)
    fun fbug1 s = fbug "FLINT2CPS" s
    fun fbug2 s = fbug "FLINT2CPS" s
    fun fbug3 s = fbug "FLINT2CPS" s
in

(**************************************************
 * utility functions for meta-level continuations *
 **************************************************)

datatype mcont = MCONT of {cnt: value list -> lexp, ts: nty list}  (* NOTE: field ts is used in performEta *) 

fun appmc (MCONT{cnt, ...}, vs) = cnt vs

fun makmc (cnt, ts) = MCONT{cnt = cnt, ts = ts}

fun rttys (MCONT{ts,...}) = ts

(****************************
 * CPS conversion for types *
 ****************************)

fun transTy t =
    case t of
      NT_VAR tv               => NT_VAR tv
    | (NT_INT | NT_SINT _)    => t
    | NT_ARRAY t'             => NT_ARRAY (transTy t')
    | NT_REF t'               => NT_REF (transTy t')
    | NT_FIX (tv, i, ts)      => NT_FIX (tv, i, map transTy ts)
    | NT_TUPLE ts             => NT_TUPLE (map transTy ts)
    | NT_EXIST (tv, t')       => fbug1 "NT_EXIST is not supported for FLINT phase"
    | NT_CODE (tvs, ts1, ts2) => NT_CODE (tvs, (contTy ts2) :: (map transTy ts1), [])

and contTy t = NT_CODE ([], map transTy t, [])

(***************************
 * CPS conversion for exps *
 ***************************)

val mkv = LV.mkLvar

exception Rename
val vT : value T.hash_table = T.mkTable (32, Rename)

fun rename lv = T.lookup vT lv
                handle Rename => VAR lv
 
(* newname : lvar_F * value_K -> unit *)
val newname = T.insert vT 

fun newnames (lvs, vs) =
    if length lvs = length vs
    then app newname (LP.zip (lvs, vs))
    else fbug "FLINT2CPS" ("lengths of name lists mismatch (" ^ Int.toString (length lvs) ^ ", "
                           ^ Int.toString (length vs) ^ ")")

fun varEq (VAR x, VAR y) = x = y
  | varEq _ = false

fun isEta (APP(v as VAR lv, ts, vs), vs') = (* need to check the while loop? *)
    let fun h (x::xs, y::ys) =
            if varEq (x, y) andalso (not (varEq (v, y))) then h(xs, ys) else NONE
          | h ([], []) = SOME v
          | h _ = NONE
    in  h (vs', vs)
    end
  | isEta _ = NONE

fun performEta (MCONT{cnt = c, ts = ts}) =
    let val lvs = map (fn _ => mkv()) ts
        val vs = map VAR lvs
        val e = c vs
    in  case isEta (e, vs) of
          SOME v => (fn x => x, v)
        | NONE   => let val f = mkv() in (fn x => FIX ([(CONT, f, [], LP.zip (lvs, ts), e)], x), VAR f) end
    end

fun transVal v =
    case v of
      VAR lv           => rename lv
    | LABEL lv         => fbug "FLINT2CPS" "unexpected LABEL for FLINT"
    | (INT _ | SINT _) => v

fun transVp (v, ap) = (transVal v, ap)

(* transExp: lexp_F * mcont -> lexp_K *)
fun transExp exp cont =
    let fun trans e = transExp e cont
    in  case exp of
          RET vs                => appmc (cont, map transVal vs)
        | LET (lvs, e, e')      =>
            let val ts = U.getTyExp e
                val c = makmc (fn vs => (newnames (lvs, vs); trans e'), ts)
            in  transExp e c
            end
        | FIX (fds, e)          => FIX (map transFun fds, trans e)
        | APP (v, ts, vs)       =>
            let val (hdr, f) = performEta cont
            in  hdr (APP (transVal v, map transTy ts, f::(map transVal vs)))
            end
        | PTAPP _               => fbug2 "PTAPP is not supported during this phase"
        | PACK _                => fbug2 "PACK is not supported during this phase"
        | UNPACK _              => fbug2 "UNPACK is not supported during this phase"
        | SWITCH (v, es, lves)  => SWITCH (transVal v, map trans es, map (fn (lv, e) => (lv, trans e)) lves)
        | CON (v, t, lv, e)     => CON (transVal v, transTy t, lv, trans e)
        | RECORD ([], lv, e)    => let val _ = newname (lv, INT 0) in trans e end
        | RECORD (vps, lv, e)   => RECORD (map transVp vps, lv, trans e)
        | SELECT (v, i, lv, e)  => SELECT (transVal v, i, lv, trans e)
        | BRANCH (c, vs, e, e') => BRANCH (c, map transVal vs, trans e, trans e')
                                   (* a more efficient impl needs to introduce one FIX here, c.f. CwC p.p.59 *)
        | PRIMOP (a, vs, lv, e) => PRIMOP (a, map transVal vs, lv, trans e)
    end

and transFun (fk, lv, tvs, lvts, e) =
    let val oldRetTy = (case fk of ESCAPE (SOME t) => t | _ => fbug3 "return type expected")
        val retTy = contTy oldRetTy
        val cv = mkv()
        val tvs' = tvs
        val lvts' = (cv, retTy)::(map (fn (lv, t) => (lv, transTy t)) lvts)
        val c = makmc (fn vs => APP (VAR cv, [], vs), U.getTyExp e)
    in  (ESCAPE NONE, lv, tvs', lvts', transExp e c) 
    end

fun transProg p = (T.clear vT; transFun p)
(*
fun transProg (fk, lv, tvs, lvts, e) =
    let val cv = mkvar()
    in  transExp e (fn x => transVal x) 
    end
*)

end (* toplevel local *)
end (* structure Flint2CPS *)
