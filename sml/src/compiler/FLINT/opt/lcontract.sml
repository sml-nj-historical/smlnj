(* Copyright 1996 by Bell Laboratories *)
(* lcontract.sml *)

signature LCONTRACT =
sig
  val lcontract : FLINT.prog -> FLINT.prog
end 

structure LContract : LCONTRACT =
struct

local structure DI = DebIndex
      structure DA = Access
      structure LT = LtyExtern
      structure FU = FlintUtil
      open FLINT
in

fun bug s = ErrorMsg.impossible ("LContract: "^s)
val say = Control.Print.say
val ident = fn x => x
fun all p (a::r) = p a andalso all p r | all p nil = true

fun isDiffs (vs, us) = 
  let fun h (VAR x) = List.all (fn y => (y<>x)) vs
        | h _ = true
   in List.all h us
  end

fun isEqs (vs, us) = 
  let fun h (v::r, (VAR x)::z) = if v = x then h(r, z) else false
        | h ([], []) = true
        | h _ = false
   in h(vs, us)
  end

datatype info
  = SimpVal of value
  | ListExp of value list
  | FunExp of DI.depth * lvar list * lexp
  | ConExp of dcon * tyc list * value
  | StdExp

exception LContPass1
fun pass1 fdec = 
  let val zz : (DI.depth option) Intmap.intmap = Intmap.new(32, LContPass1)
      val add = Intmap.add zz
      val get = Intmap.map zz
      val rmv = Intmap.rmv zz
      fun enter(x, d) = add(x, SOME d)
      fun kill x = ((get x; rmv x) handle _ => ())
      fun mark nd x = 
        (let val s = get x
             val _ = rmv x
          in case s
              of NONE => ()
               | SOME _ => add(x, NONE)  (* depth no longer matters *)
(*
               | SOME d => if (d=nd) then add(x, NONE)
                           else ()
*)
         end) handle _ => ()

      fun cand x = (get x; true) handle _ => false

      fun lpfd d ({isrec=SOME _,...}, v, vts, e) = lple d e
        | lpfd d (_, v, vts, e) = (enter(v, d); lple d e)

      and lple d e = 
        let fun psv (VAR x) = kill x
              | psv _ = ()

            and pst (v, vks, e) = lple (DI.next d) e
         
            and pse (RET vs) = app psv vs
              | pse (LET(vs, e1, e2)) = (pse e1; pse e2)          
              | pse (FIX(fdecs, e)) = (app (lpfd d) fdecs; pse e) 
              | pse (APP(VAR x, vs)) = (mark d x; app psv vs)
              | pse (APP(v, vs)) = (psv v; app psv vs)
              | pse (TFN(tfdec, e)) = (pst tfdec; pse e)
              | pse (TAPP(v, _)) = psv v
              | pse (RECORD(_,vs,_,e)) = (app psv vs; pse e)
              | pse (SELECT(u,_,_,e)) = (psv u; pse e)
              | pse (CON(_,_,u,_,e)) = (psv u; pse e)
              | pse (SWITCH(u, _, ces, oe)) =
                  (psv u; app (fn (_,x) => pse x) ces; 
                   case oe of NONE => () | SOME x => pse x)
              | pse (RAISE _) = ()
              | pse (HANDLE(e,v)) = (pse e; psv v)
              | pse (BRANCH(_, vs, e1, e2)) = (app psv vs; pse e1; pse e2) 
              | pse (PRIMOP(_, vs, _, e)) = (app psv vs; pse e)

         in pse e
        end

   in lpfd DI.top fdec; (cand, fn () => Intmap.clear zz)
  end (* pass1 *)

(************************************************************************
 *                      THE MAIN FUNCTION                               *
 ************************************************************************)
fun lcontract (fdec, init) = 
let 

(* In pass1, we calculate the list of functions that are the candidates
 * for contraction. To be such a candidate, a function must be called 
 * only once, and furthermore, the call site must be at the same 
 * depth as the definition site. (ZHONG)
 *
 * Being at the same depth is not strictly necessary, we'll relax this
 * constraint in the future.
 *)
val (isCand, cleanUp) = 
 if init then (fn _ => false, fn () => ()) else pass1 fdec

exception LContract
val m : (int ref * info) Intmap.intmap = Intmap.new(32, LContract)

val enter = Intmap.add m
val get = Intmap.map m
val kill = Intmap.rmv m

fun chkIn (v, info) = enter(v, (ref 0, info))

(** check if a variable is dead *)
fun dead v = (case get v of (ref 0, _) => true
                          | _ => false) handle _ => false

fun once v = (case get v of (ref 1, _) => true | _ => false) handle _ => false

(** check if all variables are dead *)
fun alldead [] = true
  | alldead (v::r) = if dead v then alldead r else false 

(** renaming a value *)
fun rename (u as (VAR v)) = 
      ((case get v
         of (_, SimpVal sv) => rename sv
          | (x, _) => (x := (!x) + 1; u)) handle _ => u)
  | rename u = u

(** selecting a field from a potentially known record *)
fun selInfo (VAR v, i)  = 
      ((case get v
         of (_, SimpVal u) => selInfo (u, i)
          | (_, ListExp vs) => 
              let val nv = List.nth(vs, i)
                           handle _ => bug "unexpected List.Nth in selInfo"
               in SOME nv
              end
          | _ => NONE) handle _ => NONE)
  | selInfo _ = NONE

(** applying a switch to a data constructor *)
fun swiInfo (VAR v, ces, oe) = 
      ((case get v
         of (_, SimpVal u) => swiInfo(u, ces, oe)
          | (_, ConExp (dc as (_,rep,_), ts, u)) =>
               let fun h ((DATAcon(dc as (_,nrep,_),ts,x),e)::r) =
                         if rep=nrep then SOME(LET([x], RET [u], e)) else h r
                     | h (_::r) = bug "unexpected case in swiInfo"
                     | h [] = oe
                in h ces
               end
          | _ => NONE) handle _ => NONE)
  | swiInfo _ = NONE

(** contracting a function application *)
fun appInfo (VAR v) =
      ((case get v
         of (ref 0, FunExp (d, vs, e)) => SOME (d, vs, e)
          | _ => NONE) handle _ => NONE)
  | appInfo _ = NONE


(** A very ad-hoc implementation of branch/switch eliminations *)
local

fun isBoolLty lt = 
  (case LT.ltd_arrow lt
    of (_, [at], [rt]) =>
         (LT.lt_eqv(at, LT.ltc_unit)) andalso (LT.lt_eqv(rt, LT.ltc_bool))
     | _ => false) 

fun isBool true (RECORD(RK_TUPLE _, [], x, 
                  CON((_,DA.CONSTANT 1,lt), [], VAR x', v, RET [VAR v']))) = 
      (x = x') andalso (v = v') andalso (isBoolLty lt)
  | isBool false (RECORD(RK_TUPLE _, [], x, 
                  CON((_,DA.CONSTANT 0,lt), [], VAR x', v, RET [VAR v']))) = 
      (x = x') andalso (v = v') andalso (isBoolLty lt)
  | isBool _ _ = false

(* functions that do the branch optimizations *)
fun boolDcon((DATAcon((_,DA.CONSTANT 1,lt1),[],v1), e1), 
             (DATAcon((_,DA.CONSTANT 0,lt2),[],v2), e2)) = 
      if (isBoolLty lt1) andalso (isBoolLty lt2) then
        SOME(RECORD(FU.rk_tuple,[],v1,e1), RECORD(FU.rk_tuple,[],v2,e2))
      else NONE
  | boolDcon(ce1 as (DATAcon((_,DA.CONSTANT 0,_),[],_), _), 
             ce2 as (DATAcon((_,DA.CONSTANT 1,_),[],_), _)) =
      boolDcon (ce2, ce1)
  | boolDcon _ = NONE

fun ssplit (LET(vs,e1,e2)) = (fn x => LET(vs,x,e2), e1)
  | ssplit e = (ident, e)

in

fun branchopt([v], e1 as (BRANCH(p, us, e11, e12)), e2) = 
      let val (hdr, se2) = ssplit e2
       in case se2 
           of SWITCH(VAR nv, _, [ce1, ce2], NONE) =>
                if (once v) andalso (nv = v) andalso (isBool true e11) 
                   andalso (isBool false e12)
                then (case boolDcon (ce1, ce2)
                       of SOME (e21, e22) => SOME(hdr(BRANCH(p, us, e21, e22)))
                        | NONE => NONE)
                else NONE
            | _ => NONE
      end
  | branchopt _ = NONE

end (* branchopt local *)


(** the main transformation function *)
fun transform [] = bug "unexpected case in transform"
  | transform (cfg as ((d, od, k)::rcfg)) = let
     fun h (f, t, (d, od, k)::r, sk) = h(f, f(t, od, d, k+sk), r, k+sk)
       | h (f, t, [], _) = t
     fun ltf t = h(LT.lt_adj_k, t, cfg, 0)
     fun tcf t = h(LT.tc_adj_k, t, cfg, 0)

     fun lpacc (DA.LVAR v) = 
           (case lpsv (VAR v) of VAR w => DA.LVAR w
                               | _ => bug "unexpected in lpacc")
       | lpacc _ = bug "unexpected path in lpacc"

     and lpdc (s, DA.EXN acc, t) = (s, DA.EXN(lpacc acc), ltf t)
       | lpdc (s, rep, t) = (s, rep, ltf t)

     and lpcon (DATAcon (dc, ts, v)) = DATAcon(lpdc dc, map tcf ts, v)
       | lpcon c = c

     and lpdt (SOME {default=v, table=ws}) =
           let fun h x = 
                 case rename (VAR x) of VAR nv => nv
                                      | _ => bug "unexpected acse in lpdt"
            in (SOME {default=h v, table=map (fn (ts,w) => (ts,h w)) ws})
           end
       | lpdt NONE = NONE

     and lpsv x = (case x of VAR v => rename x | _ => x)

     and lpfd (fk, v, vts, e) = 
       let val nfk = 
             case fk 
              of {isrec=SOME(t,lk), known, inline, cconv} =>
                   {isrec=SOME(map ltf t, lk), known=known, inline=inline,
                           cconv=cconv} 
               | _ => fk
        in (nfk, v, map (fn (v,t) => (v,ltf t)) vts, #1(loop e))
       end

     and lplet (hdr: lexp -> lexp, pure, v: lvar, info: info, e) = 
       let val _ = chkIn(v, info)
           val (ne, b) = loop e
        in if pure then (if dead v then (ne, b) else (hdr ne, b))
           else (hdr ne, false)
       end (* function lplet *)

     and loop le =
       (case le
         of RET vs => (RET (map lpsv vs), true)
          | LET(vs, RET us, e) =>
              (ListPair.app chkIn (vs, map SimpVal us); loop e)
          | LET(vs, LET(us, e1, e2), e3) => 
              loop(LET(us, e1, LET(vs, e2, e3)))
          | LET(vs, FIX(fdecs, e1), e2) =>
              loop(FIX(fdecs, LET(vs, e1, e2)))
          | LET(vs, TFN(tfd, e1), e2) => 
              loop(TFN(tfd, LET(vs, e1, e2)))
          | LET(vs, CON(dc, ts, u, v, e1), e2) =>
              loop(CON(dc, ts, u, v, LET(vs, e1, e2)))
          | LET(vs, RECORD(rk, us, v, e1), e2) => 
              loop(RECORD(rk, us, v, LET(vs, e1, e2)))
          | LET(vs, SELECT(u, i, v, e1), e2) => 
              loop(SELECT(u, i, v, LET(vs, e1, e2)))
          | LET(vs, PRIMOP(p, us, v, e1), e2) =>
              loop(PRIMOP(p, us, v, LET(vs, e1, e2)))
          | LET(vs, e1, e2 as (RET us)) =>
              if isEqs(vs, us) then loop e1
              else let val (ne1, b1) = loop e1
                       val nus = map lpsv us
                    in if (isDiffs(vs, nus)) andalso b1 then (RET nus, true)
                       else (LET(vs, ne1, RET nus), b1)
                   end
          | LET(vs, e1, e2) => 
              let val _ = app (fn v => chkIn(v, StdExp)) vs
                  val (ne1, b1) = loop e1
                  val (ne2, b2) = loop e2
               in if (alldead vs) andalso b1 then (ne2, b2)
                  else (case branchopt(vs, ne1, ne2)
                         of SOME xx => (xx, b1 andalso b2)
                          | NONE => 
                              (case ne2 
                                of (RET us) => 
                                     if isEqs(vs, us) then (ne1, b1)
                                     else (LET(vs, ne1, ne2), b1)
                                 | _ => (LET(vs, ne1, ne2), b1 andalso b2)))
              end

          | FIX(fdecs, e) =>
              let fun g ({isrec=SOME _, ...} :fkind, v, _, _) =
                         chkIn(v, StdExp)
                    | g ((_, v, vts, xe) : fundec) = 
                         chkIn(v, if isCand v then FunExp(od, map #1 vts, xe) 
                                  else StdExp)
                  val _ = app g fdecs
                  val (ne, b) = loop e
               in if alldead (map #2 fdecs) then (ne, b)
                  else (FIX(map lpfd fdecs, ne), b)
              end
          | APP(u, us) => 
              (case appInfo u
                of SOME(od', vs, e) => 
                     let val ne = LET(vs, RET us, e)
                      in transform ((od, od', 0)::cfg) ne
                     end
                 | _ => (APP(lpsv u, map lpsv us), false))

          | TFN(tfdec as (v, tvks, xe), e) => 
              lplet ((fn z => TFN((v, tvks, 
                              #1(transform ((DI.next d, DI.next od, 
                                            k+1)::rcfg) xe)), z)), 
                     true, v, StdExp, e)
          | TAPP(u, ts) => (TAPP(lpsv u, map tcf ts), true)

          | CON(c, ts, u, v, e) =>   (* this could be made more finegrain *)
              lplet ((fn z => CON(lpdc c, map tcf ts, lpsv u, v, z)), 
                     true, v, ConExp(c,ts,u), e)
          | SWITCH (v, cs, ces, oe) => 
              (case swiInfo(v, ces, oe)
                of SOME ne => loop ne
                 | _ => let val nv = lpsv v
                            fun h ((c, e), (es, b)) = 
                              let val nc = lpcon c
                                  val (ne, nb) = loop e
                               in ((nc, ne)::es, nb andalso b)
                              end
                            val (nces, ncb) = foldr h ([], true) ces 
                            val (noe, nb) = 
                              case oe 
                               of NONE => (NONE, ncb)
                                | SOME e => let val (ne, b) = loop e
                                             in (SOME ne, b andalso ncb)
                                            end
                         in (SWITCH(nv, cs, nces, noe), nb)
                        end)

          | RECORD (rk, us, v, e) => 
              lplet ((fn z => RECORD(rk, map lpsv us, v, z)), 
                     true, v, ListExp us, e)
          | SELECT(u, i, v, e) => 
              (case selInfo (u, i)
                of SOME nv => (chkIn(v, SimpVal nv); loop e)
                 | NONE => lplet ((fn z => SELECT(lpsv u, i, v, z)), 
                                  true, v, StdExp, e))

          | RAISE(v, ts) => (RAISE(lpsv v, map ltf ts), false)
          | HANDLE(e, v) => 
              let val (ne, b) = loop e
               in if b then (ne, true)
                  else (HANDLE(ne, lpsv v), false)
              end

          | BRANCH(px as (d, p, lt, ts), vs, e1, e2) =>
              let val (ne1, b1) = loop e1
                  val (ne2, b2) = loop e2
               in (BRANCH(case (d,ts) of (NONE, []) => px 
                                       | _ => (lpdt d, p, lt, map tcf ts), 
                          map lpsv vs, ne1, ne2), false)
              end
          | PRIMOP(px as (dt, p, lt, ts), vs, v, e) => 
              lplet ((fn z => PRIMOP((case (dt, ts) 
                                       of (NONE, []) => px 
                                        | _ => (lpdt dt, p, lt, map tcf ts)), 
                                     map lpsv vs, v, z)), 
                     false (* isPure p *), v, StdExp, e))

     in loop
    end (* function transform *)

val d = DI.top
val (fk, f, vts, e) = fdec
in (fk, f, vts, #1 (transform [(d, d, 0)] e))
   before (Intmap.clear m; cleanUp())
end (* function lcontract *)

(** run the lambda contraction twice *)
val lcontract = fn fdec => lcontract(lcontract(fdec, true), false)

end (* toplevel local *)
end (* structure LContract *)


(*
 * $Log$
 *)
