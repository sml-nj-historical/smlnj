(* Copyright 1996 by Bell Laboratories *)
(* lcontract.sml *)

signature LCONTRACT =
sig
  val lcontract : Lambda.lexp -> Lambda.lexp
end 

structure LContract : LCONTRACT =
struct

local structure DI = DebIndex
      open Access Lambda
in

val sameName = LambdaVar.sameName
fun bug s = ErrorMsg.impossible ("LambdaOpt: "^s)
val ident = fn le => le
fun all p (a::r) = p a andalso all p r | all p nil = true

fun isDiff(x, VAR v) = (x <> v)
  | isDiff(x, GENOP({default,table}, _, _, _)) = 
      (x <> default) andalso (all (fn (_, w) => (x <> w)) table)
  | isDiff _ = true

datatype info
  = CompExp
  | SimpVal of value
  | ListExp of value list
  | FunExp of lvar * lty * lexp
  | SimpExp

fun isPure(SVAL _) = true
  | isPure(RECORD _) = true
  | isPure(SRECORD _) = true
  | isPure(VECTOR _) = true
  | isPure(SELECT _) = true
  | isPure(FN _) = true
  | isPure(TFN _) = true
  | isPure(CON _) = true
  | isPure(DECON _) = true (* this can be problematic *)
  | isPure(ETAG _) = true
  | isPure(PACK _) = true
  | isPure(WRAP _) = true
  | isPure(UNWRAP _) = true
  | isPure(SWITCH(v, _, ces, oe)) = 
      let fun g((_,x)::r) = if isPure x then g r else false
            | g [] = case oe of NONE => true | SOME z => isPure z
       in g ces
      end
  | isPure _ = false
  (*** the cases for FIX and LET have already been flattened, thus
       they should not occur ***)

exception LContPass1
fun pass1 lexp = 
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
               | SOME d => if (d=nd) then add(x, NONE)
                           else ()
         end) handle _ => ()

      fun cand x = (get x; true) handle _ => false

      fun loop (e, d) = 
        let fun psv (VAR x) = kill x
              | psv _ = ()
         
            and pse (SVAL v) = psv v
              | pse (FN(v, _, e)) = pse e
              | pse (APP(VAR x, v2)) = (mark d x; psv v2)
              | pse (APP(v1, v2)) = (psv v1; psv v2)
              | pse (FIX(vs, ts, es, be)) = (app pse es; pse be)
              | pse (LET(v, FN (_,_,e1), e2)) = (enter(v, d); pse e1; pse e2)
              | pse (LET(v, e1, e2)) = (pse e1; pse e2)
              | pse (TFN(ks, e)) = loop(e, DI.next d)
              | pse (TAPP(v, _)) = psv v
              | pse (VECTOR(vs,_)) = app psv vs
              | pse (RECORD vs) = app psv vs
              | pse (SRECORD vs) = app psv vs
              | pse (SELECT(_,v)) = psv v
              | pse (CON(_,_,v)) = psv v
              | pse (DECON(_,_,v)) = psv v
              | pse (SWITCH(v, _, ces, oe)) =
                  (psv v; app (fn (_,x) => pse x) ces; 
                   case oe of NONE => () | SOME x => pse x)
              | pse (ETAG(v, _)) = psv v
              | pse (HANDLE(e,v)) = (pse e; psv v)
              | pse (PACK(_,_,_,v)) = psv v
              | pse (WRAP(_,_,v)) = psv v
              | pse (UNWRAP(_,_,v)) = psv v
              | pse (RAISE _) = ()

         in pse e
        end

   in loop (lexp, DI.top); cand
  end

(************************************************************************
 *                      THE MAIN FUNCTION                               *
 ************************************************************************)
fun lcontract lexp = 
let 

val isCand = pass1 lexp

exception LContract
val m : (int ref * info) Intmap.intmap = Intmap.new(32, LContract)

val enter = Intmap.add m
val get = Intmap.map m
val kill = Intmap.rmv m

fun chkIn (v, info) = enter(v, (ref 0, info))

fun refer v = 
  ((case get v
     of (_, SimpVal sv) => SOME sv
      | (x, _) => (x := (!x) + 1; NONE)) handle _ => NONE)

fun selInfo v = (SOME(get v)) handle _ => NONE

fun chkOut v = 
  (let val x = get v
    in kill v; SOME x
   end handle _ => NONE)


fun mkInfo (_, RECORD vs) = ListExp vs
  | mkInfo (_, SRECORD vs) = ListExp vs
  | mkInfo (v, SELECT(i, VAR x)) = 
      let fun h z = 
            (case selInfo z
              of SOME(_, ListExp vs) => 
                   let val nv = List.nth(vs, i)
                         handle _ => bug "unexpected List.Nth in SELECT"
                    in SimpVal nv
                   end
               | SOME(_, SimpVal (VAR w)) => h w
               | _ => SimpExp)
        in h x
       end

  | mkInfo (v, e as FN x) = if isCand v then FunExp x else SimpExp
  | mkInfo (_, e) = if isPure e then SimpExp else CompExp

fun lpacc (LVAR v) = 
      (case lpsv (VAR v)
        of VAR w => LVAR w
         | _ => bug "unexpected in lpacc")
  | lpacc _ = bug "unexpected path in lpacc"

and lpdc (s, EXN acc, t) = (s, EXN(lpacc acc), t)
  | lpdc x = x

and lpcon (DATAcon dc) = DATAcon(lpdc dc)
  | lpcon c = c

and lpdt {default=v, table=ws} =
  let fun h x = case (refer x)
                 of SOME(VAR nv) => nv
                  | NONE => x
   in {default=h v, table=map (fn (ts,w) => (ts,h w)) ws}
  end

and lpsv x = 
  (case x
    of VAR v => (case (refer v) of SOME nsv => lpsv nsv 
                                 | NONE => (x : value))
     | GENOP(dict, p, lt, ts) => GENOP(lpdt dict, p, lt, ts)
     | _ => x)

and loop le =
  (case le
    of SVAL v => SVAL(lpsv v)
     | FN(v, t, e) => FN(v, t, loop e)
     | APP(v1 as VAR x, v2) => 
         (case selInfo x
           of SOME(ref c, FunExp(z,_,b)) => 
               (if (c = 0) then loop(LET(z, SVAL v2, b))
                else bug "unexpected FunExp in APP")
(* commented out because it won't have any effect for the time being.
            | SOME(_, SimpVal (y as VAR _)) => loop(APP(y, v2)) 
*)
            | _ => APP(lpsv v1, lpsv v2))
     | APP(v1, v2) => APP(lpsv v1, lpsv v2)
     | FIX(vs, ts, es, b) => 
         let fun g ((FN _)::r) = g r
               | g (_::r) = false
               | g [] = true
             val _ = if g es then () else bug "unexpected cases in loop-FIX"
             val _ = app (fn x => chkIn(x, SimpExp)) vs
             val nb = loop b
             val ws = map chkOut vs

             fun h ((SOME(ref 0, _))::r) = h r
               | h (_::r) = false
               | h [] = true
          in if h ws then nb 
             else FIX(vs, ts, map loop es, nb)
         end
     | LET(v, LET(u, e1, e2), e3) => 
         loop(LET(u, e1, LET(v, e2, e3)))
     | LET(v, FIX(vs, ts, es, b), e) =>
         loop(FIX(vs, ts, es, LET(v, b, e)))
     | LET(v, SVAL sv, e2) => 
         (chkIn(v, SimpVal sv); loop e2)
     | LET(v, e1, e2 as SVAL (VAR x)) =>
         if (v = x) then loop e1
         else if isPure e1 then loop e2
              else LET(v, loop e1, loop e2)
     | LET(v, e1 as FN(v1, t1, b1), e2 as APP(VAR x, sv)) =>
         if isDiff(v, sv) then
           (if (v = x) then loop(LET(v1, SVAL sv, b1)) else loop e2)
         else LET(v, loop e1, loop e2)
     | LET(v, e1, e2) => 
         let val _ = chkIn(v, mkInfo(v,e1))
             val ne2 = loop e2
             val w = chkOut v
          in case w 
              of SOME(_, CompExp) => LET(v, loop e1, ne2)
               | SOME(ref 0, _) => ne2
               | _ => (case (e1, ne2)
                        of (FN(v1,t1,b1), APP(VAR x, sv)) =>
                             if isDiff(v, sv) then
                              (if (v=x) then loop(LET(v1, SVAL sv,b1))
                               else ne2)
                             else LET(v, loop e1, ne2)
                         | (_, SVAL(VAR x)) =>
                             if isPure e1 then (if v=x then loop e1
                                                else ne2)
                             else LET(v, loop e1, ne2)
                         | _ => LET(v, loop e1, ne2))
         end
     | TFN(ks, e) => TFN(ks, loop e)
     | TAPP(v, ts) => TAPP(lpsv v, ts)
     | VECTOR(vs, t) => VECTOR(map lpsv vs, t)
     | RECORD vs => RECORD (map lpsv vs)
     | SRECORD vs => SRECORD (map lpsv vs)
     | SELECT(i, v as VAR x) => 
         (case selInfo x
           of SOME(_, ListExp vs) => 
                let val nv = List.nth(vs, i)
                      handle _ => bug "unexpected List.Nth in SELECT"
                 in SVAL(lpsv nv)
                end
            | SOME(_, SimpVal (y as VAR _)) => loop(SELECT(i, y))
            | _ => SELECT(i, lpsv v))
     | SELECT(i, v) => SELECT(i, lpsv v)
     | CON(c, ts, v) => CON(lpdc c, ts, lpsv v)
     | DECON(c, ts, v) => DECON(lpdc c, ts, lpsv v)
     | SWITCH (v, cs, ces, oe) => 
         let val nv = lpsv v
             val nces = map (fn (c, e) => (lpcon c, loop e)) ces
             val noe = case oe of NONE => NONE | SOME e => SOME (loop e)
          in SWITCH(nv, cs, nces, noe)
         end
     | ETAG(v, t) => ETAG(lpsv v, t)
     | RAISE(v, t) => RAISE(lpsv v, t)
     | HANDLE(e, v) => HANDLE(loop e, lpsv v)
     | PACK(t, ts1, ts2, v) => PACK(t, ts1, ts2, lpsv v)
     | WRAP(t, b, v) => WRAP(t, b, lpsv v)
     | UNWRAP(t, b, v) => UNWRAP(t, b, lpsv v))

val nlexp = loop lexp
in (Intmap.clear m; nlexp)
end 

end (* toplevel local *)
end (* structure LContract *)

