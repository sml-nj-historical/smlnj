(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* recover.sml *)

(* recover the type information of a closed FLINT program *)

signature RECOVER = 
sig 
  val recover : (FLINT.prog * bool) -> 
                  {getLty: DebIndex.depth -> FLINT.value -> FLINT.lty,
                   cleanUp: unit -> unit}
end (* signature SPECIALIZE *)

structure Recover : RECOVER = 
struct 

local structure LT = LtyExtern
      structure DI = DebIndex
      open FLINT
in

fun bug s = ErrorMsg.impossible ("Recover: "^s)

fun ltInst (lt, ts) =
  (case LT.lt_inst(lt, ts)
    of [x] => x
     | _ => bug "unexpected case in ltInst")

(** there two functions are applicable to the types of primops and data
    constructors only (ZHONG) *)
fun arglty (lt, ts) = 
  let val (_, atys, _) = LT.ltd_arrow(ltInst(lt, ts))
   in case atys of [x] => x
                 | _ => bug "unexpected case in arglty"
  end
fun reslty (lt, ts) =
  let val (_, _, rtys) = LT.ltd_arrow(ltInst(lt, ts))
   in case rtys of [x] => x
                 | _ => bug "unexpected case in reslty"
  end

exception RecoverLty
fun recover (fdec, postRep) = 
  let val zz : (lty * DI.depth) Intmap.intmap = Intmap.new(32, RecoverLty)
      val add = Intmap.add zz
      val get = Intmap.map zz
      fun addvar d (x, t) = add(x, (t, d))
      fun addvars d vts = app (addvar d) vts
      fun getlty d (VAR v) = 
            let val (t, od) = get v
             in LT.lt_adj(t, od, d)
            end
        | getlty d (INT _ | WORD _) = LT.ltc_int
        | getlty d (INT32 _ | WORD32 _) = LT.ltc_int32
        | getlty d (REAL _) = LT.ltc_real
        | getlty d (STRING _) = LT.ltc_string

      (* loop : depth -> lexp -> lty list *)
      fun loop d e = 
        let fun lpv u = getlty d u
            fun lpvs vs = map lpv vs
            val addv = addvar d
            val addvs = addvars d

            fun lpd (fk, f, vts, e) = 
              (addvs vts; addv (f, LT.ltc_fkfun(fk, map #2 vts, lpe e)))

            and lpds (fds as ((fk as FK_FUN{isrec=SOME _, ...},_,_,_)::_)) =
                  let fun h ((fk as FK_FUN{isrec=SOME rts, ...}, 
                             f, vts, _) : fundec) = 
                            addv(f, LT.ltc_fkfun(fk, map #2 vts, rts)) 
                        | h _ = bug "unexpected case in lpds" 
                      val _ = app h fds
                   in app lpd fds
                  end
              | lpds [fd] = lpd fd
              | lpds _ = bug "unexpected case 2 in lpds"

            and lpc (DATAcon((_,_,lt), ts, v), e) = 
                  (addv (v, arglty(lt, ts)); lpe e)
              | lpc (_, e) = lpe e

            and lpe (RET vs) = lpvs vs
              | lpe (LET(vs, e1, e2)) = 
                  (addvs (ListPair.zip(vs, lpe e1)); lpe e2)
              | lpe (FIX(fdecs, e)) = (lpds fdecs; lpe e)
              | lpe (APP(u, vs)) = #2(LT.ltd_fkfun (lpv u))
              | lpe (TFN((v, tvks, e1), e2)) = 
                  (addv(v, LT.ltc_poly(map #2 tvks, loop (DI.next d) e1));
                   lpe e2)
              | lpe (TAPP(v, ts)) = LT.lt_inst (lpv v, ts)
              | lpe (RECORD(rk,vs,v,e)) = 
                  (addv (v, LT.ltc_rkind(rk, lpvs vs)); lpe e)
              | lpe (SELECT(u,i,v,e)) = 
                  (addv (v, LT.ltd_rkind(lpv u, i)); lpe e)
              | lpe (CON((_,_,lt),ts,_,v,e)) = 
                  (addv (v, reslty(lt, ts)); lpe e)
              | lpe (SWITCH(_, _, ces, e)) =
                  let val lts = map lpc ces
                   in case e of NONE => hd lts
                              | SOME e => lpe e
                  end
              | lpe (RAISE (_, lts)) = lts
              | lpe (HANDLE(e, _)) = lpe e
              | lpe (BRANCH(p, _, e1, e2)) = 
                  let val _ = lpe e1
                   in lpe e2
                  end
              | lpe (PRIMOP((_,PrimOp.WCAST, lt, []), _, v, e)) = 
                  if postRep then 
                     (case LT.ltd_fct lt
                       of ([_],[r]) => (addv(v, r); lpe e)
                        | _ => bug "unexpected case for WCAST")
                  else bug "unexpected primop WCAST in recover"
              | lpe (PRIMOP((_,_,lt,ts), _, v, e)) = 
                  (addv (v, reslty (lt, ts)); lpe e)

         in lpe e 
        end (* function transform *)

      val (fkind, f, vts, e) = fdec
      val d = DI.top
      val _ = addvars d vts
      val atys = map #2 vts
      val rtys = loop d e
      val _ = addvar d (f, LT.ltc_fkfun(fkind, atys, rtys))
  in {getLty=getlty, cleanUp=fn () => Intmap.clear zz}
 end (* function recover *)

end (* local *)
end (* structure Recover *)
