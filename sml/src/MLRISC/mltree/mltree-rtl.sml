(*
 * Basic RTLs and query functions on these RTLs
 *
 * -- Allen
 *)
structure MLTreeRTL : MLTREE_RTL =
struct

   fun error msg = MLRiscErrorMsg.error("MLTreeRTL",msg)

   (* Dummy modules *)
   structure Const : CONSTANT =
   struct
     type const = int
     fun toString s = "r"^Int.toString s
     fun valueOf c = c
     fun hash c = Word.fromInt c
     fun ==(x:const,y:const) = x=y
   end

   structure LabelExp = LabelExp(Const)

   structure Region : REGION =
   struct
     type region = int
     val memory = ~1
     val stack = ~1
     val readonly = ~1
     fun toString r = "mem"^Int.toString r
   end
 
   structure PseudoOp : PSEUDO_OPS =
   struct
     type pseudo_op = unit
     fun toString _ = ""
     fun emitValue _ = ()
     fun sizeOf _ = 0
     fun adjustLabels _ = false
   end

   structure RTLExt =
   struct
      structure Basis = MLTreeBasis

      datatype  ('s,'r,'f,'c) sx =
         ASSIGN of 'r loc * 'r
      |  PAR of 's * 's 
   
      and ('s,'r,'f,'c) rx =
         FORALL of 'r
      |  FETCH  of 'r loc
      |  ARG    of string * string
      |  OP     of Basis.misc_op ref * 'r list
      |  SLICE  of {from:'r, to:'r} list * Basis.ty * 'r
   
      and  'r loc  = AGG of Basis.ty * endian * 'r cell
   
      and  'r cell = CELL of string * Basis.ty * 'r * 'r
   
      and   endian = LITTLE_ENDIAN | BIG_ENDIAN
   
      and  ('s,'r,'f,'c) fx = FX
      and  ('s,'r,'f,'c) ccx = CCX
   
   end

   structure T = MLTreeF
     (structure LabelExp  = LabelExp
      structure Region    = Region
      structure Stream    = InstructionStream(PseudoOp)
      structure Extension = RTLExt
     )

   structure W = Word

   val itow = Word.fromInt

   datatype rtlOp     = datatype RTLExt.rx
   datatype rtlAction = datatype RTLExt.sx
   datatype rtlCell   = datatype RTLExt.cell
   datatype rtlLoc    = datatype RTLExt.loc
   datatype rtlEndian = datatype RTLExt.endian

   type action = T.stm
   type rtl    = action
   type exp    = T.rexp
   type cond   = T.ccexp
   type loc    = T.rexp rtlLoc
   type cell   = T.rexp rtlCell
   type ty     = T.ty

   type hasher   = T.hasher
   type equality = T.equality
   type printer  = T.printer
 
   structure Util = MLTreeUtils
     (structure T = T
      fun hashRtlOp hasher (FORALL e) = #rexp (hasher:hasher) e
        | hashRtlOp hasher (FETCH l) = hashLoc hasher l
        | hashRtlOp hasher (ARG _)   = 0w3
        | hashRtlOp hasher (OP(ref{hash, ...}, es)) = hash + hashRexps hasher es
        | hashRtlOp hasher (SLICE(sl, ty, e)) = 
          itow ty + hashSlices hasher sl + #rexp hasher e
      and hashRexps hasher es = foldr (fn (e,h) => #rexp hasher e + h) 0w23 es
      and hashSlices hasher sl = foldr (fn ({from,to},h) =>
              #rexp hasher from + #rexp hasher to + h) 0w33 sl
      and hashLoc hasher (AGG(t, endian, c)) = itow t + hashCell hasher c
      and hashCell hasher (CELL(k, t, e, r)) = 
             itow t + #rexp hasher e + #rexp hasher r
      and hashRtlAction hasher (ASSIGN(l, e)) = 
            hashLoc hasher l + #rexp hasher e
        | hashRtlAction hasher (PAR(a,b)) = #stm hasher a + #stm hasher b

      fun eqRtlOp eq (FORALL x, FORALL y)  = #rexp (eq:equality) (x,y)
        | eqRtlOp eq (FETCH l, FETCH l')   = eqLoc eq (l,l')
        | eqRtlOp eq (ARG x, ARG y)        = x=y
        | eqRtlOp eq (OP(x,es), OP(x',es')) = 
            x=x' andalso eqRexps (#rexp eq) (es,es')
        | eqRtlOp eq (SLICE(sl, t, e), SLICE(sl', t', e')) = 
            t=t' andalso eqSlices eq (sl,sl') andalso #rexp eq (e,e')
        | eqRtlOp eq _ = false
      and eqRtlAction eq (ASSIGN(l, e), ASSIGN(l', e')) = 
             eqLoc eq (l,l') andalso #rexp eq (e, e')
        | eqRtlAction eq (PAR(a, b), PAR(a', b')) = 
             #stm eq (a, a') andalso #stm eq (b, b')
        | eqRtlAction eq _ = false
      and eqRexps eq ([],[]) = true
        | eqRexps eq (x::xs,y::ys) = eq(x,y) andalso eqRexps eq (xs,ys)
        | eqRexps eq _ = false
      and eqSlice eq ({from=x,to=y},{from=x',to=y'}) =
             #rexp eq(x,x') andalso #rexp eq(y,y')
      and eqSlices eq ([], []) = true
        | eqSlices eq (x::xs,y::ys) = 
           eqSlice eq (x,y) andalso eqSlices eq (xs,ys)
        | eqSlices eq _ = false
      and eqLoc eq (AGG(t,e,c), AGG(t',e',c')) = 
          t=t andalso e=e' andalso eqCell eq (c,c')
      and eqCell eq (CELL(k, t, e, r), CELL(k', t', e', r')) =
          t=t' andalso k=k' andalso #rexp eq (e,e') andalso #rexp eq (r,r')

      fun listify f es = 
        List.foldr (fn (e,"") => f e | (e,s) => f e^","^s) "" es

      fun showTy ty = "."^Int.toString ty
      and showRtlOp pr (t,FORALL e) = "forall "^ #rexp (pr:printer) e
        | showRtlOp pr (t,FETCH l)  = showLoc pr l
        | showRtlOp pr (t,ARG(k,x)) = k^" "^x
        | showRtlOp pr (t,OP(ref{name, ...}, es)) = name^showTy t^showExps pr es
        | showRtlOp pr (t,SLICE(sl, ty, e)) = 
            #rexp pr e^" at ["^showSlices pr sl^"]"
      and showSlices pr sl = 
          listify (fn {from,to} => #rexp pr from^".."^ #rexp pr to) sl
      and showLoc pr (AGG(t', endian, CELL(k, t, e, r))) = 
          let val r = case r of
                       T.LI 0 => ""
                     | r  => ":"^ #rexp pr r
              val body = "$"^k^"["^ lhs pr e^r^"]"
          in  if t = t' orelse t = 0 then body else 
              showEnd endian^showTy t'^showTy t^" "^body 
          end
      and lhs pr (T.REG(ty,r)) = #dstReg pr (ty,r)
        | lhs pr e = #rexp pr e
      and showEnd LITTLE_ENDIAN = "aggl"
        | showEnd BIG_ENDIAN    = "aggb"
      and showRtlAction pr (ASSIGN(l, e)) = showLoc pr l ^ " := " ^ #rexp pr e
        | showRtlAction pr (PAR(a,b)) = #stm pr a ^" || "^ #stm pr b
      and showExps pr es = "("^listify (#rexp pr) es^")"

      fun noHash _ _ = 0w0 
      val hashSext = hashRtlAction
      val hashRext = hashRtlOp
      val hashFext = noHash
      val hashCCext = noHash
      fun noEq _ _ = true
      val eqSext = eqRtlAction
      val eqRext = eqRtlOp
      val eqFext = noEq
      val eqCCext = noEq
      fun noShow _ _ = "" 
      val showSext = showRtlAction
      val showRext = showRtlOp
      val showFext = noShow
      val showCCext = noShow
     )

   val hashRTL = Util.hashStm
   val eqRTL   = Util.eqStm
   val showRTL = Util.show

   structure Basis = T.Basis

   structure Rewrite = MLTreeRewrite
    (structure T = T
     fun rext rw (FETCH l) = FETCH(loc rw l)
       | rext rw (FORALL e) = FORALL(#rexp rw e)
       | rext {rexp, fexp, ccexp, stm} (OP(m,es)) = OP(m,map rexp es)
       | rext {rexp, fexp, ccexp, stm} (SLICE(sl, ty, e)) =
              SLICE(map (fn {from,to} => {from=rexp from,to=rexp to}) sl,
                    ty,rexp e)
       | rext {rexp, fexp, ccexp, stm} e = e
     and sext rw (ASSIGN(l, e)) = ASSIGN(loc rw l, #rexp rw e) 
       | sext rw (PAR(a,b))     = PAR(#stm rw a, #stm rw b) 
     and fext rw x = x
     and ccext rw x = x
     and loc rw (AGG(t1,t2,c)) = AGG(t1,t2,cell rw c)
     and cell rw (CELL(k,t,e,r)) = CELL(k,t,#rexp rw e,#rexp rw r)
    )

   val A_TRAPPING   = W.<<(0w1,0w1)
   val A_PINNED     = W.<<(0w1,0w2)
   val A_SIDEEFFECT = W.<<(0w1,0w3)
   val A_MUTATOR    = W.<<(0w1,0w4)
   val A_LOOKER     = W.<<(0w1,0w5)
   val A_BRANCH     = W.<<(0w1,0w6)
   val A_PURE       = 0wx0

   (* 
    * Create new RTL operators 
    *)
   val hashCnt   = ref 0w0
   fun newHash() = let val h = !hashCnt in hashCnt := h + 0w124127; h end
   fun newOp{name,attribs} = ref{name=name,attribs=attribs,hash=newHash()}

   (*
    *  Reduce a RTL to compiled internal form
    *)
   fun reduce rtl =
   let fun getRegion (T.REG(_,r)) = r
         | getRegion e = error("getRegion: "^Util.rexpToString e)
       fun rexp _ (T.REXT(ty,FETCH(AGG(_,_,CELL("GP",_,T.REG(_,r),_))))) = 
              T.REG(ty,r)
         | rexp _ (T.REXT(ty,FETCH(AGG(_,_,CELL("FP",_,T.REG(_,r),_))))) = 
              T.REG(ty,r)
         | rexp _ (T.REXT(ty,FETCH(AGG(_,_,CELL("MEM",_,ea,region)))))=
              T.LOAD(ty,ea,getRegion region)
         | rexp _ e = e
       fun stm _ (T.SEQ[s]) = s
         | stm _ (T.EXT(ASSIGN(AGG(ty,_,CELL("MEM",_,ea,region)),d))) =
              T.STORE(ty,ea,d,getRegion region)
         | stm _ (T.EXT(ASSIGN(AGG(ty,_,CELL("GP",_,T.REG(_,r),_)),d))) = 
              T.MV(ty,r,d)
         (*| stm _ (T.EXT(ASSIGN(AGG(ty,_,CELL("FP",_,T.REG(_,r))),d))) = 
              T.MV(ty,r,d)*)
         | stm _ (T.EXT(ASSIGN(AGG(ty,_,
                       CELL(_,_,T.REXT(_,FORALL(T.REG(_,0))),_)),
              T.REXT(_,FETCH(AGG(_,_,
                       CELL(_,_,T.REXT(_,FORALL(T.REG(_,0))),_))))))) =
              T.COPY(ty,[],[])
         | stm _ (T.EXT(PAR(s,T.SEQ []))) = s
         | stm _ (T.EXT(PAR(T.SEQ [],s))) = s
         | stm _ s = s
       fun ccexp _ e = e
       fun NIL _ x = x
   in  #stm(Rewrite.rewrite{rexp=rexp,fexp=NIL,stm=stm,ccexp=ccexp}) rtl
   end

   (*
    * Create a uniq RTL 
    *)
    
   fun new(action) = 
   let val action = reduce action
       val attribs = A_PURE
   in  case action of
         T.COPY _ => action
       | _ => T.RTL{e=action,hash=ref(newHash()),attribs=attribs} : rtl
   end

   val COPY = T.COPY(0,[],[])
   val JMP  = T.JMP([],T.REG(0,0),[])

   (* Query functions *)
   fun can'tMoveUp rtl = true
   fun can'tMoveDown rtl = true
   fun hasSideEffect rtl = true

end
