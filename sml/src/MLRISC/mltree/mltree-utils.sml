(* 
 *  Common operations on MLTREE
 *
 * -- Allen 
 *)
functor MLTreeUtils (T : MLTREE) : MLTREE_UTILS =
struct

   structure T        = T
   structure Constant = T.Constant
   structure LabelExp = T.LabelExp
   structure Region   = T.Region
   structure B        = T.Basis
   structure W        = Word

   val w = W.fromInt

   type ('s,'r,'f,'c) hasher =
      {stm    : ('s,'r,'f,'c) T.stm -> word,
       rexp   : ('s,'r,'f,'c) T.rexp -> word,
       fexp   : ('s,'r,'f,'c) T.fexp -> word,
       ccexp  : ('s,'r,'f,'c) T.ccexp -> word,
       mlrisc : ('s,'r,'f,'c) T.mlrisc list -> word
      }    

   type ('s,'r,'f,'c) equality =
      { stm    : ('s,'r,'f,'c) T.stm * ('s,'r,'f,'c) T.stm -> bool,
        rexp   : ('s,'r,'f,'c) T.rexp * ('s,'r,'f,'c) T.rexp -> bool,
        fexp   : ('s,'r,'f,'c) T.fexp * ('s,'r,'f,'c) T.fexp -> bool,
        ccexp  : ('s,'r,'f,'c) T.ccexp * ('s,'r,'f,'c) T.ccexp -> bool,
        mlrisc : ('s,'r,'f,'c) T.mlrisc list * ('s,'r,'f,'c) T.mlrisc list -> bool
      } 

   type ('s,'r,'f,'c) prettyPrinter =
      { stm   : ('s,'r,'f,'c) T.stm -> string,
        rexp  : ('s,'r,'f,'c) T.rexp -> string,
        fexp  : ('s,'r,'f,'c) T.fexp -> string,
        ccexp : ('s,'r,'f,'c) T.ccexp -> string,
        mlrisc : ('s,'r,'f,'c) T.mlrisc list -> string,
        srcReg : T.ty * T.var -> string,
        dstReg : T.ty * T.var -> string
      }

   fun error msg = MLRiscErrorMsg.error("MLTreeUtils",msg)
   fun ws is = 
   let fun f([],h) = h
         | f(i::is,h) = f(is,w i+h)
   in  f(is,0w0) end

   (*
    * Hashing
    *)
   fun hash {stm=hashSext, rexp=hashRext, fexp=hashFext, ccexp=hashCCext} =
   let  
   fun hashLabel(Label.Label{id,...}) = w id

   fun hashCtrls ctrl = ws ctrl
   fun hashCtrl  ctrl = w ctrl
   fun hasher() = 
      {stm=hashStm, rexp=hashRexp, fexp=hashFexp, ccexp=hashCCexp,
       mlrisc=hashMlriscs}
   and hashStm stm =
      case stm of  
      T.MV(t,dst,rexp) => 0w123 + w t + w dst + hashRexp rexp
    | T.CCMV(dst,ccexp) => 0w1234 + w dst + hashCCexp ccexp
    | T.FMV(fty,dst,fexp) => 0w12345 + w fty + w dst + hashFexp fexp
    | T.COPY(ty,dst,src) => 0w234 + w ty + ws dst + ws src
    | T.FCOPY(fty,dst,src) => 0w456 + w fty + ws dst + ws src
    | T.JMP(ctrl,ea,labels) => 0w45 + hashRexp ea
    | T.CALL(ea,flow,defs,uses,cdefs,cuses,mem) =>
          hashRexp ea  + hashMlriscs defs + hashMlriscs uses + 
          hashCtrls cdefs + hashCtrls cuses
    | T.RET _ => 0w567
    | T.STORE(ty,ea,data,mem) => 0w888 + w ty + hashRexp ea + hashRexp data 
    | T.FSTORE(fty,ea,data,mem) => 0w7890 + w fty + hashRexp ea + hashFexp data
    | T.BCC(ctrl,a,lab) => 0w233 + hashCtrls ctrl + hashCCexp a + hashLabel lab
    | T.IF(ctrl,a,b,c) => 
         0w233 + hashCtrls ctrl + hashCCexp a + hashStm b + hashStm c
    | T.ANNOTATION(stm, a) => hashStm stm 
    | T.PHI block => w block
    | T.PINNED stm => 0w12312 + hashStm stm
    | T.REGION(stm,ctrl) => hashStm stm + hashCtrl ctrl
    | T.RTL{hash,...} => !hash
    | T.SEQ ss => hashStms(ss, 0w23)
    | _ => error "hashStm" 

   and hashStms([],h) = h
     | hashStms(s::ss,h) = hashStms(ss,hashStm s + h)

   and hashMlrisc(T.CCR ccexp) = hashCCexp ccexp
     | hashMlrisc(T.GPR rexp) = hashRexp rexp 
     | hashMlrisc(T.FPR fexp) = hashFexp fexp

   and hashMlriscs [] = 0w123
     | hashMlriscs(m::ms) = hashMlrisc m + hashMlriscs ms

   and hash2(ty,x,y) = w ty + hashRexp x + hashRexp y

   and hashRexp rexp =  
      case rexp of
      T.REG(ty, src) => w ty + w src
    | T.LI i => w i
    | T.LI32 w => W.fromLargeWord w
    | T.LI64 w => W.fromLargeWord(Word64.toLargeWord w)
    | T.LABEL le => LabelExp.hash le
    | T.CONST c => Constant.hash c
    | T.NEG(ty, x) => w ty + hashRexp x + 0w24
    | T.ADD x => hash2 x + 0w234
    | T.SUB x => hash2 x + 0w456
    | T.MULS x => hash2 x + 0w2131
    | T.DIVS x => hash2 x + 0w156
    | T.QUOTS x => hash2 x + 0w1565
    | T.REMS x => hash2 x + 0w231
    | T.MULU x => hash2 x + 0w123
    | T.DIVU x => hash2 x + 0w1234
    | T.REMU x => hash2 x + 0w211
    | T.NEGT(ty, x) => w ty + hashRexp x + 0w1224
    | T.ADDT x => hash2 x + 0w1219
    | T.SUBT x => hash2 x + 0w999
    | T.MULT x => hash2 x + 0w7887
    | T.DIVT x => hash2 x + 0w88884
    | T.QUOTT x => hash2 x + 0w8884
    | T.REMT x => hash2 x + 0w99
    | T.ANDB x => hash2 x + 0w12312
    | T.ORB x => hash2 x + 0w558
    | T.XORB x => hash2 x + 0w234
    | T.NOTB(ty, x) => w ty + hashRexp x  
    | T.SRA x => hash2 x + 0w874 
    | T.SRL x => hash2 x + 0w223
    | T.SLL x => hash2 x + 0w499
    | T.COND(ty,e,e1,e2) => w ty + hashCCexp e + hashRexp e1 + hashRexp e2
    | T.CVTI2I(ty, T.SIGN_EXTEND, ty', rexp) => 
        0w232 + w ty + w ty' + hashRexp rexp
    | T.CVTI2I(ty, T.ZERO_EXTEND, ty', rexp) => 
        0w737 + w ty + w ty' + hashRexp rexp
    | T.CVTF2I(ty, round, ty', fexp) => 
        w ty + B.hashRoundingMode round + w ty' + hashFexp fexp
    | T.LOAD(ty, ea, mem) => w ty + hashRexp ea + 0w342
    | T.LET(stm, rexp) => hashStm stm + hashRexp rexp
    | T.PRED(e, ctrl) => hashRexp e + hashCtrl ctrl
    | T.MARK(e, _) => hashRexp e
    | T.REXT(ty, rext) => w ty + hashRext (hasher()) rext

  and hashRexps([],h) = h 
    | hashRexps(e::es,h) = hashRexps(es,hashRexp e + h)

  and hash2'(ty,x,y) = w ty + hashFexp x + hashFexp y

  and hashFexp fexp =  
      case fexp of
      T.FREG(fty, src) => w fty + w src
    | T.FLOAD(fty, ea, mem) => w fty + hashRexp ea
    | T.FADD x => hash2' x + 0w123
    | T.FMUL x => hash2' x + 0w1234
    | T.FSUB x => hash2' x + 0w12345
    | T.FDIV x => hash2' x + 0w234
    | T.FCOPYSIGN x => hash2' x + 0w883
    | T.FCOND(fty,c,x,y) => w fty + hashCCexp c + hashFexp x + hashFexp y
    | T.FABS(fty, fexp) => w fty + hashFexp fexp + 0w2345
    | T.FNEG(fty, fexp) => w fty + hashFexp fexp + 0w23456
    | T.FSQRT(fty, fexp) => w fty + hashFexp fexp + 0w345
    | T.CVTI2F(fty, ty, rexp) => w fty + w ty + hashRexp rexp
    | T.CVTF2F(fty, fty', fexp) => w fty + hashFexp fexp + w fty' 
    | T.FMARK(e, _) => hashFexp e
    | T.FPRED(e, ctrl) => hashFexp e + hashCtrl ctrl
    | T.FEXT(fty, fext) => w fty + hashFext (hasher()) fext

  and hashFexps([],h) = h
    | hashFexps(e::es,h) = hashFexps(es,hashFexp e + h)

  and hashCCexp ccexp =
      case ccexp of
      T.CC(cc, src) => B.hashCond cc + w src
    | T.FCC(fcc, src) => B.hashFcond fcc + w src
    | T.CMP(ty, cond, x, y) => 
        w ty + B.hashCond cond + hashRexp x + hashRexp y
    | T.FCMP(fty, fcond, x, y) => 
        w fty + B.hashFcond fcond + hashFexp x + hashFexp y
    | T.NOT x => 0w2321 + hashCCexp x 
    | T.AND(x,y) => 0w2321 + hashCCexp x + hashCCexp y
    | T.OR(x,y) => 0w8721 + hashCCexp x + hashCCexp y
    | T.XOR(x,y) => 0w6178 + hashCCexp x + hashCCexp y
    | T.TRUE => 0w0
    | T.FALSE => 0w1232
    | T.CCMARK(e, _) => hashCCexp e
    | T.CCEXT(ty,ccext) => w ty + hashCCext (hasher()) ccext

  and hashCCexps([],h) = h
    | hashCCexps(e::es,h) = hashCCexps(es,hashCCexp e + h)

  in {stm=hashStm, rexp=hashRexp, fexp=hashFexp, ccexp=hashCCexp, 
      mlrisc=hashMlriscs}
  end

   (*
    * Equality
    *)

  fun equal{stm=eqSext, rexp=eqRext, fexp=eqFext, ccexp=eqCCext} = 
  let
  fun eqLabel(Label.Label{id=x,...},Label.Label{id=y,...}) = x=y 
  fun eqLabels([],[]) = true
    | eqLabels(a::b,c::d) = eqLabel(a,c) andalso eqLabels(b,d)
    | eqLabels _ = false

  (* statements *)
  fun equality() = {stm=eqStm, rexp=eqRexp, fexp=eqFexp, ccexp=eqCCexp,
                    mlrisc=eqMlriscs}
  and eqStm(T.MV(a,b,c),T.MV(d,e,f)) = b=e andalso a=d andalso eqRexp(c,f)
    | eqStm(T.CCMV(a,b),T.CCMV(c,d)) = a=c andalso eqCCexp(b,d)
    | eqStm(T.FMV(a,b,c),T.FMV(d,e,f)) = b=e andalso a=d andalso eqFexp(c,f)
    | eqStm(T.COPY x,T.COPY y) = x = y
    | eqStm(T.FCOPY x,T.FCOPY y) = x = y
    | eqStm(T.JMP(a,b,c),T.JMP(a',b',c')) = 
         a=a' andalso eqRexp(b,b') andalso eqLabels(c,c')
    | eqStm(T.CALL(a,_,b,c,_,_,_),T.CALL(d,_,e,f,_,_,_)) =  
         eqRexp(a,d) andalso eqMlriscs(b,e) andalso eqMlriscs(c,f)
    | eqStm(T.RET _,T.RET _) = true
    | eqStm(T.STORE(a,b,c,_),T.STORE(d,e,f,_)) = 
         a=d andalso eqRexp(b,e) andalso eqRexp(c,f)
    | eqStm(T.FSTORE(a,b,c,_),T.FSTORE(d,e,f,_)) =
         a=d andalso eqRexp(b,e) andalso eqFexp(c,f)
    | eqStm(T.ANNOTATION(s1, _),s2) = eqStm(s1,s2)
    | eqStm(s1,T.ANNOTATION(s2, _)) = eqStm(s1,s2)
    | eqStm(T.PHI x,T.PHI y) = x=y
    | eqStm(T.BCC(a,b,c),T.BCC(a',b',c')) = 
        a=a' andalso eqCCexp(b,b') andalso eqLabel(c,c')
    | eqStm(T.IF(a,b,c,d),T.IF(a',b',c',d')) = 
        a=a' andalso eqCCexp(b,b') andalso eqStm(c,c') andalso eqStm(d,d')
    | eqStm(T.PINNED x,T.PINNED y) = eqStm(x,y)
    | eqStm(T.RTL{hash=x,...},T.RTL{hash=y,...}) = x=y
    | eqStm(T.REGION(a,b),T.REGION(a',b')) = b = b' andalso eqStm(a,a')
    | eqStm(T.EXT a,T.EXT a') = eqSext (equality()) (a,a')
    | eqStm _ = false

  and eqStms([],[]) = true
    | eqStms(a::b,c::d) = eqStm(a,c) andalso eqStms(b,d)
    | eqStms _ = false

  and eqMlrisc(T.CCR a,T.CCR b) = eqCCexp(a,b)
    | eqMlrisc(T.GPR a,T.GPR b) = eqRexp(a,b)
    | eqMlrisc(T.FPR a,T.FPR b) = eqFexp(a,b)
    | eqMlrisc _ = false

  and eqMlriscs([],[]) = true
    | eqMlriscs(a::b,c::d) = eqMlrisc(a,c) andalso eqMlriscs(b,d)
    | eqMlriscs _ = false

  and eq2((a,b,c),(d,e,f)) = a=d andalso eqRexp(b,e) andalso eqRexp(c,f)
   
  and eqRexp(T.REG(a,b),T.REG(c,d)) = a=c andalso b=d
    | eqRexp(T.LI a,T.LI b) = a=b 
    | eqRexp(T.LI32 a,T.LI32 b) = a=b
    | eqRexp(T.LI64 a,T.LI64 b) = a=b
    | eqRexp(T.LABEL a,T.LABEL b) = LabelExp.==(a,b)
    | eqRexp(T.CONST a,T.CONST b) = Constant.==(a,b)
    | eqRexp(T.NEG(t,x),T.NEG(t',x')) = t = t' andalso eqRexp(x,x')
    | eqRexp(T.ADD x,T.ADD y) = eq2(x,y)
    | eqRexp(T.SUB x,T.SUB y) = eq2(x,y)
    | eqRexp(T.MULS x,T.MULS y) = eq2(x,y)
    | eqRexp(T.DIVS x,T.DIVS y) = eq2(x,y)
    | eqRexp(T.QUOTS x,T.QUOTS y) = eq2(x,y)
    | eqRexp(T.REMS x,T.REMS y) = eq2(x,y)
    | eqRexp(T.MULU x,T.MULU y) = eq2(x,y)
    | eqRexp(T.DIVU x,T.DIVU y) = eq2(x,y)
    | eqRexp(T.REMU x,T.REMU y) = eq2(x,y)
    | eqRexp(T.NEGT(t,x),T.NEGT(t',x')) = t = t' andalso eqRexp(x,x')
    | eqRexp(T.ADDT x,T.ADDT y) = eq2(x,y)
    | eqRexp(T.SUBT x,T.SUBT y) = eq2(x,y)
    | eqRexp(T.MULT x,T.MULT y) = eq2(x,y)
    | eqRexp(T.DIVT x,T.DIVT y) = eq2(x,y)
    | eqRexp(T.QUOTT x,T.QUOTT y) = eq2(x,y)
    | eqRexp(T.REMT x,T.REMT y) = eq2(x,y)
    | eqRexp(T.ANDB x,T.ANDB y) = eq2(x,y)
    | eqRexp(T.ORB x,T.ORB y) = eq2(x,y)
    | eqRexp(T.XORB x,T.XORB y) = eq2(x,y)
    | eqRexp(T.NOTB(a,b),T.NOTB(c,d)) = a=c andalso eqRexp(b,d)
    | eqRexp(T.SRA x,T.SRA y) = eq2(x,y)
    | eqRexp(T.SRL x,T.SRL y) = eq2(x,y)
    | eqRexp(T.SLL x,T.SLL y) = eq2(x,y)
    | eqRexp(T.COND(a,b,c,d),T.COND(e,f,g,h)) = 
         a=e andalso eqCCexp(b,f) andalso eqRexp(c,g) andalso eqRexp(d,h)
    | eqRexp(T.CVTI2I(a,b,c,d),T.CVTI2I(a',b',c',d')) = 
         a=a' andalso b=b' andalso c=c' andalso eqRexp(d,d')
    | eqRexp(T.CVTF2I(a,b,c,d),T.CVTF2I(e,f,g,h)) = 
         a=e andalso b=f andalso c=g andalso eqFexp(d,h)
    | eqRexp(T.LOAD(a,b,_),T.LOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqRexp(T.LET(a,b),T.LET(c,d)) = eqStm(a,c) andalso eqRexp(b,d)
    | eqRexp(T.MARK(a,_),b) = eqRexp(a,b)
    | eqRexp(a,T.MARK(b,_)) = eqRexp(a,b)
    | eqRexp(T.PRED(a,b),T.PRED(a',b')) = b = b' andalso eqRexp(a,a')
    | eqRexp(T.REXT(a,b),T.REXT(a',b')) =   
          a=a' andalso eqRext (equality()) (b,b') 
    | eqRexp _ = false

  and eqRexps([],[]) = true
    | eqRexps(a::b,c::d) = eqRexp(a,c) andalso eqRexps(b,d)
    | eqRexps _ = false

  and eq2'((a,b,c),(d,e,f)) = a=d andalso eqFexp(b,e) andalso eqFexp(c,f)
  and eq1'((a,b),(d,e)) = a=d andalso eqFexp(b,e) 

  and eqFexp(T.FREG x,T.FREG y) = x = y
    | eqFexp(T.FLOAD(a,b,_),T.FLOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqFexp(T.FADD x,T.FADD y) = eq2'(x,y) 
    | eqFexp(T.FMUL x,T.FMUL y) = eq2'(x,y)
    | eqFexp(T.FSUB x,T.FSUB y) = eq2'(x,y) 
    | eqFexp(T.FDIV x,T.FDIV y) = eq2'(x,y)
    | eqFexp(T.FCOPYSIGN x, T.FCOPYSIGN y) = eq2'(x,y)
    | eqFexp(T.FCOND(t,x,y,z), T.FCOND(t',x',y',z')) = 
        t=t' andalso eqCCexp(x,x') andalso eqFexp(y,y') andalso eqFexp(z,z')
    | eqFexp(T.FABS x,T.FABS y) = eq1'(x,y)
    | eqFexp(T.FNEG x,T.FNEG y) = eq1'(x,y)
    | eqFexp(T.FSQRT x,T.FSQRT y) = eq1'(x,y)
    | eqFexp(T.CVTI2F(a,b,c),T.CVTI2F(a',b',c')) = 
         a=a' andalso b=b' andalso eqRexp(c,c')
    | eqFexp(T.CVTF2F(a,b,c),T.CVTF2F(a',b',c')) = 
         a=a' andalso b=b' andalso eqFexp(c,c')
    | eqFexp(T.FEXT(a,f),T.FEXT(b,g)) = a=b andalso eqFext (equality()) (f,g) 
    | eqFexp(T.FMARK(a,_),b) = eqFexp(a,b)
    | eqFexp(a,T.FMARK(b,_)) = eqFexp(a,b)
    | eqFexp(T.FPRED(a,b),T.FPRED(a',b')) = b = b' andalso eqFexp(a,a')
    | eqFexp _ = false

  and eqFexps([],[]) = true
    | eqFexps(a::b,c::d) = eqFexp(a,c) andalso eqFexps(b,d)
    | eqFexps _ = false

  and eqCCexp(T.CC x,T.CC y) = x=y
    | eqCCexp(T.FCC x,T.FCC y) = x=y
    | eqCCexp(T.CMP(x,a,b,c),T.CMP(y,d,e,f)) = 
        a=d andalso eqRexp(b,e) andalso eqRexp(c,f) andalso x = y
    | eqCCexp(T.FCMP(x,a,b,c),T.FCMP(y,d,e,f)) =
        a=d andalso eqFexp(b,e) andalso eqFexp(c,f) andalso x = y
    | eqCCexp(T.NOT x, T.NOT y) = eqCCexp(x,y)
    | eqCCexp(T.AND x, T.AND y) = eqCCexp2(x,y)
    | eqCCexp(T.OR x,  T.OR y) = eqCCexp2(x,y)
    | eqCCexp(T.XOR x, T.XOR y) = eqCCexp2(x,y)
    | eqCCexp(T.CCMARK(a,_),b) = eqCCexp(a,b)
    | eqCCexp(a,T.CCMARK(b,_)) = eqCCexp(a,b)
    | eqCCexp(T.CCEXT(t,a),T.CCEXT(t',b)) = 
        t=t' andalso eqCCext (equality()) (a,b)
    | eqCCexp(T.TRUE, T.TRUE) = true
    | eqCCexp(T.FALSE, T.FALSE) = true
    | eqCCexp _ = false

  and eqCCexp2((x,y),(x',y')) = eqCCexp(x,x') andalso eqCCexp(y,y')

  and eqCCexps([],[]) = true
    | eqCCexps(a::b,c::d) = eqCCexp(a,c) andalso eqCCexps(b,d)
    | eqCCexps _ = false
  in  {stm=eqStm, rexp=eqRexp, fexp=eqFexp, ccexp=eqCCexp, mlrisc=eqMlriscs} 
  end

  (*
   * Pretty printing
   *)
  fun show {stm=showStm, rexp=showRexp, fexp=showFexp, ccexp=showCCexp} 
      (dst,src) =
  let fun ty t = "."^Int.toString t
      fun fty 32 = ".s"
        | fty 64 = ".d"
        | fty 128 = ".q"
        | fty t   = ty t

      fun reg(t,v) = "r"^Int.toString v^ty t
      fun freg(t,v) = "f"^Int.toString v^fty t
      fun ccreg v = "cc"^Int.toString v   
      fun ctrlreg v = "p"^Int.toString v   

      fun srcReg(t,v) = List.nth(src,v) handle _ => reg(t,v)
      fun srcFreg(t,v) = List.nth(src,v) handle _ => freg(t,v)
      fun srcCCreg v = List.nth(src,v) handle _ => ccreg v
      fun srcCtrlreg v = List.nth(src,v) handle _ => ctrlreg v

      fun dstReg(t,v) = List.nth(dst,v) handle _ => reg(t,v)
      fun dstFreg(t,v) = List.nth(dst,v) handle _ => freg(t,v)
      fun dstCCreg v =  List.nth(dst,v) handle _ => ccreg v
      fun dstCtrlreg v = List.nth(dst,v) handle _ => ctrlreg v

      fun listify f =
      let fun g(t,[]) = ""
            | g(t,[r]) = f(t,r)
            | g(t,r::rs) = f(t,r)^","^g(t,rs)
      in  g end

      fun listify' f =
      let fun g([]) = ""
            | g([r]) = f(r)
            | g(r::rs) = f(r)^","^g(rs)
      in  g end

      val srcRegs = listify srcReg 
      val dstRegs = listify dstReg 
      val srcFregs = listify srcFreg 
      val dstFregs = listify dstFreg 
      val srcCCregs = listify' srcCCreg 
      val dstCCregs = listify' dstCCreg 
      val srcCtrlregs = listify' srcCtrlreg 
      val dstCtrlregs = listify' dstCtrlreg 
      fun usectrl cr  = " ["^srcCtrlreg cr^"]"
      fun usectrls [] = ""
        | usectrls cr = " ["^srcCtrlregs cr^"]"
      fun defctrl cr  = ""^dstCtrlreg cr^" <- "
      fun defctrls [] = ""
        | defctrls cr = ""^dstCtrlregs cr^" <- "

      fun copy(t,dst,src) = dstRegs(t, dst)^" := "^srcRegs(t, src)
      fun fcopy(t,dst,src) = dstFregs(t, dst)^" := "^srcFregs(t, src)

      fun shower() = {stm=stm, rexp=rexp, fexp=fexp, ccexp=ccexp, 
                      mlrisc=mlriscs, dstReg=dstReg, srcReg=srcReg}
          (* pretty print a statement *)
      and stm(T.MV(t,dst,e)) = dstReg(t,dst)^" := "^rexp e
        | stm(T.CCMV(dst,e)) = dstCCreg dst^" := "^ccexp e
        | stm(T.FMV(fty,dst,e)) = dstFreg(fty,dst)^" := "^fexp e
        | stm(T.COPY(ty,dst,src)) = copy(ty,dst,src)
        | stm(T.FCOPY(fty,dst,src)) = fcopy(fty,dst,src)
        | stm(T.JMP(cr,ea,labels)) = defctrls cr^"jmp "^rexp ea
        | stm(T.CALL(ea,flow,defs,uses,cdef,cuse,mem)) = 
              defctrls cdef^"call "^rexp ea^usectrls cuse
        | stm(T.RET(cr,flow)) = defctrls cr^"ret"
        | stm(T.STORE(ty,ea,e,mem)) = store(ty,"",ea,mem,e)
        | stm(T.FSTORE(fty,ea,e,mem)) = fstore(fty,"",ea,mem,e)
        | stm(T.BCC(cr,a,lab)) = 
             defctrls cr^"bcc "^ccexp a^" "^Label.nameOf lab
        | stm(T.IF(cr,a,b,c)) = 
             defctrls cr^"if "^ccexp a^" then "^stm b^" else "^stm c
        | stm(T.SEQ []) = "skip"
        | stm(T.SEQ s) = stms(";",s)
        | stm(T.REGION(s,cr)) = stm s^usectrl cr
        | stm(T.ANNOTATION(s, a)) = stm s 
        | stm(T.PHI b) = "phi["^Int.toString b^"]"
        | stm(T.PINNED s) = "pinned "^stm s
        | stm(T.RTL{e,...}) = stm e
        | stm(T.EXT x) = showStm (shower()) x
        | stm _ = error "stm"

      and stms(sep,[]) = ""
        | stms(sep,[s]) = stm s
        | stms(sep,s::ss) = stm s^sep^stms(sep,ss)

          (* pretty print an expression  *)
      and rexp(T.REG(ty, src)) = srcReg(ty,src)
        | rexp(T.LI i) = Int.toString i
        | rexp(T.LI32 w) = Word32.toString w
        | rexp(T.LI64 w) = Word64.toString w
        | rexp(T.LABEL le) = LabelExp.toString le
        | rexp(T.CONST c) = Constant.toString c
        | rexp(T.NEG x) = one("neg",x)
        | rexp(T.ADD x) = two("add",x)
        | rexp(T.SUB x) = two("sub",x)
        | rexp(T.MULS x) = two("muls",x)
        | rexp(T.DIVS x) = two("divs",x)
        | rexp(T.QUOTS x) = two("quots",x)
        | rexp(T.REMS x) = two("rems",x)
        | rexp(T.MULU x) = two("mulu",x)
        | rexp(T.DIVU x) = two("divu",x)
        | rexp(T.REMU x) = two("remu",x)
        | rexp(T.NEGT x) = one("negt",x)
        | rexp(T.ADDT x) = two("addt",x)
        | rexp(T.SUBT x) = two("subt",x)
        | rexp(T.MULT x) = two("mult",x)
        | rexp(T.DIVT x) = two("divt",x)
        | rexp(T.QUOTT x) = two("quott",x)
        | rexp(T.REMT x) = two("remt",x)
        | rexp(T.ANDB x) = two("andb",x)
        | rexp(T.ORB x) = two("orb",x)
        | rexp(T.XORB x) = two("xorb",x)
        | rexp(T.NOTB x) = one("notb",x)
        | rexp(T.SRA x) = two("sra",x)
        | rexp(T.SRL x) = two("srl",x)
        | rexp(T.SLL x) = two("sll",x)
        | rexp(T.COND(t,cc,e1,e2)) = 
             "cond"^ty t^ccexp cc^"("^rexp e1^","^rexp e2^")"
        | rexp(T.CVTI2I(t, T.SIGN_EXTEND, t', e)) = "sx"^ty t^ty t'^" "^rexp e
        | rexp(T.CVTI2I(t, T.ZERO_EXTEND, t', e)) = "zx"^ty t^ty t'^" "^rexp e
        | rexp(T.CVTF2I(t, round, t', e)) = 
             "cvtf2i"^ty t^B.roundingModeToString round^fty t'^" "^fexp e
        | rexp(T.LOAD(ty, ea, mem)) = load(ty,"",ea,mem)
        | rexp(T.LET(s, e)) = stm s^";"^rexp e
        | rexp(T.PRED(e, cr)) = rexp e^usectrl cr
        | rexp(T.MARK(e, _)) = rexp e
        | rexp(T.REXT e) = showRexp (shower()) e

      and slices sc = listify' (fn {from,to} => rexp from^".."^rexp to) sc

          (* pretty print a real expression  *)
      and fexp(T.FREG f) = srcFreg f
        | fexp(T.FLOAD(fty, ea, mem)) = fload(fty,"",ea,mem)
        | fexp(T.FADD x) = two'("fadd",x)
        | fexp(T.FMUL x) = two'("fmul",x)
        | fexp(T.FSUB x) = two'("fsub",x)
        | fexp(T.FDIV x) = two'("fdiv",x)
        | fexp(T.FCOPYSIGN x) = two'("fcopysign",x)
        | fexp(T.FABS x) = one'("fabs",x)
        | fexp(T.FNEG x) = one'("fneg",x)
        | fexp(T.FSQRT x) = one'("fsqrt",x)
        | fexp(T.FCOND(t,cc,e1,e2)) = 
             "fcond"^fty t^ccexp cc^"("^fexp e1^","^fexp e2^")"
        | fexp(T.CVTI2F(t, t', e)) = "cvti2f"^ty t'^" "^rexp e
        | fexp(T.CVTF2F(t, t', e)) = "cvtf2f"^fty t^fty t'^" "^fexp e
        | fexp(T.FPRED(e, cr)) = fexp e^usectrl cr
        | fexp(T.FMARK(e, _)) = fexp e
        | fexp(T.FEXT e) = showFexp (shower()) e

      and ccexp(T.CC(cc,r)) = srcCCreg r^B.condToString cc
        | ccexp(T.FCC(fcc,r)) = srcCCreg r^B.fcondToString fcc
        | ccexp(T.CMP(t,cc,x,y)) = "cmp"^B.condToString cc^ty t^pair(x,y)
        | ccexp(T.FCMP(t,fcc,x,y)) = "fcmp"^B.fcondToString fcc^fty t^pair'(x,y)
        | ccexp(T.NOT x) = "not "^ccexp x
        | ccexp(T.AND(x,y)) = two''(" and ",x,y)
        | ccexp(T.OR(x,y)) = two''(" or ",x,y)
        | ccexp(T.XOR(x,y)) = two''(" xor ",x,y)
        | ccexp(T.CCMARK(e, _)) = ccexp e
        | ccexp(T.TRUE) = "true"
        | ccexp(T.FALSE) = "false"
        | ccexp(T.CCEXT(e)) = showCCexp (shower()) e

      and mlrisc(T.GPR e) = rexp e
        | mlrisc(T.FPR e) = fexp e
        | mlrisc(T.CCR e) = ccexp e

      and mlriscs l = listify' mlrisc l

      (* Auxiliary functions *)
      and one(opcode,(t,x)) = opcode^ty t^"("^rexp x^")"
      and two(opcode,(t,x,y)) = opcode^ty t^pair(x,y)
      and pair(x,y) = "("^rexp x^","^rexp y^")"
      and one'(opcode,(t,x)) = opcode^fty t^"("^fexp x^")"
      and two'(opcode,(t,x,y)) = opcode^fty t^pair'(x,y)
      and two''(c,x,y) = "("^ccexp x^ c ^ ccexp y^")"
      and pair'(x,y) = "("^fexp x^","^fexp y^")"
      and rexps es = "("^foldr (fn (e,"") => rexp e
                                 | (e,x) => rexp e^","^x) "" es^")"
      and fexps es = "("^foldr (fn (e,"") => fexp e
                                 | (e,x) => fexp e^","^x) "" es^")"
      and ccexps es = "("^foldr (fn (e,"") => ccexp e
                                  | (e,x) => ccexp e^","^x) "" es^")"
      and store(t,u,ea,m,e) = mem(t,u,ea,m)^" := "^rexp e
      and fstore(t,u,ea,m,e) = fmem(t,u,ea,m)^" := "^fexp e
      and ccstore(u,ea,m,e) = ccmem(u,ea,m)^" := "^ccexp e
      and load(t,u,ea,m) = mem(t,u,ea,m)
      and fload(t,u,ea,m) = fmem(t,u,ea,m)
      and ccload(u,ea,m) = ccmem(u,ea,m)
      and addr(u,ea,m) = 
          let val r = Region.toString m
              val r = if r = "" then r else ","^r
          in  u^"["^rexp ea^r^"]" end
      and mem(t,u,ea,m) = "mem"^ty t^addr(u,ea,m)
      and fmem(t,u,ea,m) = "mem"^fty t^addr(u,ea,m)
      and ccmem(u,ea,m) = "mem"^addr(u,ea,m)

   in shower()
   end

   fun noshow _ = error "no extension"
   val noshows = {stm=noshow, rexp=noshow, fexp=noshow, ccexp=noshow}
   fun stmToString s   = #stm(show noshows ([],[])) s
   fun rexpToString s  = #rexp(show noshows ([],[])) s
   fun fexpToString s  = #fexp(show noshows ([],[])) s
   fun ccexpToString s = #ccexp(show noshows ([],[])) s

end 
