(* 
 *  Common operations on MLTREE
 *
 * -- Allen 
 *)
functor MLTreeUtilsFn
   (structure T : MLTREE
    val hashRext : T.rextension -> word
    val hashFext : T.fextension -> word
    val eqRext : T.rextension * T.rextension -> bool
    val eqFext : T.fextension * T.fextension -> bool
    val rextToString : T.rextension -> string
    val fextToString : T.fextension -> string
   ) : MLTREE_UTILS =
struct

   structure T        = T
   structure U        = T.Util
   structure Constant = T.Constant
   structure Region   = T.Region
   structure W        = Word

   val w = W.fromInt

   fun error msg = MLRiscErrorMsg.error("MLTreeUtils",msg)
   fun ws is = 
   let fun f([],h) = h
         | f(i::is,h) = f(is,w i+h)
   in  f(is,0w0) end

   (*
    * Hashing
    *)
   fun hashStm stm =
      case stm of  
      T.MV(t,dst,rexp) => 0w123 + w t + w dst + hashRexp rexp
    | T.CCMV(dst,ccexp) => 0w1234 + w dst + hashCCexp ccexp
    | T.FMV(fty,dst,fexp) => 0w12345 + w fty + w dst + hashFexp fexp
    | T.COPY(ty,dst,src) => 0w234 + w ty + ws dst + ws src
    | T.FCOPY(fty,dst,src) => 0w456 + w fty + ws dst + ws src
    | T.JMP(ea,labels) => 0w45 + hashRexp ea
    | T.CALL(ea,defs,uses,mem) =>
          hashRexp ea  + hashMlriscs defs + hashMlriscs uses
    | T.RET => 0w567
    | T.STORE(ty,ea,data,mem) => 
         0w888 + w ty + hashRexp ea + hashRexp data 
    | T.STORE_UNALIGNED(ty,ea,data,mem) => 
         0w999 + w ty + hashRexp ea + hashRexp data
    | T.FSTORE(fty,ea,data,mem) => 
         0w7890 + w fty + hashRexp ea + hashFexp data
    | T.FSTORE_UNALIGNED(fty,ea,data,mem) => 
         0w8901 + w fty + hashRexp ea + hashFexp data
    | T.BCC(cond,ccexp,lab) => U.hashCond cond + hashCCexp ccexp 
    | T.FBCC(fcond,ccexp,lab) => U.hashFcond fcond + hashCCexp ccexp
    | T.ANNOTATION(stm, a) => hashStm stm 
    | T.RTL(ref h,_,_) => h
    | T.RTLPHI block => w block
    | T.RTLPINNED stm => 0w12312 + hashStm stm
    | T.RTLPAR stms => foldr (fn (s,h) => hashStm s + h) 0w0 stms

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
    | T.ADD x => hash2 x + 0w234
    | T.SUB x => hash2 x + 0w456
    | T.MULS x => hash2 x + 0w2131
    | T.DIVS x => hash2 x + 0w156
    | T.REMS x => hash2 x + 0w231
    | T.MULU x => hash2 x + 0w123
    | T.DIVU x => hash2 x + 0w1234
    | T.REMU x => hash2 x + 0w211
    | T.ADDT x => hash2 x + 0w1219
    | T.SUBT x => hash2 x + 0w999
    | T.MULT x => hash2 x + 0w7887
    | T.DIVT x => hash2 x + 0w88884
    | T.REMT x => hash2 x + 0w99
    | T.ANDB x => hash2 x + 0w12312
    | T.ORB x => hash2 x + 0w558
    | T.XORB x => hash2 x + 0w234
    | T.NOTB(ty, x) => w ty + hashRexp x  
    | T.SRA x => hash2 x + 0w874 
    | T.SRL x => hash2 x + 0w223
    | T.SLL x => hash2 x + 0w499
    | T.COND(ty,e,e1,e2) => w ty + hashCCexp e + hashRexp e1 + hashRexp e2
    | T.CVTI2I(ty, ext, ty', rexp) => 
        w ty + U.hashExt ext + w ty' + hashRexp rexp
    | T.CVTF2I(ty, round, ty', fexp) => 
        w ty + U.hashRoundingMode round + w ty' + hashFexp fexp
    | T.LOAD(ty, ea, mem) => w ty + hashRexp ea + 0w342
    | T.LOAD_UNALIGNED(ty, ea, mem) => w ty + hashRexp ea + 0w3811
    | T.SEQ(stm, rexp) => hashStm stm + hashRexp rexp
    | T.MARK(e, _) => hashRexp e
    | T.EXT(ty, rextension, args) => 
          w ty + hashRexps(args,hashRext rextension) 
    | T.RTLPC => 0w7
    | T.RTLMISC(ref{hash,...},args) => hashRexps(args,hash)

  and hashRexps([],h) = h 
    | hashRexps(e::es,h) = hashRexps(es,hashRexp e + h)

  and hash2'(ty,x,y) = w ty + hashFexp x + hashFexp y

  and hashFexp fexp =  
      case fexp of
      T.FREG(fty, src) => w fty + w src
    | T.FLOAD(fty, ea, mem) => w fty + hashRexp ea
    | T.FLOAD_UNALIGNED(fty, ea, mem) => w fty + hashRexp ea
    | T.FADD x => hash2' x + 0w123
    | T.FMUL x => hash2' x + 0w1234
    | T.FSUB x => hash2' x + 0w12345
    | T.FDIV x => hash2' x + 0w234
    | T.FABS(fty, fexp) => w fty + hashFexp fexp + 0w2345
    | T.FNEG(fty, fexp) => w fty + hashFexp fexp + 0w23456
    | T.FSQRT(fty, fexp) => w fty + hashFexp fexp + 0w345
    | T.CVTI2F(fty, ext, ty, rexp) => 
        w fty + U.hashExt ext + w ty + hashRexp rexp
    | T.CVTF2F(fty, r, fty', fexp) => 
        w fty + hashFexp fexp + w fty' + U.hashRoundingMode r 
    | T.FSEQ(stm, fexp) => hashStm stm + hashFexp fexp
    | T.FMARK(e, _) => hashFexp e
    | T.FEXT(fty, fextension, args) => 
         w fty + hashFexps(args,hashFext fextension)
    | T.RTLFMISC(ref{hash,...},args) => hashFexps(args,hash)

  and hashFexps([],h) = h
    | hashFexps(e::es,h) = hashFexps(es,hashFexp e + h)

  and hashCCexp ccexp =
      case ccexp of
      T.CC src => w src
    | T.CMP(ty, cond, x, y) => 
        w ty + U.hashCond cond + hashRexp x + hashRexp y
    | T.FCMP(fty, fcond, x, y) => 
        w fty + U.hashFcond fcond + hashFexp x + hashFexp y
    | T.RTLCCMISC(ref{hash,...},args) => hashCCexps(args,hash)
    | T.CCMARK(e, _) => hashCCexp e

  and hashCCexps([],h) = h
    | hashCCexps(e::es,h) = hashCCexps(es,hashCCexp e + h)

   (*
    * Equality
    *)

  fun equalLabel(Label.Label{id=x,...},Label.Label{id=y,...}) = x=y 
  fun equalLabels([],[]) = true
    | equalLabels(a::b,c::d) = equalLabel(a,c) andalso equalLabels(b,d)
    | equalLabels _ = false

  (* statements *)
  fun eqStm(T.MV(a,b,c),T.MV(d,e,f)) = b=e andalso a=d andalso eqRexp(c,f)
    | eqStm(T.CCMV(a,b),T.CCMV(c,d)) = a=c andalso eqCCexp(b,d)
    | eqStm(T.FMV(a,b,c),T.FMV(d,e,f)) = b=e andalso a=d andalso eqFexp(c,f)
    | eqStm(T.COPY x,T.COPY y) = x = y
    | eqStm(T.FCOPY x,T.FCOPY y) = x = y
    | eqStm(T.JMP(a,b),T.JMP(c,d)) = eqRexp(a,c) andalso equalLabels(b,d)
    | eqStm(T.CALL(a,b,c,_),T.CALL(d,e,f,_)) =  
         eqRexp(a,d) andalso eqMlriscs(b,e) andalso eqMlriscs(c,f)
    | eqStm(T.RET,T.RET) = true
    | eqStm(T.STORE(a,b,c,_),T.STORE(d,e,f,_)) = 
         a=d andalso eqRexp(b,e) andalso eqRexp(c,f)
    | eqStm(T.STORE_UNALIGNED(a,b,c,_),T.STORE_UNALIGNED(d,e,f,_)) = 
         a=d andalso eqRexp(b,e) andalso eqRexp(c,f)
    | eqStm(T.FSTORE(a,b,c,_),T.FSTORE(d,e,f,_)) =
         a=d andalso eqRexp(b,e) andalso eqFexp(c,f)
    | eqStm(T.FSTORE_UNALIGNED(a,b,c,_),T.FSTORE_UNALIGNED(d,e,f,_)) =
         a=d andalso eqRexp(b,e) andalso eqFexp(c,f)
    | eqStm(T.BCC(a,b,c),T.BCC(d,e,f)) = 
         a=d andalso eqCCexp(b,e) andalso equalLabel(c,f)
    | eqStm(T.FBCC(a,b,c),T.FBCC(d,e,f)) = 
         a=d andalso eqCCexp(b,e) andalso equalLabel(c,f)
    | eqStm(T.ANNOTATION(s1, _),s2) = eqStm(s1,s2)
    | eqStm(s1,T.ANNOTATION(s2, _)) = eqStm(s1,s2)
    | eqStm(T.RTL(r,_,_),T.RTL(s,_,_)) = r=s
    | eqStm(T.RTLPHI x,T.RTLPHI y) = x=y
    | eqStm(T.RTLPINNED x,T.RTLPINNED y) = eqStm(x,y)
    | eqStm(T.RTLPAR x,T.RTLPAR y) = eqStms(x,y)
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
    | eqRexp(T.ADD x,T.ADD y) = eq2(x,y)
    | eqRexp(T.SUB x,T.SUB y) = eq2(x,y)
    | eqRexp(T.MULS x,T.MULS y) = eq2(x,y)
    | eqRexp(T.DIVS x,T.DIVS y) = eq2(x,y)
    | eqRexp(T.REMS x,T.REMS y) = eq2(x,y)
    | eqRexp(T.MULU x,T.MULU y) = eq2(x,y)
    | eqRexp(T.DIVU x,T.DIVU y) = eq2(x,y)
    | eqRexp(T.REMU x,T.REMU y) = eq2(x,y)
    | eqRexp(T.ADDT x,T.ADDT y) = eq2(x,y)
    | eqRexp(T.SUBT x,T.SUBT y) = eq2(x,y)
    | eqRexp(T.MULT x,T.MULT y) = eq2(x,y)
    | eqRexp(T.DIVT x,T.DIVT y) = eq2(x,y)
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
    | eqRexp(T.CVTI2I(a,b,c,d),T.CVTI2I(e,f,g,h)) = 
         a=e andalso b=f andalso c=g andalso eqRexp(d,h)
    | eqRexp(T.CVTF2I(a,b,c,d),T.CVTF2I(e,f,g,h)) = 
         a=e andalso b=f andalso c=g andalso eqFexp(d,h)
    | eqRexp(T.LOAD(a,b,_),T.LOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqRexp(T.LOAD_UNALIGNED(a,b,_),T.LOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqRexp(T.SEQ(a,b),T.SEQ(c,d)) = eqStm(a,c) andalso eqRexp(b,d)
    | eqRexp(T.MARK(a,_),b) = eqRexp(a,b)
    | eqRexp(a,T.MARK(b,_)) = eqRexp(a,b)
    | eqRexp(T.EXT(a,f,es1),T.EXT(b,g,es2)) =   
          a=b andalso eqRext(f,g) andalso eqRexps(es1,es2)
    | eqRexp(T.RTLPC,T.RTLPC) = true
    | eqRexp(T.RTLMISC(a,b),T.RTLMISC(c,d)) = a=c andalso eqRexps(b,d)
    | eqRexp _ = false

  and eqRexps([],[]) = true
    | eqRexps(a::b,c::d) = eqRexp(a,c) andalso eqRexps(b,d)
    | eqRexps _ = false

  and eq2'((a,b,c),(d,e,f)) = a=d andalso eqFexp(b,e) andalso eqFexp(c,f)
  and eq1'((a,b),(d,e)) = a=d andalso eqFexp(b,e) 

  and eqFexp(T.FREG x,T.FREG y) = x = y
    | eqFexp(T.FLOAD(a,b,_),T.FLOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqFexp(T.FLOAD_UNALIGNED(a,b,_),T.FLOAD(c,d,_)) = a=c andalso eqRexp(b,d)
    | eqFexp(T.FADD x,T.FADD y) = eq2'(x,y) 
    | eqFexp(T.FMUL x,T.FMUL y) = eq2'(x,y)
    | eqFexp(T.FSUB x,T.FSUB y) = eq2'(x,y) 
    | eqFexp(T.FDIV x,T.FDIV y) = eq2'(x,y)
    | eqFexp(T.FABS x,T.FABS y) = eq1'(x,y)
    | eqFexp(T.FNEG x,T.FNEG y) = eq1'(x,y)
    | eqFexp(T.FSQRT x,T.FSQRT y) = eq1'(x,y)
    | eqFexp(T.CVTI2F(a,b,c,d),T.CVTI2F(e,f,g,h)) = 
         a=e andalso b=f andalso c=g andalso eqRexp(d,h)
    | eqFexp(T.CVTF2F(a,b,c,d),T.CVTF2F(e,f,g,h)) = 
         a=e andalso b=f andalso c=g andalso eqFexp(d,h)
    | eqFexp(T.FSEQ(a,b),T.FSEQ(c,d)) = eqStm(a,c) andalso eqFexp(b,d)
    | eqFexp(T.FMARK(a,_),b) = eqFexp(a,b)
    | eqFexp(a,T.FMARK(b,_)) = eqFexp(a,b)
    | eqFexp(T.FEXT(a,f,es1),T.FEXT(b,g,es2)) =
         a=b andalso eqFext(f,g) andalso eqFexps(es1,es2)
    | eqFexp(T.RTLFMISC(a,b),T.RTLFMISC(c,d)) = a=c andalso eqFexps(b,d)
    | eqFexp _ = false

  and eqFexps([],[]) = true
    | eqFexps(a::b,c::d) = eqFexp(a,c) andalso eqFexps(b,d)
    | eqFexps _ = false

  and eqCCexp(T.CC x,T.CC y) = x=y
    | eqCCexp(T.CMP(x,a,b,c),T.CMP(y,d,e,f)) = 
        a=d andalso eqRexp(b,e) andalso eqRexp(c,f) andalso x = y
    | eqCCexp(T.FCMP(x,a,b,c),T.FCMP(y,d,e,f)) =
        a=d andalso eqFexp(b,e) andalso eqFexp(c,f) andalso x = y
    | eqCCexp(T.RTLCCMISC(a,b),T.RTLCCMISC(c,d)) = a=c andalso eqCCexps(b,d)
    | eqCCexp(T.CCMARK(a,_),b) = eqCCexp(a,b)
    | eqCCexp(a,T.CCMARK(b,_)) = eqCCexp(a,b)
    | eqCCexp _ = false

  and eqCCexps([],[]) = true
    | eqCCexps(a::b,c::d) = eqCCexp(a,c) andalso eqCCexps(b,d)
    | eqCCexps _ = false
  (*
   * Pretty printing
   *)
  fun prettyPrinting (dst,src) s =
  let fun ty t = "."^Int.toString t
      fun fty 32 = ".s"
        | fty 64 = ".d"
        | fty 128 = ".q"
        | fty t   = ty t

      fun reg(t,v) = "r"^Int.toString v^ty t
      fun freg(t,v) = "f"^Int.toString v^fty t
      fun ccreg v = "cc"^Int.toString v   
  
      fun srcReg(t,v) = List.nth(src,v) handle _ => reg(t,v)
      fun srcFreg(t,v) = List.nth(src,v) handle _ => freg(t,v)
      fun srcCCreg v = List.nth(src,v) handle _ => ccreg v

      fun dstReg(t,v) = List.nth(dst,v) handle _ => reg(t,v)
      fun dstFreg(t,v) = List.nth(dst,v) handle _ => freg(t,v)
      fun dstCCreg v =  List.nth(dst,v) handle _ => ccreg v

      fun srcRegs(t,[]) = ""
        | srcRegs(t,[r]) = srcReg(t,r)
        | srcRegs(t,r::rs) = srcReg(t,r)^","^srcRegs(t,rs)

      fun dstRegs(t,[]) = ""
        | dstRegs(t,[r]) = dstReg(t,r)
        | dstRegs(t,r::rs) = dstReg(t,r)^","^dstRegs(t,rs)

      fun srcFregs(t,[]) = ""
        | srcFregs(t,[f]) = srcFreg(t,f)
        | srcFregs(t,f::fs) = srcFreg(t,f)^","^srcFregs(t,fs)

      fun dstFregs(t,[]) = ""
        | dstFregs(t,[f]) = dstFreg(t,f)
        | dstFregs(t,f::fs) = dstFreg(t,f)^","^dstFregs(t,fs)

      fun copy(t,dst,src) = dstRegs(t, dst)^" := ^"^srcRegs(t, src)
      fun fcopy(t,dst,src) = dstFregs(t, dst)^" := "^srcFregs(t, src)

      val unaligned = "u"

          (* pretty print a statement *)
      fun stm(T.MV(t,dst,e)) = dstReg(t,dst)^" := "^rexp e
        | stm(T.CCMV(dst,e)) = dstCCreg dst^" := "^ccexp e
        | stm(T.FMV(fty,dst,e)) = dstFreg(fty,dst)^" := "^fexp e
        | stm(T.COPY(ty,dst,src)) = copy(ty,dst,src)
        | stm(T.FCOPY(fty,dst,src)) = fcopy(fty,dst,src)
        | stm(T.JMP(ea,labels)) = "jmp "^rexp ea
        | stm(T.CALL(ea,defs,uses,mem)) = "call "^rexp ea
        | stm T.RET = "ret"
        | stm(T.STORE(ty,ea,e,mem)) = store(ty,"",ea,mem,e)
        | stm(T.STORE_UNALIGNED(ty,ea,e,mem)) = store(ty,unaligned,ea,mem,e)
        | stm(T.FSTORE(fty,ea,e,mem)) = fstore(fty,"",ea,mem,e)
        | stm(T.FSTORE_UNALIGNED(fty,ea,e,mem)) = fstore(fty,unaligned,ea,mem,e)
        | stm(T.BCC(cond,e,lab)) = branch("b"^U.condToString cond,e,lab)
        | stm(T.FBCC(fcond,e,lab)) = branch("fb"^U.fcondToString fcond,e,lab)
        | stm(T.ANNOTATION(s, a)) = stm s 
        | stm(T.RTL(_,_,s)) = stm s
        | stm(T.RTLPHI b) = "phi["^Int.toString b^"]"
        | stm(T.RTLPINNED s) = "pinned "^stm s
        | stm(T.RTLPAR ss) = stms("||",ss)

      and stms(sep,[]) = ""
        | stms(sep,[s]) = stm s
        | stms(sep,s::ss) = stm s^sep^stms(sep,ss)

          (* pretty print an expression  *)
      and rexp(T.REG(ty, src)) = srcReg(ty,src)
        | rexp(T.LI i) = (List.nth(src,i) handle _ => Int.toString i)
        | rexp(T.LI32 w) = Word32.toString w
        | rexp(T.LI64 w) = Word64.toString w
        | rexp(T.LABEL le) = LabelExp.toString le
        | rexp(T.CONST c) = Constant.toString c
        | rexp(T.ADD x) = two("add",x)
        | rexp(T.SUB x) = two("sub",x)
        | rexp(T.MULS x) = two("muls",x)
        | rexp(T.DIVS x) = two("divs",x)
        | rexp(T.REMS x) = two("rems",x)
        | rexp(T.MULU x) = two("mulu",x)
        | rexp(T.DIVU x) = two("divu",x)
        | rexp(T.REMU x) = two("remu",x)
        | rexp(T.ADDT x) = two("addt",x)
        | rexp(T.SUBT x) = two("subt",x)
        | rexp(T.MULT x) = two("mult",x)
        | rexp(T.DIVT x) = two("divt",x)
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
        | rexp(T.CVTI2I(t, ext, t', e)) = 
             "cvti2i"^ty t^U.extToString ext^ty t'^" "^rexp e
        | rexp(T.CVTF2I(t, round, t', e)) = 
             "cvtf2i"^ty t^U.roundingModeToString round^fty t'^" "^fexp e
        | rexp(T.LOAD(ty, ea, mem)) = load(ty,"",ea,mem)
        | rexp(T.LOAD_UNALIGNED(ty, ea, mem)) = load(ty,unaligned,ea,mem)
        | rexp(T.SEQ(s, e)) = stm s^";"^rexp e
        | rexp(T.MARK(e, _)) = rexp e
        | rexp(T.EXT(t, rext, args)) = rextToString rext^ty t^rexps args
        | rexp(T.RTLPC) = "PC"
        | rexp(T.RTLMISC(ref{name,...}, args)) = name^rexps args

          (* pretty print a real expression  *)
      and fexp(T.FREG f) = srcFreg f
        | fexp(T.FLOAD(fty, ea, mem)) = fload(fty,"",ea,mem)
        | fexp(T.FLOAD_UNALIGNED(fty, ea, mem)) = fload(fty,unaligned,ea,mem)
        | fexp(T.FADD x) = two'("fadd",x)
        | fexp(T.FMUL x) = two'("fmul",x)
        | fexp(T.FSUB x) = two'("fsub",x)
        | fexp(T.FDIV x) = two'("fdiv",x)
        | fexp(T.FABS x) = one'("fabs",x)
        | fexp(T.FNEG x) = one'("fneg",x)
        | fexp(T.FSQRT x) = one'("fsqrt",x)
        | fexp(T.CVTI2F(t, ext, t', e)) = 
             "cvti2f"^fty t^U.extToString ext^ty t'^" "^rexp e
        | fexp(T.CVTF2F(t, r, t', e)) = 
             "cvtf2f"^fty t^U.roundingModeToString r^fty t'^" "^fexp e
        | fexp(T.FSEQ(s, e)) = stm s^";"^fexp e
        | fexp(T.FMARK(e, _)) = fexp e
        | fexp(T.FEXT(t, fext, args)) = fextToString fext^fty t^fexps args
        | fexp(T.RTLFMISC(ref{name,...}, args)) = name^fexps args

      and ccexp(T.CC r) = srcCCreg r
        | ccexp(T.CMP(t,cond, x, y)) = 
            "cmp"^U.condToString cond^ty t^pair(x,y)
        | ccexp(T.FCMP(t,fcond, x, y)) = 
            "fcmp"^U.fcondToString fcond^fty t^pair'(x,y)
        | ccexp(T.CCMARK(e, _)) = ccexp e
        | ccexp(T.RTLCCMISC(ref{name,...}, args)) = name^ccexps args

      (* Auxiliary functions *)
      and one(opcode,(t,x)) = opcode^ty t^"("^rexp x^")"
      and two(opcode,(t,x,y)) = opcode^ty t^pair(x,y)
      and pair(x,y) = "("^rexp x^","^rexp y^")"
      and one'(opcode,(t,x)) = opcode^fty t^"("^fexp x^")"
      and two'(opcode,(t,x,y)) = opcode^fty t^pair'(x,y)
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
      and branch(c,e,lab) = c^" "^ccexp e^" "^Label.nameOf lab
      and load(t,u,ea,m) = mem(t,u,ea,m)
      and fload(t,u,ea,m) = fmem(t,u,ea,m)
      and ccload(u,ea,m) = ccmem(u,ea,m)
      and addr(u,ea,m) = u^"["^rexp ea^","^Region.toString m^"]"
      and mem(t,u,ea,m) = "mem"^ty t^addr(u,ea,m)
      and fmem(t,u,ea,m) = "mem"^fty t^addr(u,ea,m)
      and ccmem(u,ea,m) = "mem"^addr(u,ea,m)

   in stm s
   end

   fun toString s = prettyPrinting([],[]) s
   val toString' = prettyPrinting

end 
