functor PPCRewrite(Instr : PPCINSTR) = struct
  structure I = Instr
  structure C = I.C

  fun ea(NONE, _, _, _) = NONE
    | ea(e as SOME(I.Direct r), rs, rt, mapr : I.C.cell -> I.C.cell) = 
       if mapr r=rs then SOME(I.Direct rt) else e 
    | ea(e as SOME(I.FDirect r), rs, rt, mapr) = 
       if mapr r=rs then SOME(I.FDirect rt) else e 
    | ea(e as SOME(I.Displace{base, disp}), rs, rt, mapr) =
       if mapr base=rs then SOME(I.Displace{base=rt, disp=disp}) 
       else e 

  fun rewriteUse(mapr : I.C.cell -> I.C.cell, instr, rs, rt) = let
    fun rplac r = if mapr r=rs then rt else r
    fun rwOperand(opnd as I.RegOp r) = 
         if mapr r = rs then I.RegOp rt else opnd
      | rwOperand opnd = opnd
  in
    case instr
    of I.L {ld, rt, ra, d, mem} =>
        I.L{ld=ld, rt=rt, ra=rplac ra, d=rwOperand d, mem=mem}
     | I.LF {ld, ft, ra, d, mem} =>
        I.LF{ld=ld, ft=ft, ra=rplac ra, d=rwOperand d, mem=mem}
     | I.ST {st, rs, ra, d, mem} => 
	I.ST{st=st, rs=rplac rs, ra=rplac ra, d=rwOperand d, mem=mem}
     | I.STF {st, fs, ra, d, mem} => 
	I.STF{st=st, fs=fs, ra=rplac ra, d=rwOperand d, mem=mem}
     | I.UNARY {oper, rt, ra, Rc, OE} =>
	I.UNARY{oper=oper, rt=rt, ra=rplac ra, Rc=Rc, OE=OE}
     | I.ARITH{oper, rt, ra, rb, Rc, OE} => 
	I.ARITH{oper=oper, rt=rt, ra=rplac ra, rb=rplac rb, Rc=Rc, OE=OE}
     | I.ARITHI{oper, rt, ra, im} => 
	I.ARITHI{oper=oper, rt=rt, ra=rplac ra, im=rwOperand im}
     | I.ROTATE {oper, ra, rs, sh, mb, me} =>
	I.ROTATE{oper=oper, ra=ra, rs=rplac rs, sh=rplac sh, mb=mb, me=me}
     | I.ROTATEI {oper, ra, rs, sh, mb, me} =>
	I.ROTATEI{oper=oper, ra=ra, rs=rplac rs, sh=rwOperand sh, mb=mb, me=me}
     | I.COMPARE {cmp, bf, l, ra, rb} =>
	I.COMPARE{cmp=cmp, bf=bf, l=l, ra=rplac ra, rb=rwOperand rb}
     | I.MTSPR{rs, spr} => I.MTSPR{rs=rplac rs, spr=spr}
     | I.TW {to, ra, si} => I.TW{to=to, ra=rplac ra, si=rwOperand si}
     | I.TD {to, ra, si} => I.TD{to=to, ra=rplac ra, si=rwOperand si}
     | I.CALL {def, use=(r,f,c), mem} => 
          I.CALL{def=def, use=(map rplac r,f,c), mem=mem}
     | I.COPY{dst, src, impl, tmp} =>
	I.COPY{dst=dst, src=map rplac src, impl=impl, tmp=tmp}
     | I.FCOPY{dst, src, impl, tmp} =>
	I.FCOPY{dst=dst, src=src, impl=impl, tmp=ea(tmp, rs, rt, mapr)}
     | I.ANNOTATION{i,a} => 
         I.ANNOTATION{i=rewriteUse(mapr,i,rs,rt),
                        a=case a of
                           C.DEF_USE{cellkind=C.GP,defs,uses} =>
                             C.DEF_USE{cellkind=C.GP,uses=map rplac uses,
                                       defs=defs}
                          | _ => a}
     | _ => instr
  end

  fun rewriteDef(mapr : I.C.cell -> I.C.cell, instr, rs, rt) = let
    fun rplac r = if mapr r = rs then rt else r
  in
    case instr
    of I.L {ld, rt, ra, d, mem} =>
        I.L{ld=ld, rt=rplac rt, ra=ra, d=d, mem=mem}
     | I.UNARY {oper, rt, ra, Rc, OE} =>
	I.UNARY{oper=oper, rt=rplac rt, ra=ra, Rc=Rc, OE=OE}
     | I.ARITH {oper, rt, ra, rb, Rc, OE} =>
	I.ARITH{oper=oper, rt=rplac rt, ra=ra, rb=rb, Rc=Rc, OE=OE}
     | I.ARITHI {oper, rt, ra, im} =>
	I.ARITHI {oper=oper, rt=rplac rt, ra=ra, im=im}
     | I.ROTATE {oper, ra, rs, sh, mb, me} =>
	I.ROTATE {oper=oper, ra=rplac ra, rs=rs, sh=sh, mb=mb, me=me}
     | I.ROTATEI {oper, ra, rs, sh, mb, me} =>
	I.ROTATEI {oper=oper, ra=rplac ra, rs=rs, sh=sh, mb=mb, me=me}
     | I.MFSPR {rt, spr} => I.MFSPR{rt=rplac rt, spr=spr}
     | I.CALL {def=(r,f,c), use, mem} => 
        I.CALL{def=(map rplac r, f, c), use=use, mem=mem}
     | I.COPY {dst, src, impl, tmp} =>
	I.COPY{dst=map rplac dst, src=src, impl=impl, tmp=ea(tmp,rs,rt,mapr)}
     | I.ANNOTATION{i,a} => 
        I.ANNOTATION{i=rewriteDef(mapr,i,rs,rt),
                        a=case a of
                           C.DEF_USE{cellkind=C.GP,defs,uses} =>
                             C.DEF_USE{cellkind=C.GP,uses=uses,
                                       defs=map rplac defs}
                          | _ => a}
     | _ => instr
  end

  fun frewriteUse(mapr : I.C.cell -> I.C.cell, instr, fs, ft) = let
    fun rplac r = if mapr r = fs then ft else r
  in
    case instr
    of I.STF {st, fs, ra, d, mem} =>
         I.STF{st=st, fs=rplac fs, ra=ra, d=d, mem=mem}
     | I.CALL{def, use=(r,f,c), mem} => 
         I.CALL{def=def, use=(r, map rplac f, c), mem=mem}
     | I.FCOMPARE {cmp, bf, fa, fb} =>
	 I.FCOMPARE{cmp=cmp, bf=bf, fa=rplac fa, fb=rplac fb}
     | I.FUNARY {oper, ft, fb, Rc} =>
	 I.FUNARY{oper=oper, ft=ft, fb=rplac fb, Rc=Rc}
     | I.FARITH {oper, ft, fa, fb, Rc} =>
	 I.FARITH{oper=oper, ft=ft, fa=rplac fa, fb=rplac fb, Rc=Rc}
     | I.FARITH3 {oper, ft, fa, fb, fc, Rc} =>
	 I.FARITH3{oper=oper,ft=ft,fa=rplac fa, fb=rplac fb, fc=rplac fc,Rc=Rc}
     | I.FCOPY {dst, src, impl, tmp} =>
	 I.FCOPY{dst=dst, src=map rplac src, impl=impl, tmp=tmp}
     | I.ANNOTATION{i,a} => 
         I.ANNOTATION{i=frewriteUse(mapr,i,fs,ft),
                        a=case a of
                           C.DEF_USE{cellkind=C.FP,defs,uses} =>
                             C.DEF_USE{cellkind=C.FP,uses=map rplac uses,
                                       defs=defs}
                          | _ => a}
     | _ => instr
  end


  fun frewriteDef(mapr : I.C.cell -> I.C.cell, instr, fs, ft) = let
    fun rplac r = if mapr r = fs then ft else r
  in
    case instr
    of I.LF{ld, ft, ra, d, mem} =>
        I.LF{ld=ld, ft=rplac ft, ra=ra, d=d, mem=mem}
     | I.FUNARY {oper, ft, fb, Rc} =>
	I.FUNARY{oper=oper, ft=rplac ft, fb=fb, Rc=Rc}
     | I.FARITH{oper, ft, fa, fb, Rc} =>
	I.FARITH{oper=oper, ft=rplac ft, fa=fa, fb=fb, Rc=Rc}
     | I.FARITH3{oper, ft, fa, fb, fc, Rc} =>
	I.FARITH3{oper=oper, ft=rplac ft, fa=fa, fb=fb, fc=fc, Rc=Rc}
    (* CALL = BCLR {bo=ALWAYS, bf=0, bit=0, LK=true, labels=[] *)
     | I.CALL{def=(r,f,c), use, mem} => 
        I.CALL{def=(r, map rplac f, c), use=use, mem=mem}
     | I.FCOPY {dst, src, impl, tmp} =>
        I.FCOPY{dst=map rplac dst, src=src, impl=impl, tmp=ea(tmp,fs,ft,mapr)}
     | I.ANNOTATION{i,a} => 
        I.ANNOTATION{i=frewriteDef(mapr,i,fs,ft),
                        a=case a of
                           C.DEF_USE{cellkind=C.FP,defs,uses} =>
                             C.DEF_USE{cellkind=C.FP,uses=uses,
                                       defs=map rplac defs}
                          | _ => a}
     | _ => instr
  end
end

