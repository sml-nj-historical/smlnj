(* alphaRewrite.sml -- rewrite an alpha instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)

functor AlphaRewrite(Instr : ALPHAINSTR) = struct
  structure I=Instr
  structure C=I.C

  fun rewriteUse(instr, rs, rt) = let
    fun match r = C.sameColor(r,rs)
    fun isRegOp (I.REGop r) = match r
      | isRegOp _ = false
    fun rwOperand(opnd as I.REGop r) = 
         if match r then I.REGop rt else opnd
      | rwOperand opnd = opnd
    fun replace r = if match r then rt else r
    fun load(ldClass, {ldOp, r, b, d, mem}) =
       if match b
       then ldClass{ldOp=ldOp, r=r, b=rt, d=d, mem=mem}
       else instr
    fun fstore(stClass, {stOp, r, b, d, mem}) =
       if match b then stClass{stOp=stOp, r=r, b=rt, d=d, mem=mem}
       else instr
    fun store{stOp, r, b, d, mem} = 
      if match r then
	if match b then
	  I.STORE{stOp=stOp, r=rt, b=rt, d=d, mem=mem}
	else
	  I.STORE{stOp=stOp, r=rt, b=b, d=d, mem=mem}
      else if match b then
	I.STORE{stOp=stOp, r=r, b=rt, d=d, mem=mem}
      else instr
    fun operate(opClass, {oper, ra, rb, rc}) = 
      if match ra then
	if isRegOp rb then 
	  opClass{oper=oper, ra=rt, rb=I.REGop rt, rc=rc}
	else opClass{oper=oper, ra=rt, rb=rb, rc=rc}
      else if isRegOp rb then
	opClass{oper=oper, ra=ra, rb=I.REGop rt, rc=rc}
      else instr

  in
    case instr
    of I.LDA{r, b, d} => if match b then I.LDA{r=r, b=rt, d=d} else instr
     | I.LDAH{r, b, d} => if match b then I.LDAH{r=r, b=rt, d=d} else instr
     | I.LOAD arg => load(I.LOAD, arg)
     | I.FLOAD farg => load(I.FLOAD, farg)
     | I.STORE arg => store arg
     | I.FSTORE farg => fstore(I.FSTORE, farg)
     | I.JMPL({r, b, d}, labs) =>
       if match b then I.JMPL({r=r, b=rt, d=d}, labs) else instr
     | I.JSR{r, b, d, defs, uses, cutsTo, mem} =>
	 I.JSR{r=r, b=replace b, d=d, defs=defs, 
               uses=C.CellSet.map {from=rs,to=rt} uses, cutsTo=cutsTo, mem=mem}
     | I.BSR{r, lab, defs, uses, cutsTo, mem} =>
	 I.BSR{r=r, lab=lab, defs=defs, 
               uses=C.CellSet.map {from=rs,to=rt} uses, cutsTo=cutsTo, mem=mem}
     | I.RET{r,b,d} => I.RET{r=r, b=replace b, d=d}
     | I.BRANCH{b=I.BR, ...} => instr
     | I.BRANCH{b, r, lab} => if match r then I.BRANCH{b=b, r=rt, lab=lab} 
                              else instr
     | I.OPERATE arg => operate(I.OPERATE, arg)
     | I.OPERATEV arg => operate(I.OPERATEV, arg)
     | I.CMOVE{oper,ra,rb,rc} => 
         I.CMOVE{oper=oper,ra=replace ra,rb=rwOperand rb,rc=replace rc}
     | I.COPY{dst, src, tmp, impl} => 
	 I.COPY{dst=dst, src=map replace src, tmp=tmp, impl=impl}
     | I.CALL_PAL{code, def, use } => 
         I.CALL_PAL{code=code, def=def, use=C.CellSet.map {from=rs,to=rt} use}
     | I.PSEUDOARITH{oper, ra, rb, rc, tmps} => 
         I.PSEUDOARITH{oper=oper, ra=replace ra, rb=rwOperand rb, rc=rc,
                       tmps=tmps}
     | I.ANNOTATION{i,a} =>
           I.ANNOTATION{i=rewriteUse(i,rs,rt),
                        a=case a of 
                           C.DEF_USE{cellkind=C.GP,defs,uses} =>
                             C.DEF_USE{cellkind=C.GP,defs=defs,
                                       uses=map replace uses}
                          | _ => a}
     | _ => instr
  end

  fun frewriteUse(instr, fs, ft) = let
     fun match f = C.sameColor(f,fs)
    fun replace f = if match f then ft else f
    fun foperate(opClass, {oper, fa, fb, fc}) = 
      if match fa then 
	opClass{oper=oper, fa=ft, fc=fc, fb=replace fb}
      else if match fb then opClass{oper=oper, fa=fa, fb=ft, fc=fc}
      else instr
  in
    case instr
    of I.FBRANCH{b, f, lab} =>
       if match f then I.FBRANCH{b=b, f=ft, lab=lab} else instr
     | I.FCOPY{dst, src, impl, tmp} => 
	I.FCOPY{dst=dst, src=map(fn f => if match f then ft else f) src, 
		tmp=tmp, impl=impl}
     | I.FSTORE{stOp, r, b, d, mem} => 
	if match r then I.FSTORE{stOp=stOp, r=ft, b=b, d=d, mem=mem} else instr
     | I.FOPERATE arg => foperate(I.FOPERATE, arg)
     | I.FOPERATEV arg => foperate(I.FOPERATEV, arg)
     | I.FUNARY{oper,fb,fc} =>
        if match fb then I.FUNARY{oper=oper,fb=ft,fc=fc} else instr
     | I.FCMOVE{oper,fa,fb,fc} => 
         I.FCMOVE{oper=oper,fa=replace fa,fb=replace fb,fc=replace fc}
     | I.JSR{r, b, d, defs, uses, cutsTo, mem} => 
         I.JSR{r=r, b=b, d=d, defs=defs, 
               uses=C.CellSet.map {from=fs,to=ft} uses, cutsTo=cutsTo, mem=mem}
     | I.BSR{r, lab, defs, uses, cutsTo, mem} => 
         I.BSR{r=r, lab=lab, defs=defs, 
               uses=C.CellSet.map {from=fs,to=ft} uses, cutsTo=cutsTo, mem=mem}
     | I.ANNOTATION{i,a} => 
         I.ANNOTATION{i=frewriteUse(i,fs,ft),
                      a=case a of 
                         C.DEF_USE{cellkind=C.FP,defs,uses} =>
                           C.DEF_USE{cellkind=C.FP,defs=defs,
                                     uses=map replace uses}
                        | _ => a}

     | _ => instr
  end

  fun rewriteDef(instr, rs, rt) = let
    fun match r = C.sameColor(r,rs)
    fun rewrite r = if match r then rt else r
    fun ea (SOME(I.Direct r)) = SOME(I.Direct (rewrite r))
      | ea x = x
  in
    case instr
    of I.LDA{r, b, d} => if match r then I.LDA{r=rt, b=b, d=d} else instr
     | I.LDAH{r, b, d} => if match r then I.LDAH{r=rt, b=b, d=d} else instr
     | I.LOAD{ldOp, r, b, d, mem} => 
       if match r then I.LOAD{ldOp=ldOp, r=rt, b=b, d=d, mem=mem} else instr
     | I.JMPL({r, b, d}, labs) =>
       if match r then I.JMPL({r=rt, b=b, d=d}, labs) else instr
     | I.JSR{r, b, d, defs, uses, cutsTo, mem} =>
         I.JSR{r=rewrite r, b=b, d=d, defs=C.CellSet.map {from=rs,to=rt} defs, 
               uses=uses, cutsTo=cutsTo, mem=mem}
     | I.BSR{r, lab, defs, uses, cutsTo, mem} =>
         I.BSR{r=rewrite r, lab=lab, defs=C.CellSet.map {from=rs,to=rt} defs, 
               uses=uses, cutsTo=cutsTo, mem=mem}
     | I.RET{r, b, d} => I.RET{r=rewrite r, b=b, d=d}
     | I.BRANCH{b=I.BR, r, lab} => 
       if match r then I.BRANCH{b=I.BR, r=rt, lab=lab} else instr
     | I.OPERATE{oper, ra, rb, rc} => 
       if match rc then I.OPERATE{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.OPERATEV{oper, ra, rb, rc} =>
       if match rc then I.OPERATEV{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.CMOVE{oper,ra,rb,rc} => I.CMOVE{oper=oper,ra=ra,rb=rb,rc=rewrite rc}
     | I.COPY{dst, src, impl, tmp} =>
	I.COPY{dst=map rewrite dst, src=src, tmp=ea tmp, impl=impl}
     | I.CALL_PAL{code, def, use} => 
         I.CALL_PAL{code=code, def=C.CellSet.map {from=rs,to=rt} def, use=use}
     | I.PSEUDOARITH{oper, ra, rb, rc, tmps} => 
         I.PSEUDOARITH{oper=oper, ra=ra, rb=rb, rc=rewrite rc,
                       tmps=C.CellSet.map {from=rs,to=rt} tmps}
     | I.ANNOTATION{i,a} => 
         I.ANNOTATION{i=rewriteDef(i,rs,rt),
                        a=case a of 
                           C.DEF_USE{cellkind=C.GP,defs,uses} =>
                             C.DEF_USE{cellkind=C.GP,uses=uses,
                                       defs=map rewrite defs}
                          | _ => a}
     | _ => instr
  end

  fun frewriteDef(instr, fs, ft) = let
    fun match f = C.sameColor(f,fs)
    fun rewrite f = if match f then ft else f
    fun ea (SOME(I.FDirect f)) = SOME(I.FDirect(rewrite f))
      | ea x  = x
  in
    case instr
    of I.DEFFREG f => if match f then I.DEFFREG ft else instr
     | I.FLOAD{ldOp, r, b, d, mem} => 
        if match r then I.FLOAD{ldOp=ldOp, r=ft, b=b, d=d, mem=mem} else instr
     | I.FOPERATE{oper, fa, fb, fc} =>
	if match fc then I.FOPERATE{oper=oper, fa=fa, fb=fb, fc=ft} else instr
     | I.FOPERATEV{oper, fa, fb, fc} =>
	if match fc then I.FOPERATEV{oper=oper, fa=fa, fb=fb, fc=ft} else instr
     | I.FUNARY{oper,fb,fc} =>
        if match fc then I.FUNARY{oper=oper,fb=fb,fc=ft} else instr
     | I.FCOPY{dst, src, tmp, impl} =>
	I.FCOPY{dst=map rewrite dst, src=src, tmp=ea tmp, impl=impl} 
     | I.FCMOVE{oper,fa,fb,fc} => I.FCMOVE{oper=oper,fa=fa,fb=fb,fc=rewrite fc}
     | I.JSR{r, b, d, defs, uses, cutsTo, mem} => 
        I.JSR{r=r, b=b, d=d, defs=C.CellSet.map {from=fs,to=ft} defs, 
              uses=uses, cutsTo=cutsTo, mem=mem}
     | I.BSR{r, lab, defs, uses, cutsTo, mem} => 
        I.BSR{r=r, lab=lab, defs=C.CellSet.map {from=fs,to=ft} defs, 
              uses=uses, cutsTo=cutsTo, mem=mem}
     | I.PSEUDOARITH{oper, ra, rb, rc, tmps} => 
         I.PSEUDOARITH{oper=oper, ra=ra, rb=rb, rc=rc, 
                       tmps=C.CellSet.map {from=fs,to=ft} tmps}
     | I.ANNOTATION{i,a} => 
         I.ANNOTATION{i=frewriteDef(i,fs,ft),
                        a=case a of
                           C.DEF_USE{cellkind=C.FP,defs,uses} =>
                             C.DEF_USE{cellkind=C.FP,uses=uses,
                                       defs=map rewrite defs}
                          | _ => a}
     | _  => instr
  end
end

