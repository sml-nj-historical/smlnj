(* alphaRewrite.sml -- rewrite an alpha instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)

functor AlphaRewrite(Instr : ALPHAINSTR) = struct
  structure I=Instr

  fun rewriteUse(mapr : I.C.cell -> I.C.cell, instr, rs, rt) = let
    fun isRegOp (opnd as I.REGop r) = mapr r=rs
      | isRegOp _ = false
    fun rwOperand(opnd as I.REGop r) = if mapr r=rs then I.REGop rt else opnd
      | rwOperand opnd = opnd
    fun replace r = if mapr r=rs then rt else r
    fun load(ldClass, {ldOp, r, b, d, mem}) =
       if mapr b=rs then ldClass{ldOp=ldOp, r=r, b=rt, d=d, mem=mem}
       else instr
    fun fstore(stClass, {stOp, r, b, d, mem}) =
       if mapr b=rs then stClass{stOp=stOp, r=r, b=rt, d=d, mem=mem}
       else instr
    fun store{stOp, r, b, d, mem} = 
      if mapr r=rs then
	if mapr b=rs then
	  I.STORE{stOp=stOp, r=rt, b=rt, d=d, mem=mem}
	else
	  I.STORE{stOp=stOp, r=rt, b=b, d=d, mem=mem}
      else if mapr b=rs then
	I.STORE{stOp=stOp, r=r, b=rt, d=d, mem=mem}
      else instr
    fun operate(opClass, {oper, ra, rb, rc}) = 
      if mapr ra=rs then
	if isRegOp rb then 
	  opClass{oper=oper, ra=rt, rb=I.REGop rt, rc=rc}
	else opClass{oper=oper, ra=rt, rb=rb, rc=rc}
      else if isRegOp rb then
	opClass{oper=oper, ra=ra, rb=I.REGop rt, rc=rc}
      else instr

  in
    case instr
    of I.LDA{r, b, d} => if mapr b=rs then I.LDA{r=r, b=rt, d=d} else instr
     | I.LDAH{r, b, d} => if mapr b=rs then I.LDAH{r=r, b=rt, d=d} else instr
     | I.LOAD arg => load(I.LOAD, arg)
     | I.FLOAD farg => load(I.FLOAD, farg)
     | I.STORE arg => store arg
     | I.FSTORE farg => fstore(I.FSTORE, farg)
     | I.JMPL({r, b, d}, labs) =>
       if mapr b=rs then I.JMPL({r=r, b=rt, d=d}, labs) else instr
     | I.JSR{r, b, d, defs, uses=(i,f), mem} =>
	 I.JSR{r=r, b=replace b, d=d, defs=defs, uses=(map replace i, f), 
               mem=mem}
     | I.RET{r,b,d} => I.RET{r=r, b=replace b, d=d}
     | I.BRANCH{b=I.BR, ...} => instr
     | I.BRANCH{b, r, lab} => if mapr r=rs then I.BRANCH{b=b, r=rt, lab=lab} 
                              else instr
     | I.OPERATE arg => operate(I.OPERATE, arg)
     | I.OPERATEV arg => operate(I.OPERATEV, arg)
     | I.CMOVE{oper,ra,rb,rc} => 
         I.CMOVE{oper=oper,ra=replace ra,rb=rwOperand rb,rc=replace rc}
     | I.COPY{dst, src, tmp, impl} => 
	 I.COPY{dst=dst, src=map replace src, tmp=tmp, impl=impl}
     | I.CALL_PAL{code, def, use } => 
         I.CALL_PAL{code=code, def=def, use=map replace use}
     | I.ANNOTATION{i,a} => I.ANNOTATION{i=rewriteUse(mapr,i,rs,rt),a=a}
     | _ => instr
  end

  fun frewriteUse(mapr : I.C.cell -> I.C.cell, instr, fs, ft) = let
    fun replace f = if mapr f=fs then ft else f
    fun foperate(opClass, {oper, fa, fb, fc}) = 
      if mapr fa=fs then 
	opClass{oper=oper, fa=ft, fc=fc, fb=replace fb}
      else if mapr fb=fs then opClass{oper=oper, fa=fa, fb=ft, fc=fc}
      else instr
  in
    case instr
    of I.FBRANCH{b, f, lab} =>
       if mapr f=fs then I.FBRANCH{b=b, f=ft, lab=lab} else instr
     | I.FCOPY{dst, src, impl, tmp} => 
	I.FCOPY{dst=dst, src=map(fn f => if mapr f=fs then ft else f) src, 
		tmp=tmp, impl=impl}
     | I.FSTORE{stOp, r, b, d, mem} => 
	if mapr r=fs then I.FSTORE{stOp=stOp, r=ft, b=b, d=d, mem=mem} else instr
     | I.FOPERATE arg => foperate(I.FOPERATE, arg)
     | I.FOPERATEV arg => foperate(I.FOPERATEV, arg)
     | I.FUNARY{oper,fb,fc} =>
        if mapr fb=fs then I.FUNARY{oper=oper,fb=ft,fc=fc} else instr
     | I.FCMOVE{oper,fa,fb,fc} => 
         I.FCMOVE{oper=oper,fa=replace fa,fb=replace fb,fc=replace fc}
     | I.JSR{r, b, d, defs, uses=(i,f), mem} => 
         I.JSR{r=r, b=b, d=d, defs=defs, uses=(i, map replace f), mem=mem}
     | I.ANNOTATION{i,a} => I.ANNOTATION{i=frewriteUse(mapr,i,fs,ft),a=a}
     | _ => instr
  end

  fun rewriteDef(mapr : I.C.cell -> I.C.cell, instr, rs, rt) = let
    fun rewrite r = if mapr r = rs then rt else r
    fun ea (SOME(I.Direct r)) = SOME(I.Direct (rewrite r))
      | ea x = x
  in
    case instr
    of I.LDA{r, b, d} => if mapr r=rs then I.LDA{r=rt, b=b, d=d} else instr
     | I.LDAH{r, b, d} => if mapr r=rs then I.LDAH{r=rt, b=b, d=d} else instr
     | I.LOAD{ldOp, r, b, d, mem} => 
       if mapr r=rs then I.LOAD{ldOp=ldOp, r=rt, b=b, d=d, mem=mem} else instr
     | I.JMPL({r, b, d}, labs) =>
       if mapr r=rs then I.JMPL({r=rt, b=b, d=d}, labs) else instr
     | I.JSR{r, b, d, defs=(i,f), uses, mem} =>
         I.JSR{r=rewrite r, b=b, d=d, defs=(map rewrite i, f), uses=uses, 
               mem=mem}
     | I.RET{r, b, d} => I.RET{r=rewrite r, b=b, d=d}
     | I.BRANCH{b=I.BR, r, lab} => 
       if mapr r=rs then I.BRANCH{b=I.BR, r=rt, lab=lab} else instr
     | I.OPERATE{oper, ra, rb, rc} => 
       if mapr rc=rs then I.OPERATE{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.OPERATEV{oper, ra, rb, rc} =>
       if rc=rs then I.OPERATEV{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.CMOVE{oper,ra,rb,rc} => I.CMOVE{oper=oper,ra=ra,rb=rb,rc=rewrite rc}
     | I.COPY{dst, src, impl, tmp} =>
	I.COPY{dst=map rewrite dst, src=src, tmp=ea tmp, impl=impl}
     | I.CALL_PAL{code, def, use } => 
         I.CALL_PAL{code=code, def=map rewrite def, use=use}
     | I.ANNOTATION{i,a} => I.ANNOTATION{i=rewriteDef(mapr,i,rs,rt),a=a}
     | _ => instr
  end

  fun frewriteDef(mapr : I.C.cell -> I.C.cell, instr, fs, ft) = let
    fun rewrite f = if mapr f = fs then ft else f
    fun ea (SOME(I.FDirect f)) = SOME(I.FDirect(rewrite f))
      | ea x  = x
  in
    case instr
    of I.DEFFREG f => if mapr f=fs then I.DEFFREG ft else instr
     | I.FLOAD{ldOp, r, b, d, mem} => 
        if mapr r=fs then I.FLOAD{ldOp=ldOp, r=ft, b=b, d=d, mem=mem} else instr
     | I.FOPERATE{oper, fa, fb, fc} =>
	if mapr fc=fs then I.FOPERATE{oper=oper, fa=fa, fb=fb, fc=ft} else instr
     | I.FOPERATEV{oper, fa, fb, fc} =>
	if mapr fc=fs then I.FOPERATEV{oper=oper, fa=fa, fb=fb, fc=ft} else instr
     | I.FUNARY{oper,fb,fc} =>
        if mapr fc=fs then I.FUNARY{oper=oper,fb=fb,fc=ft} else instr
     | I.FCOPY{dst, src, tmp, impl} =>
	I.FCOPY{dst=map rewrite dst, src=src, tmp=ea tmp, impl=impl} 
     | I.FCMOVE{oper,fa,fb,fc} => I.FCMOVE{oper=oper,fa=fa,fb=fb,fc=rewrite fc}
     | I.JSR{r, b, d, defs=(i,f), uses, mem} => 
        I.JSR{r=r, b=b, d=d, defs=(i, map rewrite f), uses=uses, mem=mem}
	
     | I.ANNOTATION{i,a} => I.ANNOTATION{i=frewriteDef(mapr,i,fs,ft),a=a}
     | _  => instr
  end
end

