(* alpha32Rewrite.sml -- rewrite an alpha instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)

functor Alpha32Rewrite(Instr : ALPHA32INSTR) = struct
  structure I=Instr

  fun rewriteUse(mapr, instr, rs, rt) = let
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
     | I.JSR({r,b,d}, defs, uses) =>
       if mapr b=rs then I.JSR({r=r,b=rt,d=d}, defs, uses) else instr
     | I.BRANCH(I.BR, _, _) => instr
     | I.BRANCH(br, r, lab) => if mapr r=rs then I.BRANCH(br, rt, lab) else instr
     | I.OPERATE arg => operate(I.OPERATE, arg)
     | I.OPERATEV arg => operate(I.OPERATEV, arg)
     | I.COPY{dst, src, tmp, impl} => 
	 I.COPY{dst=dst, src=map replace src, tmp=tmp, impl=impl}
     | _ => instr
  end

  fun frewriteUse(mapr, instr, fs, ft) = let
    fun replace f = if mapr f=fs then ft else f
    fun foperate(opClass, {oper, fa, fb, fc}) = 
      if mapr fa=fs then 
	opClass{oper=oper, fa=ft, fc=fc, fb=replace fb}
      else if mapr fb=fs then opClass{oper=oper, fa=fa, fb=ft, fc=fc}
      else instr
  in
    case instr
    of I.FBRANCH(br, f, lab) =>
       if mapr f=fs then I.FBRANCH(br, ft, lab) else instr
     | I.FCOPY{dst, src, impl, tmp} => 
	I.FCOPY{dst=dst, src=map(fn f => if mapr f=fs then ft else f) src, 
		tmp=tmp, impl=impl}
     | I.FSTORE{stOp, r, b, d, mem} => 
	if mapr r=fs then I.FSTORE{stOp=stOp, r=ft, b=b, d=d, mem=mem} else instr
     | I.FOPERATE arg => foperate(I.FOPERATE, arg)
     | I.FOPERATEV arg => foperate(I.FOPERATEV, arg)
     | _ => instr
  end

  fun rewriteDef(mapr, instr, rs, rt) = let
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
     | I.JSR({r, b, d}, defs, uses) =>
       if mapr r=rs then I.JSR({r=rt, b=b, d=d}, defs, uses) else instr
     | I.BRANCH(I.BR, r, lab) => 
       if mapr r=rs then I.BRANCH(I.BR, rt, lab) else instr
     | I.OPERATE{oper, ra, rb, rc} => 
       if mapr rc=rs then I.OPERATE{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.OPERATEV{oper, ra, rb, rc} =>
       if rc=rs then I.OPERATEV{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.COPY{dst, src, impl, tmp} =>
	I.COPY{dst=map rewrite dst, src=src, tmp=ea tmp, impl=impl}
     | _ => instr
  end

  fun frewriteDef(mapr, instr, fs, ft) = let
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
     | I.FCOPY{dst, src, tmp, impl} =>
	I.FCOPY{dst=map rewrite dst, src=src, tmp=ea tmp, impl=impl} 
     | _  => instr
  end
end

(*
 * $Log: alpha32Rewrite.sml,v $
 * Revision 1.5  1998/02/17 02:48:59  george
 *   The rewrite functions take a register map and checks to see
 *   if the register used/defined in the instruction needs to
 *   be rewritten.
 *
 * Revision 1.4  1998/02/16 13:57:45  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.3  1997/08/29 11:00:14  george
 *   Added code to handle the new LDS, CVTLQ, DIVL and DIVLU instructions.
 *
# Revision 1.2  1997/07/28  20:04:10  george
#   Added support for regions
#
# Revision 1.1  1997/07/03  13:57:41  george
#    Version 109.30
#
 *)

