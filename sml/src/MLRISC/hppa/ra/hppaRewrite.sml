(* hppaRewrite.sml -- rewrite an hppa instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)

functor HppaRewrite(Instr:HPPAINSTR) = struct
  structure I = Instr
  structure C = I.C
  structure CB = CellsBasis

  fun error msg = MLRiscErrorMsg.error("HppaRewrite",msg)

  fun rewriteUse(instr, rs, rt) = let
    fun replc r = if CB.sameColor(r,rs) then rt else r
  in
    case instr
    of I.STORE{st, b, d, r, mem} => 
	I.STORE{st=st, b=replc b, d=d, r=replc r, mem=mem} 
     | I.LOAD{l, r1, r2, t, mem} =>
	I.LOAD{l=l, r1=replc r1, r2=replc r2, t=t, mem=mem}
     | I.LOADI{li, r, i, t, mem=mem} => 
	I.LOADI{li=li, r=replc r, i=i, t=t, mem=mem} 
     | I.ARITH{a, r1, r2, t} => I.ARITH{a=a, r1=replc r1, r2=replc r2, t=t}
     | I.ARITHI{ai, r, i, t} => I.ARITHI{ai=ai, r=replc r, i=i, t=t}
     | I.COMCLR_LDO{cc, r1, r2, b, i, t1, t2} => 
         if CB.registerId t1 <> 0 andalso not(CB.sameColor(t1,t2)) 
            andalso CB.sameColor(t2,rs) then 
            error "rewriteUse: COMCLR_LDO"
         else
         I.COMCLR_LDO{cc=cc, r1=replc r1, r2=replc r2, b=replc b, i=i, 
                      t1=t1, t2=t2}
     | I.COMICLR_LDO{cc, i1, r2, b, i2, t1, t2} => 
         if CB.registerId t1 <> 0 andalso not(CB.sameColor(t1,t2)) 
            andalso CB.sameColor(t2,rs) then 
            error "rewriteUse: COMICLR_LDO"
         else
         I.COMICLR_LDO{cc=cc, i1=i1, r2=replc r2, b=replc b, i2=i2, 
                       t1=t1, t2=t2}
     | I.SHIFTV{sv, r, len, t} => I.SHIFTV{sv=sv, r=replc r, len=len, t=t}
     | I.SHIFT{s, r, p, len, t} => I.SHIFT{s=s, r=replc r, p=p, len=len, t=t} 
     | I.BCOND{cmp, bc, r1, r2, t, f, n, nop} => 
	I.BCOND{cmp=cmp, bc=bc, r1=replc r1, r2=replc r2, t=t, f=f,n=n, nop=nop}
     | I.BCONDI{cmpi, bc, i, r2, t, f, n, nop} => 
        I.BCONDI{cmpi=cmpi, bc=bc, i=i, r2=replc r2, t=t, f=f,n=n, nop=nop} 
     | I.BB{bc, r, p, t, f, n, nop} => 
        I.BB{bc=bc, r=replc r, p=p, t=t, f=f,n=n, nop=nop} 
     | I.BV{x, b, labs, n} => I.BV{x=replc x, b=replc b, labs=labs,n=n} 
     | I.BE{b, d, sr, labs, n} => I.BE{b=replc b, d=d, sr=sr, labs=labs, n=n} 
     | I.BLR{x, t, labs, n} => I.BLR{x=replc x, t=t, labs=labs,n=n} 
     | I.BLE{b, d, sr, t, defs, uses, cutsTo, mem} => 
	I.BLE{b=replc b, d=d, sr=sr, t=t, defs=defs, 
              uses=C.CellSet.map {from=rs,to=rt} uses, cutsTo=cutsTo, mem=mem} 
     | I.BL{lab, t, defs, uses, mem, cutsTo, n} => 
	I.BL{lab=lab, t=t, defs=defs, cutsTo=cutsTo,
             uses=C.CellSet.map {from=rs,to=rt} uses, mem=mem, n=n} 
     | I.LDO{b, t, i} => I.LDO{b=replc b, t=t, i=i} 
     | I.COPY{dst, src, tmp, impl} => 
	I.COPY{dst=dst, src=map replc src, impl=impl, tmp=tmp}
     | I.MTCTL{r, t} => I.MTCTL{r=replc r, t=t}
     | I.FSTORE{fst, b, d, r, mem} => 
        I.FSTORE{fst=fst, b=replc b, d=d, r=r, mem=mem} 
     | I.FSTOREX{fstx, b, x, r, mem} =>
        I.FSTOREX{fstx=fstx, b=replc b, x=replc x, r=r, mem=mem} 
     | I.FLOAD{fl, b, d, t, mem} =>
	I.FLOAD{fl=fl, b=replc b, d=d, t=t, mem=mem} 
     | I.FLOADX{flx, b, x, t, mem} =>
	I.FLOADX{flx=flx, b=replc b, x=replc x, t=t, mem=mem} 
     | I.ANNOTATION{i,a} => 
        I.ANNOTATION{i=rewriteUse(i,rs,rt),
                        a=case a of
                           CB.DEF_USE{cellkind=CB.GP,defs,uses} =>
                             CB.DEF_USE{cellkind=CB.GP,uses=map replc uses,
                                       defs=defs}
                          | _ => a}
     | _ => instr
  end

  fun rewriteDef(instr, rs, rt) = let
    fun replc r = if CB.sameColor(r,rs) then rt else r
    fun ea (SOME(I.Direct r)) = SOME(I.Direct (replc r))
      | ea x = x
  in
    case instr
    of I.ARITH{a, r1, r2, t} => I.ARITH{a=a, r1=r1, r2=r2, t=replc t} 
     | I.ARITHI{ai, i, r, t} => I.ARITHI{ai=ai, i=i, r=r, t=replc t}
     | I.LOAD{l, r1, r2, t, mem} => I.LOAD{l=l,r1=r1,r2=r2,t=replc t,mem=mem} 
     | I.LOADI{li, i, r, t, mem} => I.LOADI{li=li,i=i,r=r,t=replc t,mem=mem} 
     | I.COMCLR_LDO{cc, r1, r2, b, i, t1, t2} => 
         if CB.registerId t1 <> 0 andalso not(CB.sameColor(t1,t2)) 
            andalso CB.sameColor(t2,rs) then 
            error "rewriteDef: COMCLR_LDO"
         else
          I.COMCLR_LDO{cc=cc, r1=r1, r2=r2, b=b, i=i, t1=replc t1, t2=replc t2} 
     | I.COMICLR_LDO{cc, i1, r2, b, i2, t1, t2} => 
         if CB.registerId t1 <> 0 andalso not(CB.sameColor(t1,t2)) 
            andalso CB.sameColor(t2,rs) then 
            error "rewriteDef: COMICLR_LDO"
         else
          I.COMICLR_LDO{cc=cc, i1=i1, r2=r2, b=b, i2=i2, 
                        t1=replc t1, t2=replc t2}
     | I.SHIFTV{sv, r, len, t} => I.SHIFTV{sv=sv, r=r, len=len, t=replc t}
     | I.SHIFT{s, r, p, len, t} => I.SHIFT{s=s, r=r, p=p, len=len, t=replc t}
     | I.BLR{x, t, labs, n} => I.BLR{x=x, t=replc t, labs=labs,n=n} 
     | I.BLE{d, b, sr, t, defs, uses, cutsTo, mem} => 
        I.BLE{d=d, b=b, sr=sr, t=replc t, 
              defs=C.CellSet.map {from=rs,to=rt} defs, uses=uses, 
              cutsTo=cutsTo, mem=mem}
     | I.BL{lab, t, defs, uses, mem, cutsTo, n} => 
	I.BL{lab=lab, t=replc t, cutsTo=cutsTo,
              defs=C.CellSet.map {from=rs,to=rt} defs, uses=uses, mem=mem, n=n} 
     | I.LDIL{i, t} => I.LDIL{i=i, t=replc t} 
     | I.LDO{i, b, t} => I.LDO{i=i, b=b, t=replc t}
     | I.COPY{dst, src, impl, tmp} =>
	  I.COPY{dst=map replc dst, src=src, impl=impl, tmp=ea tmp}
     | I.ANNOTATION{i,a} => 
          I.ANNOTATION{i=rewriteDef(i,rs,rt),
                        a=case a of
                           CB.DEF_USE{cellkind=CB.GP,defs,uses} =>
                             CB.DEF_USE{cellkind=CB.GP,uses=uses,
                                       defs=map replc defs}
                          | _ => a}
     | _ => instr
  end

  fun frewriteUse(instr, fs, ft) = let
    fun replc r = if CB.sameColor(r,fs) then ft else r
  in
    case instr
    of I.FSTORE{fst, b, d, r, mem} =>
        I.FSTORE{fst=fst, b=b, d=d, r=replc r, mem=mem}
     | I.FSTOREX{fstx, b, x, r, mem} =>
	I.FSTOREX{fstx=fstx, b=b, x=x, r=replc r, mem=mem} 
     | I.FARITH{fa, r1, r2, t}  => 
	I.FARITH{fa=fa, r1=replc r1, r2=replc r2, t=t}
     | I.FUNARY{fu, f, t} => I.FUNARY{fu=fu, f=replc f, t=t} 
     | I.FCNV{fcnv, f, t} => I.FCNV{fcnv=fcnv, f=replc f, t=t} 
     | I.FBRANCH{cc,fmt,f1,f2,t,f,n,long} =>
         I.FBRANCH{cc=cc,fmt=fmt,f1=replc f1,f2=replc f2,t=t,f=f,n=n,long=long}
     | I.FCOPY{dst, src, tmp, impl} => 
	I.FCOPY{dst=dst, src=map replc src, impl=impl, tmp=tmp}
     | I.BLE{d, b, sr, t, defs, uses, cutsTo, mem} => 
        I.BLE{d=d, b=b, sr=sr, t=replc t, defs=defs, 
             uses=C.CellSet.map {from=fs,to=ft} uses, cutsTo=cutsTo, mem=mem}
     | I.BL{lab, t, defs, uses, mem, cutsTo, n} => 
	I.BL{lab=lab, t=t, defs=defs, cutsTo=cutsTo, 
             uses=C.CellSet.map {from=fs,to=ft} uses, mem=mem, n=n} 
     | I.ANNOTATION{i,a} => 
        I.ANNOTATION{i=frewriteUse(i,fs,ft),
                        a=case a of
                           CB.DEF_USE{cellkind=CB.FP,defs,uses} =>
                             CB.DEF_USE{cellkind=CB.FP,uses=map replc uses,
                                       defs=defs}
                          | _ => a}
     | _ => instr
    (*esac*)
  end

  fun frewriteDef(instr, fs, ft) = let
    fun replc r = if CB.sameColor(r,fs) then ft else r
    fun ea (SOME(I.FDirect f)) = SOME(I.FDirect(replc f))
      | ea x  = x
  in
    case instr
    of I.FLOAD{fl, b, d, t, mem} => 
         I.FLOAD{fl=fl, b=b, d=d, t=replc t,mem=mem}
     | I.FLOADX{flx, b, x, t, mem} => 
         I.FLOADX{flx=flx, b=b, x=x, t=replc t, mem=mem} 
     | I.FARITH {fa, r1, r2, t} => I.FARITH{fa=fa, r1=r1, r2=r2, t=replc t}
     | I.FUNARY{fu, f, t} => I.FUNARY{fu=fu, f=f, t=replc t}
     | I.FCNV{fcnv, f, t} => I.FCNV{fcnv=fcnv, f=f, t=replc t}
     | I.FCOPY{dst, src, impl, tmp} => 
	I.FCOPY{dst=map replc dst, src=src, impl=impl, tmp=ea tmp}
     | I.BLE{d, b, sr, t, defs, uses, cutsTo, mem} => 
        I.BLE{d=d, b=b, sr=sr, t=replc t, cutsTo=cutsTo,
              defs=C.CellSet.map {from=fs,to=ft} defs, uses=uses, mem=mem}
     | I.BL{lab, t, defs, uses, mem, cutsTo, n} => 
	I.BL{lab=lab, t=t, cutsTo=cutsTo, 
             defs=C.CellSet.map {from=fs,to=ft} defs, uses=uses, mem=mem, n=n} 
     | I.ANNOTATION{i,a} => 
        I.ANNOTATION{i=frewriteDef(i,fs,ft),
                        a=case a of
                           CB.DEF_USE{cellkind=CB.FP,defs,uses} =>
                             CB.DEF_USE{cellkind=CB.FP,uses=uses,
                                       defs=map replc defs}
                          | _ => a}
     | _ => instr
    (*esac*)
  end
end

