functor SparcRewrite(Instr:SPARCINSTR) = 
struct
   structure I = Instr

   fun rwset(S,rw) = SortedList.uniq(map rw S)

   fun rewriteUse(mapr,instr,rs,rt) =  
   let fun R r = if mapr r = rs then rt else r 
       fun O(i as I.REG r) = if mapr r = rs then I.REG rt else i
         | O i = i
   in  case instr of
         I.LOAD{l,r,i,d,mem} => I.LOAD{l=l,r=R r,i=O i,d=d,mem=mem}
       | I.STORE{s,d,r,i,mem} => I.STORE{s=s,d=R d,r=R r,i=O i,mem=mem}
       | I.FLOAD{l,r,i,d,mem} => I.FLOAD{l=l,r=R r,i=O i,d=d,mem=mem}
       | I.FSTORE{s,d,r,i,mem} => I.FSTORE{s=s,d=d,r=R r,i=O i,mem=mem}
       | I.ARITH{a,r,i,d} => I.ARITH{a=a,r=R r,i=O i,d=d}
       | I.SHIFT{s,r,i,d} => I.SHIFT{s=s,r=R r,i=O i,d=d}
       | I.BR{r,p,rcond,a,nop,label} =>
            I.BR{r=R r,p=p,rcond=rcond,a=a,nop=nop,label=label}
       | I.MOVicc{b,i,d} => I.MOVicc{b=b,i=O i,d=R d}
       | I.MOVfcc{b,i,d} => I.MOVfcc{b=b,i=O i,d=R d}
       | I.MOVR{rcond,r,i,d} => I.MOVR{rcond=rcond,r=R r,i=O i,d=R d}
       | I.JMP{r,i,labs,nop} => I.JMP{r=R r,i=O i,labs=labs,nop=nop}
       | I.JMPL{r,i,d,defs,uses=(A,B,C),nop,mem} => 
            I.JMPL{r=R r,i=O i,d=d,defs=defs,uses=(rwset(A,R),B,C),nop=nop,mem=mem}
       | I.CALL{defs,uses=(A,B,C),label,nop,mem} => 
            I.CALL{defs=defs,uses=(rwset(A,R),B,C),label=label,nop=nop,mem=mem}
       | I.SAVE{r,i,d} => I.SAVE{r=R r,i=O i,d=d}
       | I.RESTORE{r,i,d} => I.RESTORE{r=R r,i=O i,d=d}
       | I.WRY{r,i} => I.WRY{r=R r,i=O i}
       | I.Ticc{t,cc,r,i} => I.Ticc{t=t,cc=cc,r=R r,i=O i}
       | I.COPY{src,dst,tmp,impl} => 
           I.COPY{src=map R src,dst=dst,tmp=tmp,impl=impl}
       | I.ANNOTATION{i,a} => I.ANNOTATION{i=rewriteUse(mapr,i,rs,rt),a=a}
       | _ => instr
   end

   fun rewriteDef(mapr,instr,rs,rt) =
   let fun R r = if mapr r = rs then rt else r 
       fun ea(SOME(I.Direct r)) = SOME(I.Direct(R r))
         | ea x = x 
   in  case instr of
         I.LOAD{l,r,i,d,mem} => I.LOAD{l=l,r=r,i=i,d=R d,mem=mem}
       | I.ARITH{a,r,i,d} => I.ARITH{a=a,r=r,i=i,d=R d}
       | I.SHIFT{s,r,i,d} => I.SHIFT{s=s,r=r,i=i,d=R d}
       | I.SETHI{i,d} => I.SETHI{i=i,d=R d}
       | I.MOVicc{b,i,d} => I.MOVicc{b=b,i=i,d=R d}
       | I.MOVfcc{b,i,d} => I.MOVfcc{b=b,i=i,d=R d}
       | I.MOVR{rcond,r,i,d} => I.MOVR{rcond=rcond,r=r,i=i,d=R d}
       | I.JMPL{r,i,d,defs=(A,B,C),uses,nop,mem} => 
            I.JMPL{r=r,i=i,d=R d,defs=(rwset(A,R),B,C),uses=uses,nop=nop,mem=mem}
       | I.CALL{defs=(A,B,C),uses,label,nop,mem} => 
            I.CALL{defs=(rwset(A,R),B,C),uses=uses,label=label,nop=nop,mem=mem}
       | I.SAVE{r,i,d} => I.SAVE{r=r,i=i,d=R d}
       | I.RESTORE{r,i,d} => I.RESTORE{r=r,i=i,d=R d}
       | I.RDY{d} => I.RDY{d=R d}
       | I.COPY{src,dst,tmp,impl} => 
           I.COPY{src=src,dst=map R dst,tmp=ea tmp,impl=impl}
       | I.ANNOTATION{i,a} => I.ANNOTATION{i=rewriteDef(mapr,i,rs,rt),a=a}
       | _ => instr
   end

   fun frewriteUse(mapr,instr,rs,rt) = 
   let fun R r = if mapr r = rs then rt else r 
   in  case instr of
         I.FPop1{a,r,d} => I.FPop1{a=a,r=R r,d=d}
       | I.FPop2{a,r1,r2,d} => I.FPop2{a=a,r1=R r1,r2=R r2,d=d}
       | I.FCMP{cmp,r1,r2,nop} => I.FCMP{cmp=cmp,r1=R r1,r2=R r2,nop=nop}
       | I.FSTORE{s,r,i,d,mem} => I.FSTORE{s=s,r=r,i=i,d=R d,mem=mem}
       | I.FMOVicc{sz,b,r,d} => I.FMOVicc{sz=sz,b=b,r=R r,d=R d}
       | I.FMOVfcc{sz,b,r,d} => I.FMOVfcc{sz=sz,b=b,r=R r,d=R d}
       | I.JMPL{r,i,d,defs,uses=(A,B,C),nop,mem} =>
           I.JMPL{r=r,i=i,d=d,defs=defs,uses=(A,rwset(B,R),C),nop=nop,mem=mem}
       | I.CALL{defs,uses=(A,B,C),label,nop,mem} =>
           I.CALL{defs=defs,uses=(A,rwset(B,R),C),label=label,nop=nop,mem=mem}
       | I.FCOPY{src,dst,tmp,impl} => 
           I.FCOPY{src=map R src,dst=dst,tmp=tmp,impl=impl}
       | I.ANNOTATION{i,a} => I.ANNOTATION{i=frewriteUse(mapr,i,rs,rt),a=a}
       | _ => instr
   end

   fun frewriteDef(mapr,instr,rs,rt) = 
   let fun R r = if mapr r = rs then rt else r 
       fun ea(SOME(I.FDirect r)) = SOME(I.FDirect(R r))
         | ea x = x 
   in  case instr of
         I.FPop1{a,r,d} => I.FPop1{a=a,r=r,d=R d}
       | I.FPop2{a,r1,r2,d} => I.FPop2{a=a,r1=r1,r2=r2,d=R d}
       | I.FLOAD{l,r,i,d,mem} => I.FLOAD{l=l,r=r,i=i,d=R d,mem=mem}
       | I.FMOVicc{sz,b,r,d} => I.FMOVicc{sz=sz,b=b,r=r,d=R d}
       | I.FMOVfcc{sz,b,r,d} => I.FMOVfcc{sz=sz,b=b,r=r,d=R d}
       | I.JMPL{r,i,d,defs=(A,B,C),uses,nop,mem} =>
           I.JMPL{r=r,i=i,d=d,defs=(A,rwset(B,R),C),uses=uses,nop=nop,mem=mem}
       | I.CALL{defs=(A,B,C),uses,label,nop,mem} =>
           I.CALL{defs=(A,rwset(B,R),C),uses=uses,label=label,nop=nop,mem=mem}
       | I.FCOPY{src,dst,tmp,impl} => 
           I.FCOPY{src=src,dst=map R dst,tmp=ea tmp,impl=impl}
       | I.ANNOTATION{i,a}=> I.ANNOTATION{i=frewriteDef(mapr,i,rs,rt),a=a}
       | _ => instr
   end
  
end

