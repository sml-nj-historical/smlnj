(*
 * This module describes how to fill delay slots on the HPPA
 *)

functor HppaDelaySlots
   (structure I : HPPAINSTR
    structure P : INSN_PROPERTIES where I = I
   ) : DELAY_SLOT_PROPERTIES =
struct
   structure I  = I
   structure SL = SortedList

   fun error msg = MLRiscErrorMsg.error("HppaDelaySlotProps",msg)

   datatype delay_slot = D_NONE | D_ERROR | D_ALWAYS | D_TAKEN | D_FALLTHRU

   val delaySlotSize = 4

   (*
    * On the HP, things are quite complicated:
    *
    *  For conditional branches:
    *  -------------------------
    *                                 Branch direction         
    *                      Forward                    Backward
    *  Nullify bit on    Nullify if branch taken   Nullify if branch not-taken
    *  Nullify bit off   Delay slot active         Delay slot active
    *
    *  For unconditional branches:
    *  ---------------------------
    *       
    *  Nullify bit on    Delay slot nullified
    *  Nullify bit off   Delay slot active       
    *)

   fun delaySlot{instr, backward} =
       case instr of
         I.BCOND{nop,n,...} => 
             {nop=nop, n=n, nOn=if backward then D_TAKEN else D_FALLTHRU, 
              nOff=D_ALWAYS}
       | I.BCONDI{nop,n,...} => 
             {nop=nop, n=n, nOn=if backward then D_TAKEN else D_FALLTHRU, 
              nOff=D_ALWAYS}
       | I.BB{nop,n,...} =>
             {nop=nop, n=n, nOn=if backward then D_TAKEN else D_FALLTHRU, 
              nOff=D_ALWAYS}
       | I.B{n,...} => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.BV{n,...} => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.BE{n,...} => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.BLR{n,...} => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.BL{n,...} => {nop=false, n=n, nOn=D_NONE, nOff=D_ALWAYS}
       | I.ANNOTATION{i,...} => delaySlot{instr=i,backward=backward}
       | _ => {n=false,nOn=D_ERROR,nOff=D_NONE,nop=false}

   fun enableDelaySlot{instr, n, nop} =
       case (instr,nop) of
         (I.BCOND{cmp,bc,r1,r2,t,f,...},_) => 
             I.BCOND{cmp=cmp,bc=bc,nop=nop,n=n,r1=r1,r2=r2,t=t,f=f}
       | (I.BCONDI{cmpi,bc,i,r2,t,f,...},_) => 
             I.BCONDI{cmpi=cmpi,bc=bc,nop=nop,n=n,i=i,r2=r2,t=t,f=f}
       | (I.BB{bc,p,r,t,f,...},_) => 
             I.BB{bc=bc,p=p,nop=nop,n=n,r=r,t=t,f=f}
       | (I.B{lab,...},false) => I.B{lab=lab,n=n}
       | (I.BV{labs,b,x,...},false) => I.BV{labs=labs,b=b,x=x,n=n}
       | (I.BE{labs,b,d,sr,...},false) => I.BE{labs=labs,b=b,d=d,sr=sr,n=n}
       | (I.BLR{x,t,labs,...},false) => I.BLR{x=x,t=t,labs=labs,n=n}
       | (I.BL{x,t,defs,uses,...},false) => 
            I.BL{x=x,t=t,defs=defs,uses=uses,n=n}
       | (I.ANNOTATION{i,a},_) => 
           I.ANNOTATION{i=enableDelaySlot{instr=i,n=n,nop=nop},a=a}
       | _ => error "enableDelaySlot"

    val defUseI = P.defUse I.C.GP
    val defUseF = P.defUse I.C.FP

    fun conflict{regmap,src=i,dst=j} = 
        let fun clash(defUse) =
                let val (di,ui) = defUse i
                    val (dj,uj) = defUse j
                in  case SL.intersect(di,uj) of
                       [] => (case SL.intersect(di,dj) of
                                [] => (case SL.intersect(ui,dj) of
                                         [] => false
                                       | _ => true)
                              | _ => true)
                    |  _ => true
                end
            fun defUseInt i = 
                let val (d,u) = defUseI i
                    val d     = SL.uniq(map regmap d)
                    val u     = SL.uniq(map regmap u)
                    (* no dependence on register 0! *) 
                    fun elim0(0::l) = l
                      | elim0 l     = l
                in  (elim0 d, elim0 u) end
            fun defUseReal i = 
                let val (d,u) = defUseF i
                    val d     = SL.uniq(map regmap d)
                    val u     = SL.uniq(map regmap u)
                in  (d,u) end
        in  clash(defUseInt) orelse clash(defUseReal) 
        end

    fun delaySlotCandidate{jmp,delaySlot=
             (I.BCOND _ | I.BCONDI _ | I.BB _ | I.FBRANCH _ | I.BV _ | I.BE _ 
             | I.COMCLR_LDO _ | I.BLR _ | I.BL _ | I.BLE _)} = false
      | delaySlotCandidate{jmp=I.ANNOTATION{i,...},delaySlot} = 
           delaySlotCandidate{jmp=i,delaySlot=delaySlot}
      | delaySlotCandidate{jmp,delaySlot=I.ANNOTATION{i,...}} = 
           delaySlotCandidate{jmp=jmp,delaySlot=i}
      | delaySlotCandidate _ = true

   fun setTarget(I.BCOND{n,nop,r1,r2,cmp,bc,t,f,...},lab) = 
         I.BCOND{cmp=cmp,bc=bc,nop=nop,n=n,r1=r1,r2=r2,t=lab,f=f}
     | setTarget(I.BCONDI{n,nop,i,r2,cmpi,bc,t,f,...},lab) = 
         I.BCONDI{cmpi=cmpi,bc=bc,nop=nop,n=n,i=i,r2=r2,t=lab,f=f}
     | setTarget(I.BB{bc,r,p,n,nop,t,f,...},lab) = 
         I.BB{bc=bc,p=p,nop=nop,n=n,r=r,t=lab,f=f}
     | setTarget(I.B{n,...},lab) = I.B{n=n,lab=lab}
     | setTarget(I.ANNOTATION{i,a},lab) = I.ANNOTATION{i=setTarget(i,lab),a=a}
     | setTarget _ = error "setTarget"

end
