functor SparcShuffle(I:SPARCINSTR) : SPARCSHUFFLE = 
struct
  structure I = I
  structure W = Word32
  structure Shuffle = Shuffle(I)
  type t = {tmp:I.ea option, dst:I.C.cell list, src:I.C.cell list}

  fun error msg = MLRiscErrorMsg.error("SparcShuffle",msg)
  val mem = I.Region.memory
  val zeroR = Option.valOf(I.C.zeroReg I.C.GP)

  fun move{src=I.Direct rs, dst=I.Direct rt} = 
       [I.ARITH{a=I.OR, r=zeroR, i=I.REG rs, d=rt}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} =
       [I.LOAD{l=I.LD, r=base, i=I.IMMED disp, d=rt, mem=mem}] 
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
       [I.STORE{s=I.ST, r=base, i=I.IMMED disp, d=rs, mem=mem}] 
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
       [I.FPop1{a=I.FMOVd, r=fs, d=fd}] 
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} = 
       [I.FLOAD{l=I.LDDF, r=base, i=I.IMMED disp, d=ft, mem=mem}] 
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = 
       [I.FSTORE{s=I.STDF, r=base, i=I.IMMED disp, d=fs, mem=mem}] 
    | fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle{mvInstr = move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end


