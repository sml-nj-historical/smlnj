functor HppaShuffle(I:HPPAINSTR) : HPPASHUFFLE = struct
  structure I = I
  structure C = I.C
  structure Shuffle = Shuffle(I)
  structure CB = CellsBasis
  type t = {tmp:I.ea option, dst:CB.cell list, src:CB.cell list}

  fun error msg = MLRiscErrorMsg.error("HppaShuffle",msg)
  val mem = I.Region.memory

  val zeroR = Option.valOf(C.zeroReg CB.GP)

  fun move{src=I.Direct rs, dst=I.Direct rt} = 
       [I.arith{a=I.OR, r1=rs, r2=zeroR, t=rt}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} =
       [I.loadi{li=I.LDW, r=base, i=I.IMMED disp, t=rt, mem=mem}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
       [I.store{st=I.STW, b=base, d=I.IMMED disp, r=rs, mem=mem}]
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} =
        [I.funary{fu=I.FCPY_D, f=fs, t=fd}]
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} = let
        val tmp = I.C.newCell CB.GP ()
      in
	[I.ldo{i=I.IMMED disp, b=base, t=tmp},
	 I.floadx{flx=I.FLDDX, b=tmp, x=zeroR, t=ft, mem=mem}]
      end
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = let
	val tmp = I.C.newCell CB.GP ()
      in
	[I.ldo{i=I.IMMED disp, b=base, t=tmp},
	 I.fstorex{fstx=I.FSTDX, b=tmp, x=zeroR, r=fs, mem=mem}]
      end
    | fmove _ = error "move"

  val shuffle = Shuffle.shuffle{mvInstr = move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end


