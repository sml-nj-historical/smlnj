functor PPCShuffle(I:PPCINSTR) = struct
  structure I = I
  structure Shuffle = Shuffle(I)

  val mem=I.Region.memory

  type t = {regmap:I.C.register->I.C.register, tmp:I.ea option,                             dst:I.C.register list, src:I.C.register list}

  fun error msg = MLRiscErrorMsg.error("PPCShuffle",msg)

  (* WARNING: these move operators assume 32 bit addressing is used! 
   * Allen
   *)
  fun move{src=I.Direct rs, dst=I.Direct rd} = 
        [I.ARITH{oper=I.OR, rt=rd, ra=rs, rb=rs, Rc=false, OE=false}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
	[I.ST{st=I.STW, rs=rs, ra=base, d=disp, mem=mem}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} = 
	[I.L{ld=I.LWZ, rt=rt, ra=base, d=disp, mem=mem}]
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
        [I.FUNARY{oper=I.FMR, fb=fs, ft=fd, Rc=false}]
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = 
	[I.STF{st=I.STFD, fs=fs, ra=base, d=disp, mem=mem}]
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} =
	[I.LF{ld=I.LFD, ft=ft, ra=base, d=disp, mem=mem}]
    | fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle {mvInstr=move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end
