functor PPCShuffle(I:PPCINSTR) = struct
  structure I = I
  structure Shuffle = Shuffle(I)

  val mem=I.Region.memory

  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  fun error msg = MLRiscErrorMsg.impossible("PPCShuffle." ^ msg)

  fun move{src=I.Direct rs, dst=I.Direct rd} = 
        [I.ARITH{oper=I.OR, rt=rd, ra=rs, rb=rs, Rc=false, OE=false}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
	[I.ST{sz=I.Word, rs=rs, ra=base, d=disp, mem=mem}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} = 
	[I.L{sz=I.Word, rt=rt, ra=base, d=disp, mem=mem}]
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
        [I.FUNARY{oper=I.FMR, fb=fs, ft=fd, Rc=false}]
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = 
	[I.ST{sz=I.Double, rs=fs, ra=base, d=disp, mem=mem}]
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} =
	[I.L{sz=I.Double, rt=ft, ra=base, d=disp, mem=mem}]
    | fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle {mvInstr=move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end
