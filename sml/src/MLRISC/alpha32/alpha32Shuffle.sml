functor Alpha32Shuffle(I:ALPHA32INSTR) = struct
  structure I = I
  structure Shuffle = Shuffle(I)

  val mem=I.Region.memory

  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}
  fun move{src=I.Direct rs, dst=I.Direct rd} = 
        [I.OPERATE{oper=I.BIS, ra=rs, rb=I.REGop 31, rc=rd}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
	[I.STORE{stOp=I.STL, r=rs, b=base, d=I.IMMop disp, mem=mem}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} = 
	[I.LOAD{ldOp=I.LDL, r=rt, b=base, d=I.IMMop disp, mem=mem}]

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
        [I.FOPERATE{oper=I.CPYS, fa=fs, fb=fs, fc=fd}]
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = 
	[I.FSTORE{stOp=I.STT, r=fs, b=base, d=I.IMMop disp, mem=mem}]
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} =
	[I.FLOAD{ldOp=I.LDT, r=ft, b=base, d=I.IMMop disp, mem=mem}]

  val shuffle = 
    Shuffle.shuffle
      {mvInstr = 
         fn {src=I.Direct rs, dst=I.Direct rd} =>
	      [I.OPERATE{oper=I.BIS, ra=rs, rb=I.REGop 31, rc=rd}],
       ea = I.Direct}

  val shufflefp = 
    Shuffle.shuffle
      {mvInstr = 
         fn {src=I.FDirect fs, dst=I.FDirect fd} =>
	      [I.FOPERATE{oper=I.CPYS, fa=fs, fb=fs, fc=fd}],
       ea = I.FDirect}
end

(*
 * $Log: alpha32Shuffle.sml,v $
 * Revision 1.1  1998/02/16 14:00:19  george
 *   Version 110.2
 *
 *)
