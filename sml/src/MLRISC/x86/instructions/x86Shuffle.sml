functor X86Shuffle(I : X86INSTR) : X86SHUFFLE =
struct
  structure I = I
  structure Shuffle = Shuffle(I)

  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  exception foo
  val shuffle =
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.MOVE{mvOp=I.MOVL, src=src, dst=dst}],
	 ea=I.Direct}

  val shufflefp = 
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.FLD src, I.FSTP dst],
	 ea = I.FDirect}
end

