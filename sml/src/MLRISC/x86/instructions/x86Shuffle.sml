functor X86Shuffle(I : X86INSTR) : X86SHUFFLE =
struct
  structure I = I
  structure Shuffle = Shuffle(I)

  type t = {regmap:int->int, tmp:I.ea option, dst:int list, src:int list}

  exception foo
  val shuffle =
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.MOVE{mvOp=I.MOVL, src=src, dst=dst}],
	 ea=I.Direct}

  (* Note, this only works with double precision floating point *)
  val shufflefp = 
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.FLDL src, I.FSTPL dst],
	 ea = I.FDirect}
end

