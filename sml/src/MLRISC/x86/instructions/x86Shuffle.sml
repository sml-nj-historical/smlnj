functor X86Shuffle(I : X86INSTR) : X86SHUFFLE =
struct
  structure I = I
  structure C = I.C
  structure Shuffle = Shuffle(I)

  type t = {tmp:I.ea option, dst:C.cell list, src:C.cell list}

  exception foo
  val shuffle =
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.MOVE{mvOp=I.MOVL, src=src, dst=dst}],
	 ea=I.Direct}

  (*
   * These assume that the ''registers'' are mapped onto the memory
   *)

  (* Note, this only works with double precision floating point *) 
  val shufflefpNormalAndSlow = 
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.FLDL src, I.FSTPL dst],
	 ea = I.FDirect}

  (* 
   * This version makes use of the x86 floating point stack for hardware
   * renaming! 
   *)
  fun shufflefpNormal{tmp, src, dst} = 
  let val n =  length src
  in  if n <= 7 then 
         let fun gen(s::ss, d::ds, pushes, pops) = 
                 if C.sameColor(s,d) then gen(ss, ds, pushes, pops)
                 else gen(ss, ds, I.FLDL(I.FDirect s)::pushes, 
                                      I.FSTPL(I.FDirect d)::pops)
               | gen(_, _, pushes, pops) = List.revAppend(pushes, pops)
         in  gen(src, dst, [], []) end
      else shufflefpNormalAndSlow{tmp=tmp, src=src, dst=dst}
  end

  (*
   * These assume that the ''registers'' are mapped onto the pseudo 
   * %fpr register.  Only works with double precision floating point for 
   * now...
   *)
  val shufflefpFast = 
       Shuffle.shuffle
         {mvInstr=fn{dst, src} => [I.FMOVE{fsize=I.FP64,src=src, dst=dst}],
	  ea = I.FPR}

  fun shufflefp(x as {tmp=SOME(I.FPR _), ...}) = shufflefpFast x
    | shufflefp x = shufflefpNormal x

end

