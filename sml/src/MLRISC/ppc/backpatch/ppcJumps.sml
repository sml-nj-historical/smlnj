functor PPCJumps 
  (structure Instr: PPCINSTR
   structure Shuffle : PPCSHUFFLE where I = Instr
  ) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure LE = LabelExp

  fun error msg = MLRiscErrorMsg.impossible ("PPCJumps." ^ msg)

  val branchDelayedArch = false

  fun isSdi instr = let
    fun operand(I.LabelOp _) = true
      | operand(I.ConstOp _) = true
      | operand _ = false
  in
    case instr
    of I.L{d, ...} => operand d
     | I.ST{d, ...} => operand d
     | I.ARITHI{im, ...} => operand im
     | I.ROTATE{sh, ...} => operand sh
     | I.COMPARE{rb, ...} => operand rb
     | I.TWI{si, ...} => operand si
     | I.BC{addr, ...} => operand addr
     | I.COPY _ => true
     | I.FCOPY _ => true
     | _ => false
  end


  (* max Size is not used for the PPC span dependency analysis. *)
  fun maxSize _ = error "maxSize"

  fun minSize(I.COPY _) = 0
    | minSize(I.FCOPY _) = 0
    | minSize _ = 4
  
  fun sdiSize(instr, regmap, labmap, loc) = let
    fun signed16 n = ~32768 <= n andalso n < 32768
    fun signed14 n = ~8192 <= n andalso n < 8192
    fun unsigned16 n = 0 <= n andalso n < 65536
    fun unsigned5 n = 0 <=n andalso n < 32

    fun operand(I.LabelOp le, inRange, lo, hi) = 
         if inRange(LE.valueOf le) then lo else hi
      | operand(I.ConstOp c, inRange, lo, hi) = 
	 if inRange(Const.valueOf c) then lo else hi
      | operand _ = error "sdiSize:operand"
  in
    case instr
    of I.L{d, ...} => operand(d, signed16, 4, 8)
     | I.ST{d, ...} => operand(d, signed16, 4, 8)
     | I.ARITHI{oper, im, ...} => 
       (case oper
	of I.ADD => operand(im, signed16, 4, 8)
	 | (I.ADDS | I.SUBF | I.MULL) => operand(im, signed16, 4, 12)
	 | (I.AND | I.OR    | I.XOR | I.XORS) => operand(im, unsigned16, 4, 12)
	 | (I.SLW | I.SRW | I.SRAW) => operand(im, unsigned5, 4, 12)
         | _ => error "sdiSize:ARITHI"
        (*esac*))
     | I.ROTATE{sh, ...} => error "sdiSize:ROTATE"
     | I.COMPARE{cmp, rb, ...} => 
       (case cmp
	of I.CMP => operand(rb, signed16, 4, 12)
         | I.CMPL => operand(rb, unsigned16, 4, 12)
       (*esac*))
     | I.BC{addr=I.LabelOp lexp, ...} => 
        if signed14((LE.valueOf lexp - loc) div 4) then 4 else 8
     | I.COPY{impl=ref(SOME l), ...} => 4 * length l
     | I.FCOPY{impl=ref(SOME l), ...} => 4 * length l
     | I.COPY{dst, src, impl as ref NONE, tmp} => let
	 val lookup = Intmap.map regmap
	 val instrs = 
	   Shuffle.shuffle{regMap=lookup, temp=tmp, dst=dst, src=src}
       in impl := SOME instrs; 4 * length instrs
       end
    | I.FCOPY{dst, src, impl as ref NONE, tmp} => let
	val lookup = Intmap.map regmap
        val instrs = 
	  Shuffle.shufflefp{regMap=lookup, temp=tmp, dst=dst, src=src}
      in impl := SOME(instrs); 4 * length instrs
      end
    | _ => error "sdiSize"
  end


  fun valueOf(I.ConstOp c) = Const.valueOf c
    | valueOf(I.LabelOp lexp) = LE.valueOf lexp
    | valueOf _ = error "valueOf"

  fun split opnd = let
    val i = valueOf opnd
    val w = Word.fromInt i
    val hi = Word.~>>(w, 0w16)
    val lo = Word.andb(w, 0w65535)
    val (high,low) = 
      if lo <  0w32768 then (hi, lo) else (hi+0w1, lo-0w65536)
  in (Word.toIntX high, Word.toIntX low)
  end

  fun expand(instr, size) = 
   (case instr
    of I.L{sz, rt, ra, d, mem} =>
       (case size
	of 4 => [I.L{sz=sz, rt=rt, ra=ra, d=I.ImmedOp(valueOf d), mem=mem}]
         | 8 => let
	     val (hi,lo) = split d
	   in
	     [I.ARITHI{oper=I.ADDS, rt=rt, ra=ra, im=I.ImmedOp hi},
	      I.L{sz=sz, rt=rt, ra=rt, d=I.ImmedOp lo, mem=mem}]
	   end
	 | _ => error "expand:L"
       (*esac*))
     | I.ST{sz, rs, ra, d, mem} =>
       (case size 
	of 4 => [I.ST{sz=sz, rs=rs, ra=ra, d=I.ImmedOp(valueOf d), mem=mem}]
         | 8 => let
	       val (hi,lo) = split d
	     in
	       [I.ARITHI{oper=I.ADDS, rt=C.asmTmpR, ra=ra, im=I.ImmedOp hi},
		I.ST{sz=sz, rs=rs, ra=C.asmTmpR, d=I.ImmedOp lo, mem=mem}]
	     end
	 | _ => error "expand:ST"
       (*esac*))
     | I.COPY{impl=ref(SOME l), ...} => l
     | I.FCOPY{impl=ref(SOME l), ...} => l
     | I.ARITHI{oper, rt, ra, im} => 
       (case size
	of 4 => [I.ARITHI{oper=oper, rt=rt, ra=ra, im=I.ImmedOp(valueOf im)}]
	 | 8 => let
	     val (hi, lo) = split im
           in
	    [I.ARITHI{oper=I.ADDS, rt=rt, ra=ra, im=I.ImmedOp hi},
	     I.ARITHI{oper=I.ADD, rt=rt, ra=rt, im=I.ImmedOp lo}]
           end
	 | 12 => let
	     val (hi,lo) = split im
	   in
	    [I.ARITHI{oper=I.ADDS, rt=C.asmTmpR, ra=0, im=I.ImmedOp hi},
	     I.ARITHI{oper=I.ADD, rt=C.asmTmpR, ra=C.asmTmpR, im=I.ImmedOp lo},
	     I.ARITH{oper=oper, rt=rt, ra=ra, rb=C.asmTmpR, OE=false, Rc=false}]
           end
       (*esac*))
     | I.BC{bo, bf, bit, fall, addr, LK} => 
       (case size
	 of 4 => [instr]
          | 8 => let
	      val newBO = 
		(case bo 
		 of I.TRUE => I.FALSE
	          | I.FALSE => I.TRUE
		  | I.ALWAYS => error "expand:newBO:BC"
		  | I.COUNTER{eqZero, cond} => error "expand:newBO:COUNTER"
                (*esac*))
            in 
	      print("emiting long form of branch"  ^ "\n");
	     [I.BC{bo=newBO, bf=bf, bit=bit, addr=fall, fall=fall, LK=false},
	      I.B{addr=addr, LK=LK}]
	    end
	  | _ => error "expand:BC"
       (*esac*))
     (* The other span dependent instructions are not generated *)
     | I.COMPARE _ => error "expand:COMPARE"
     | _ => error "expand"
  (*esac*))
end
