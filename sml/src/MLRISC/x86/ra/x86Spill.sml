(* X86Spill.sml 
 *
 * X86 spilling is complicated business. 
 * Allen: and it just got more complicated; now we have to recognize the regmap.
 *)
signature X86SPILL = sig
  structure I : X86INSTR
  val spill :  
    I.instruction * (I.C.cell -> I.C.cell) * I.C.cell * I.operand -> 
      {code:I.instruction list, proh:I.C.cell list, newReg:I.C.cell option}

  val reload : 
    I.instruction * (I.C.cell -> I.C.cell) * I.C.cell * I.operand -> 
      {code:I.instruction list, proh:I.C.cell list, newReg:I.C.cell option}

  val fspill :  
    I.instruction * (I.C.cell -> I.C.cell) * I.C.cell * I.operand -> 
      {code:I.instruction list, proh:I.C.cell list, newReg:I.C.cell option}

  val freload : 
    I.instruction * (I.C.cell -> I.C.cell) * I.C.cell * I.operand -> 
      {code:I.instruction list, proh:I.C.cell list, newReg:I.C.cell option}
end


functor X86Spill(structure Instr: X86INSTR
                 structure Props: INSN_PROPERTIES where I = Instr
		) : X86SPILL = struct

  structure I  = Instr
  structure C  = I.C

  fun error msg = MLRiscErrorMsg.impossible("X86Spill: "^ msg)

  fun immed(I.Immed _) = true
    | immed(I.ImmedLabel _) = true
    | immed(I.Const _) = true
    | immed(I.LabelEA _) = true
    | immed _ = false

  fun immedOrReg(I.Direct r) = true
    | immedOrReg(I.Immed _) = true
    | immedOrReg(I.ImmedLabel _) = true
    | immedOrReg(I.Const _) = true
    | immedOrReg(I.LabelEA _) = true
    | immedOrReg _ = false

  (* Annotate instruction *)
  fun mark(instr,[]) = instr
    | mark(instr,a::an) = mark(I.ANNOTATION{i=instr,a=a},an)

  val newReg = C.newReg

  (* XXX:: Need to go through all the cases where 'done' is used
   * to make sure that a the src cannot contain the register
   * being spilled.
   *)
  fun spill(instr, regmap, reg, spillLoc) = 
  let fun done(instr, an) = {code=[mark(instr, an)], proh=[], newReg=NONE}
      fun spillIt(instr,an) =
      case instr of 
        I.CALL(addr, defs, uses, mem) =>
          done(I.CALL(addr, C.rmvReg(reg,defs), uses, mem), an)
      | I.MOVE{mvOp=I.MOVZX, src, dst} => 
          let val tmpR = newReg() val tmp = I.Direct tmpR
          in  {proh=[tmpR], newReg=SOME tmpR,
               code=[mark(I.MOVE{mvOp=I.MOVZX, src=src, dst=tmp}, an),
                     I.MOVE{mvOp=I.MOVL, src=tmp, dst=spillLoc}]
              }
          end
      | I.MOVE{mvOp, src as I.Direct rs, dst} =>
          if regmap rs=reg then {code=[], proh=[], newReg=NONE}
	  else done(I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}, an)
      | I.MOVE{mvOp, src, dst=I.Direct _} => 
	  if Props.eqOpn(src, spillLoc) then {code=[], proh=[], newReg=NONE}
          else if immed src then 
             done(I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}, an)
	  else 
          let val tmpR = newReg()
              val tmp  = I.Direct tmpR
          in  {proh=[tmpR],
               newReg=SOME tmpR,
               code=[mark(I.MOVE{mvOp=mvOp, src=src, dst=tmp}, an),
                     I.MOVE{mvOp=mvOp, src=tmp, dst=spillLoc}]
              }
          end 
      | I.LEA{addr, r32} => 
          let val tmpR = newReg()
          in  {proh=[tmpR],
               newReg=SOME tmpR,
               code=[mark(I.LEA{addr=addr, r32=tmpR}, an),
                     I.MOVE{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}]
              }
          end 
      | I.BINARY{binOp, src, dst} => (* note: dst = reg *)
          if immedOrReg src then  
             {proh=[],
              code=[(* I.MOVE{mvOp=I.MOVL, src=dst, dst=spillLoc}, XXX *)
	            mark(I.BINARY{binOp=binOp, src=src, dst=spillLoc}, an)
                   ],
              newReg=NONE
             }
          else 
	  let val tmpR = newReg()
              val tmp  = I.Direct tmpR
	  in  {proh=[tmpR],
               code=[(* I.MOVE{mvOp=I.MOVL, src=dst, dst=spillLoc}, XXX *)
                     I.MOVE{mvOp=I.MOVL, src=src, dst=tmp},
	             mark(I.BINARY{binOp=binOp, src=tmp, dst=spillLoc}, an)
                    ],
               newReg=NONE
              }
	  end
      | I.MULTDIV _ => error "spill: MULTDIV"
      | I.MUL3{src1, src2, dst} => 
          let val tmpR = newReg() 
          in  {proh=[tmpR], newReg=SOME tmpR,
               code=[mark(I.MUL3{src1=src1, src2=src2, dst=tmpR}, an),
                     I.MOVE{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}]
              }
          end
      | I.UNARY{unOp, opnd} => done(I.UNARY{unOp=unOp, opnd=spillLoc}, an)
      | I.POP _ => done(I.POP spillLoc, an)
      | I.COPY _ => error "spill: COPY"
      | I.FNSTSW  => error "spill: FNSTSW"
      | I.ANNOTATION{i,a} => spillIt(i, a::an)
      | _ => error "spill"
  in  spillIt(instr, [])
  end (* spill *)

  fun reload(instr, regmap, reg, spillLoc) = 
  let fun operand(rt, opnd) =
      (case opnd
       of I.Direct r => if regmap r=reg then I.Direct rt else opnd
        | I.Displace{base, disp, mem} => 
	   if regmap base=reg then I.Displace{base=rt, disp=disp, mem=mem} 
           else opnd
	| I.Indexed{base=NONE, index, scale, disp, mem=mem} => 
	   if regmap index=reg then
	     I.Indexed{base=NONE, index=rt, scale=scale, disp=disp, mem=mem}
	   else opnd
	| I.Indexed{base as SOME b, index, scale, disp, mem=mem} => 
	   if regmap b=reg then 
	     operand(rt, I.Indexed{base=SOME rt, index=index, 
				   scale=scale, disp=disp, mem=mem})
	   else if regmap index=reg then
	     I.Indexed{base=base, index=rt, scale=scale, disp=disp, mem=mem}
		else opnd
	| opnd => opnd
      (*esac*))

    fun done(instr, an) = {code=[mark(instr, an)], proh=[], newReg=NONE}

    (* This version assumes that the value of tmpR is killed *)
    fun withTmp(f, an) = 
    let val tmpR = newReg()
    in  {newReg=NONE,
         proh=[tmpR], 
         code=[I.MOVE{mvOp=I.MOVL, src=spillLoc, dst=I.Direct tmpR}, 
               mark(f tmpR, an)
              ]
        }
    end

    (* This version assumes that the value of tmpR is available afterwards *)
    fun withTmp'(f, an) = 
    let val tmpR = newReg()
        val tmp  = I.Direct tmpR
    in  {newReg=SOME tmpR,
         proh=[tmpR], 
         code=[I.MOVE{mvOp=I.MOVL, src=spillLoc, dst=I.Direct tmpR}, 
               mark(f tmpR, an)
              ]
        }
    end

    fun reloadIt(instr, an) =
    case instr
    of I.JMP(I.Direct _, labs) => done(I.JMP(spillLoc, labs), an)
     | I.JMP(opnd, labs) => withTmp(fn t => I.JMP(operand(t, opnd), labs), an)
     | I.JCC{opnd=I.Direct _, cond} => done(I.JCC{opnd=spillLoc, cond=cond}, an)
     | I.JCC{opnd, cond} => 
          withTmp(fn t => I.JCC{opnd=operand(t,opnd), cond=cond}, an)
     | I.CALL(opnd, defs, uses, mem) => 
       let val tmpR = newReg()
       in  {proh=[tmpR],
            newReg=NONE,
	    code=[mark(
                   I.CALL(operand(tmpR, opnd), defs, C.rmvReg(reg,uses), mem),
                  an)]
           }
       end
     | I.MOVE{mvOp, src, dst as I.Direct _} => 
 	withTmp'(fn t =>I.MOVE{mvOp=mvOp, src=operand(t, src), dst=dst},an)
     | I.MOVE{mvOp, src as I.Direct _, dst} => 
	if Props.eqOpn(dst, spillLoc) then {code=[], proh=[], newReg=NONE}
	else withTmp
	  (fn t => 
	     I.MOVE{mvOp=mvOp, src=operand(t, src), dst=operand(t, dst)}, an)
     | I.MOVE{mvOp, src, dst} => 
	withTmp
	 (fn t => 
	    I.MOVE{mvOp=mvOp, src=operand(t, src), dst=operand(t, dst)}, an)
     | I.LEA{r32, addr} => 
	withTmp'(fn tmpR => I.LEA{r32=r32, addr=operand(tmpR, addr)}, an)
     | I.CMP{lsrc, rsrc} => 
	withTmp(fn tmpR => 
          I.CMP{lsrc=operand(tmpR, lsrc), rsrc=operand(tmpR, rsrc)}, an)
     | I.BINARY{binOp, src, dst as I.Direct _} => 
	withTmp(fn tmpR => 
            I.BINARY{binOp=binOp, src=operand(tmpR, src), dst=dst}, an)
     | I.BINARY{binOp, src, dst} => 
	withTmp(fn tmpR => I.BINARY{binOp=binOp, src=operand(tmpR, src), 
                                                 dst=operand(tmpR, dst)}, an)
     | I.MULTDIV{multDivOp, src} => 
	withTmp(fn tmpR => 
            I.MULTDIV{multDivOp=multDivOp, src=operand(tmpR, src)}, an)
     | I.MUL3{src1, src2, dst} => 
	withTmp(fn tmpR => 
          I.MUL3{src1=operand(tmpR, src1), src2=src2, 
		 dst=if regmap dst = reg then error "reload:MUL3" else dst}, an)
     | I.UNARY{unOp, opnd} => 
	withTmp'(fn tmpR => I.UNARY{unOp=unOp, opnd=operand(tmpR, opnd)}, an)
     | I.PUSH arg => withTmp(fn tmpR => I.PUSH(operand(tmpR, arg)), an)
     | I.COPY _ => error "reload:COPY"
     | I.FILD opnd => withTmp'(fn tmpR => I.FILD(operand(tmpR, opnd)), an)
     | I.FLD opnd => withTmp'(fn tmpR => I.FLD(operand(tmpR, opnd)), an)
     | I.FSTP opnd => withTmp'(fn tmpR => I.FSTP(operand(tmpR, opnd)), an)
     | I.FBINARY{binOp, src, dst} => 
	withTmp'(fn tmpR => 
	         I.FBINARY{binOp=binOp, src=operand(tmpR, src), dst=dst}, an)
				     
     | I.ANNOTATION{i,a} => reloadIt(i, a::an)
     | _ => error "reload"
  in reloadIt(instr, [])
  end (*reload*)

  fun fspill(instr, regmap, reg, spillLoc) = 
  let fun spillIt(instr, an) = 
      (case instr of 
         I.FSTP _ => {proh=[], code=[mark(I.FSTP spillLoc, an)], newReg=NONE}
       | I.CALL(opnd, defs, uses, mem) =>
	 {proh=[],
	  code=[mark(I.CALL(opnd, C.rmvFreg(reg,defs), uses, mem), an)],
	  newReg=NONE}
       | I.ANNOTATION{i,a} => spillIt(i, a::an)
       | _ => error "fspill"
      (*esac*))
  in  spillIt(instr, []) 
  end (* fspill *)

  fun freload(instr, regmap, reg, spillLoc) = 
  let fun reloadIt(instr, an) = 
      (case instr of 
         I.FLD opnd => {code=[mark(I.FLD spillLoc, an)], proh=[], newReg=NONE}
       | I.FBINARY{binOp, src=I.FDirect f, dst} => 
	   if regmap f = reg then 
	     {code=[mark(I.FBINARY{binOp=binOp, src=spillLoc, dst=dst}, an)],
	      proh=[], 
              newReg=NONE}
	   else error "freload:FBINARY"
       | I.CALL(opnd, defs, uses, mem) =>
	 {proh=[],
	  code=[mark(I.CALL(opnd, C.rmvFreg(reg,defs), uses, mem), an)],
          newReg=NONE}
       | I.ANNOTATION{i,a} => reloadIt(i, a::an)
       | _  => error "freload"
      (*esac*))
  in  reloadIt(instr, [])
  end (* freload *)

end
