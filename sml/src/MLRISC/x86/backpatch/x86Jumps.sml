(* X86Jumps.sml --- information to resolve jumps for runtime code generation.
 *
 *  COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Jumps
  (structure Instr : X86INSTR
   structure Shuffle : X86SHUFFLE where I = Instr
   structure MCEmitter : MC_EMIT where I = Instr) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure LE = I.LabelExp

  fun error msg = MLRiscErrorMsg.error("X86Jumps",msg)

  val esp = 4
  val ebp = 5
  val branchDelayedArch = false

  fun imm8 i = ~128 <= i andalso i < 128

  fun isSdi instr = let
    fun operand(I.ImmedLabel _) = true
      | operand(I.LabelEA _) = true
      | operand(I.Displace{disp, ...}) = operand disp
      | operand(I.Indexed{disp, ...}) = operand disp
      | operand _ = false
    fun cmptest{lsrc, rsrc} = operand lsrc orelse operand rsrc
  in 
    case instr
    of I.JMP(opnd, _) => operand opnd
     | I.JCC{opnd, ...} => operand opnd
     | I.BINARY{src, dst, ...} => operand src orelse operand dst
     | I.MOVE{src, dst, ...} => operand src orelse operand dst
     | I.LEA{addr, ...} => operand addr
     | ( I.CMPL arg | I.CMPW arg | I.CMPB arg 
       | I.TESTL arg | I.TESTW arg | I.TESTB arg) => cmptest arg
     | I.MULTDIV{src, ...} => operand src
     | I.MUL3{src1, ...} => operand src1
     | I.UNARY{opnd, ...} => operand opnd
     | I.SET{opnd, ...} => operand opnd
     | I.CMOV{src, dst, ...} => operand src 
     | (I.PUSHL opnd | I.PUSHW opnd | I.PUSHB opnd) => operand opnd
     | I.POP opnd =>  operand opnd
     | I.FSTPT opnd => operand opnd
     | I.FSTPL opnd => operand opnd
     | I.FSTPS opnd => operand opnd
     | I.FLDT opnd => operand opnd
     | I.FLDL opnd => operand opnd
     | I.FLDS opnd => operand opnd
     | I.FBINARY{src, dst, ...} => operand src orelse operand dst
     | I.FILD opnd => operand opnd
     | I.ANNOTATION{i,...} => isSdi i
     | _ => false
  end

  fun minSize(I.JMP _) = 2
    | minSize(I.JCC _) = 2
    | minSize(I.ANNOTATION{i,...}) = minSize i
    | minSize _ = 1

  fun maxSize _ = 12

  (* value of span-dependent operand *)
  fun operand(I.ImmedLabel le) = LE.valueOf le
    | operand(I.LabelEA le) = LE.valueOf le
    | operand _ = error "operand"

  fun sdiSize(instr, regmap, labmap, loc) = let
    fun branch(opnd, short, long) = let
      val offset = operand opnd - loc
    in if imm8(offset - 2) then short else long
    end
  
    val encode = MCEmitter.emitInstr
  in
    case instr
    of I.JMP(opnd, _) => branch(opnd, 2, 5)
     | I.JCC{opnd, ...} => branch(opnd, 2, 6)
     | I.ANNOTATION{i,...} => sdiSize(i, regmap, labmap, loc)
     | _ => Word8Vector.length(encode(instr, regmap))
  end  (*sdiSize*)

  fun expand(instr, size, loc) =
    case instr 
    of I.JMP(opnd, labs)  => [I.JMP(I.Relative(operand opnd-loc), labs)]
     | I.JCC{cond, opnd} => 
        [I.JCC{cond=cond, opnd=I.Relative(operand opnd-loc)}]
     | I.ANNOTATION{i,...} => expand(i, size, loc)
     | opnd => [opnd]
end

