(* X86Jumps.sml --- information to resolve jumps for runtime code generation.
 *
 *  COPYRIGHT (c) 1997 Bell Laboratories.
 *)

functor X86Jumps
  (structure Instr : X86INSTR
   structure AsmEmitter : EMITTER_NEW where I = Instr
   structure Shuffle : X86SHUFFLE where I = Instr
   structure MCEmitter : MC_EMIT where I = Instr) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure LE = LabelExp

  fun error msg = MLRiscErrorMsg.impossible ("X86Jumps." ^ msg)

  val esp = 4
  val ebp = 5
  val branchDelayedArch = false

  fun imm8 i = ~128 <= i andalso i < 128

  fun isSdi instr = let
    fun operand(I.ImmedLabel _) = true
      | operand(I.Const _) = true
      | operand(I.LabelEA _) = true
      | operand(I.Displace{disp, ...}) = operand disp
      | operand(I.Indexed{disp, ...}) = operand disp
      | operand _ = false
  in 
    case instr
    of I.JMP(opnd, _) => operand opnd
     | I.JCC{opnd, ...} => operand opnd
     | I.BINARY{src, dst, ...} => operand src orelse operand dst
     | I.MOVE{src, dst, ...} => operand src orelse operand dst
     | I.LEA{addr, ...} => operand addr
     | I.CMP{lsrc, rsrc} => operand lsrc orelse operand rsrc
     | I.MULTDIV{src, ...} => operand src
     | I.MUL3{src1, ...} => operand src1
     | I.UNARY{opnd, ...} => operand opnd
     | I.PUSH opnd => operand opnd
     | I.POP opnd =>  operand opnd
     | I.FSTP opnd => operand opnd
     | I.FLD opnd => operand opnd
     | I.FBINARY{src, dst, ...} => operand src orelse operand dst
     | I.FILD opnd => operand opnd
     | _ => false
  end

  fun minSize(I.JMP _) = 2
    | minSize(I.JCC _) = 2
    | minSize _ = 1

  fun maxSize _ = 12

  (* value of span-dependent operand *)
  fun operand(I.ImmedLabel le) = LE.valueOf le
    | operand(I.Const c) = Const.valueOf c
    | operand(I.LabelEA le) = LE.valueOf le
    | operand _ = error "operand"

  fun sdiSize(instr, regmap, labmap, loc) = let
    fun branch(opnd, short, long) = let
      val offset = operand opnd - loc
    in if imm8(offset - 2) then short else long
    end
  
    val encode = MCEmitter.emitInstr
    fun lookup r = Intmap.map regmap r handle _ => r
  in
    case instr
    of I.JMP(opnd, _) => branch(opnd, 2, 5)
     | I.JCC{opnd, ...} => branch(opnd, 2, 6)
     | _ => Word8Vector.length(encode(instr, regmap))
  end  (*sdiSize*)

  fun expand(instr, _, loc) =
    case instr 
    of I.JMP(opnd, labs)  => [I.JMP(I.Relative(operand opnd-loc), labs)]
     | I.JCC{cond, opnd} => 
        [I.JCC{cond=cond, opnd=I.Relative(operand opnd-loc)}]
     | opnd => [opnd]
end

