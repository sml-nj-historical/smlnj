(* regmaskConst.sml --- the only special constant used is a list of registers.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

functor RegMaskConst(structure RegMask : REGMASK) : CONST_TYPE = 
struct
  datatype root = Reg of int | Mem of int
  datatype const = REGLIST of root list * int Intmap.intmap

  fun toString(REGLIST(regs, regmap)) = let
    val lookup = Intmap.map regmap
    fun doRegs(Reg(r)::rest, acc) = let
          val reg = lookup r handle _ => r
        in doRegs(rest, "r" ^  Int.toString reg::acc)
        end
      | doRegs([], acc) = "("::acc
  in concat(doRegs(regs, [")"]))
  end

  fun valueOf(REGLIST(regs, regmap)) = let
    val lookup = Intmap.map regmap
    fun add(Reg r, mask) = RegMask.regMask(lookup r, mask)
      | add(Mem m, mask) = RegMask.memMask(m, mask)
  in Word.toIntX (List.foldl add 0w0 regs)
  end
end

(*
 * $Log$
 *)
