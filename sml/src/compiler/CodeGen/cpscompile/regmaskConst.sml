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
 * $Log: regmaskConst.sml,v $
 * Revision 1.4  1997/09/29 20:58:44  george
 * *** empty log message ***
 *
 * Revision 1.3  1997/07/17  12:37:21  george
 *   The constant type used to specialize MLTrees is now done more compactly.
 *
 * Revision 1.2  1997/06/11  19:15:42  george
 *   Fixed bad printing of register lists.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:34  george
 *   Version 109.24
 *
 *)
