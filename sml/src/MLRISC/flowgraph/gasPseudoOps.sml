(* gasPseudoOps.sml
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Laboratories.
 *
 * Implements the string related functions to emit pseudo-ops
 * in the standard GAS syntax.
 *)
signature GAS_PSEUDO_OPS = sig
    structure T : MLTREE
    val lexpToString : T.labexp -> string
    val toString : (T.labexp, 'a) PseudoOpsBasisTyp.pseudo_op -> string
    val defineLabel : Label.label -> string
end

functor GasPseudoOps
  ( structure T : MLTREE
    val labFmt : {gPrefix: string, aPrefix: string}
   ) : GAS_PSEUDO_OPS =
struct
  structure T = T
  structure PB = PseudoOpsBasisTyp
  structure Fmt = Format

  fun error msg = MLRiscErrorMsg.error ("GasPseudoOps.", msg)

  fun prIntInf i = if IntInf.sign i < 0 then "-"^IntInf.toString(IntInf.~ i) 
                   else IntInf.toString i

  fun prInt i = if i < 0 then "-"^Int.toString(~i) else Int.toString i

  (* operator precedences:
     Note: these differ from C's precedences
	  2 MULT, DIV, LSHIFT, RSHIFT
	  1 AND, OR
	  0 PLUS, MINUS
  *)

  fun parens (str, prec, op_prec) = 
      if prec > op_prec then "(" ^ str ^ ")" else str

  fun lexpToString le = toStr(le, 0)

  and toStr(T.LABEL lab, _) = Label.fmt labFmt lab 
    | toStr(T.LABEXP le, p) = toStr(le, p)
    | toStr(T.CONST c, _) = 
        (prInt(T.Constant.valueOf c) handle _ => T.Constant.toString c)
    | toStr(T.LI i, _) = prIntInf i
    | toStr(T.MULS(_,lexp1, lexp2), _) = toStr(lexp1, 2) ^ "*" ^ toStr(lexp2,2)
    | toStr(T.DIVS(_,lexp1, lexp2), _) =  toStr(lexp1, 2) ^ "/" ^ toStr(lexp2,2)
    | toStr(T.SLL(_,lexp, cnt), prec) = toStr(lexp,2) ^ "<<" ^ toStr(cnt,2)
    | toStr(T.SRL(_,lexp, cnt), prec) = toStr(lexp,2) ^ ">>" ^ toStr(cnt,2)
    | toStr(T.ANDB(_,lexp, mask), prec) = 
        parens(toStr(lexp,1) ^ "&" ^ toStr(mask, 1), prec, 1)
    | toStr(T.ORB(_,lexp, mask), prec) = 
        parens(toStr(lexp, 1) ^ "|" ^ toStr(mask, 1), prec, 1)
    | toStr(T.ADD(_,lexp1, lexp2), prec) = 
        parens(toStr(lexp1, 0) ^ "+" ^ toStr(lexp2, 0), prec, 0)
    | toStr(T.SUB(_,lexp1, lexp2), prec) = 
        parens(toStr(lexp1, 0) ^ "-" ^ toStr(lexp2, 0), prec, 0)
    | toStr _ = error "toStr"

  fun defineLabel lab = lexpToString (T.LABEL lab) ^ ":"

  fun decls (fmt, labs) =
    String.concat 
      (map (fn lab => (Fmt.format fmt [Fmt.STR (lexpToString(T.LABEL lab))])) labs)

  fun toString(PB.ALIGN_SZ n)     = Fmt.format ".align %d" [Fmt.INT n]
    | toString(PB.ALIGN_ENTRY)    = ".align 4"			(* 16 byte boundary *)
    | toString(PB.ALIGN_LABEL)    = ".p2align 4,,7"

    | toString(PB.DATA_LABEL lab) = Label.fmt labFmt lab ^ ":"
    | toString(PB.DATA_READ_ONLY) = ".section      .rodata"
    | toString(PB.DATA)	          = ".data"
    | toString(PB.TEXT)	          = ".text"
    | toString(PB.SECTION at)     = ".section     " ^ Atom.toString at

    | toString(PB.REORDER)        = ""
    | toString(PB.NOREORDER)      = ""

    | toString(PB.INT{sz, i})     = 
        String.concat
           ((case sz
	     of 8  => ".byte "
              | 16 => ".short "
              | 32 => ".int "
              | 64 => error "INT64"
	    (*esac*)) :: map (fn lexp => lexpToString lexp ^ " ") i)

    | toString(PB.ASCII s)        = Fmt.format ".ascii \"%s\"" [Fmt.STR s]
    | toString(PB.ASCIIZ s)       = Fmt.format ".asciiz \"%s\"" [Fmt.STR s]

    | toString(PB.FLOAT{sz, f})   = 
         String.concat 
	   ((case sz
	     of 32 => ".single "
	      | 64 => ".double "
	      | 128 => ".extended "
            (*easc*)) :: f)

    | toString(PB.IMPORT labs) = decls(".global %s\n", labs)
    | toString(PB.EXPORT labs) = decls(".extern %s\n", labs)

    | toString(PB.EXT _) = error "EXT"

end