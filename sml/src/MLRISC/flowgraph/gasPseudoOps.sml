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

  fun toString(PB.ALIGN_SZ n)     = Fmt.format "\t.align\t%d" [Fmt.INT n]
    | toString(PB.ALIGN_ENTRY)    = "\t.align\t4"	(* 16 byte boundary *)
    | toString(PB.ALIGN_LABEL)    = "\t.p2align\t4,,7"

    | toString(PB.DATA_LABEL lab) = Label.fmt labFmt lab ^ ":"
    | toString(PB.DATA_READ_ONLY) = "\t.section\t.rodata"
    | toString(PB.DATA)	          = "\t.data"
    | toString(PB.BSS)		  = "\t.section\t.bss"
    | toString(PB.TEXT)	          = "\t.text"
    | toString(PB.SECTION at)     = "\t.section\t" ^ Atom.toString at

    | toString(PB.REORDER)        = ""
    | toString(PB.NOREORDER)      = ""

    | toString(PB.INT{sz, i})     = let
	fun join [] = []
	  | join [lexp] = [lexpToString lexp]
	  | join (lexp::r) = lexpToString lexp :: "," :: join r
	val pop = (case sz
	       of 8 => "\t.byte\t"
		| 16 => "\t.short\t"
		| 32 => "\t.int\t"
		| 64 => error "INT64"
	      (* end case *))
	in
	  String.concat (pop :: join i)
	end

    | toString(PB.ASCII s)        =
	Fmt.format "\t.ascii\t\"%s\"" [Fmt.STR(String.toCString s)]
    | toString(PB.ASCIIZ s)       = 
        Fmt.format "\t.asciz \"%s\"" [Fmt.STR(String.toCString s)]

    | toString(PB.SPACE sz)	  = Fmt.format "\t.space\t%d" [Fmt.INT sz]

    | toString(PB.FLOAT{sz, f})   = let
	fun join [] = []
	  | join [f] = [f]
	  | join (f::r) = f :: "," :: join r
	val pop = (case sz
	       of 32 => "\t.single "
		| 64 => "\t.double "
		| 128 => "\t.extended "
	      (* end case *))
	in
	  String.concat (pop :: join f)
	end

    | toString(PB.IMPORT labs) = decls("\t.extern\t%s", labs)
    | toString(PB.EXPORT labs) = decls("\t.global\t%s", labs)
    | toString(PB.COMMENT txt) = Fmt.format "/* %s */" [Fmt.STR txt]
        

    | toString(PB.EXT _) = error "EXT"

end
