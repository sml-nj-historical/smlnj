(* X86Cells.sml --- interface for x86 registers.
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *)

structure X86Cells : X86CELLS = 
struct
  structure S = SortedList
  type register = int
  type regmap = register Intmap.intmap
  datatype cellclass = GP | FP | CC | MEM | CTRL

  exception Cells
  fun error msg = MLRiscErrorMsg.impossible ("X86Cells."^msg)

  val eax = 0	val ecx = 1				
  val edx = 2	val ebx = 3				
  val esp = 4   val ebp = 5
  val esi = 6   val edi = 7

  val stackptrR = esp
  val asmTmpR = ~1			(* not used  *)
  val fasmTmp = ~1			(* '' *)

  val firstPseudo = 256
  fun downto0 0 = [0]
    | downto0 n = n::downto0(n-1)
  val physicalRegs = downto0 31

  val counter = ref firstPseudo
  val regCnt = ref 0			(* number of general registers *)
  val fregCnt = ref 0			(* number of floating registers *)

  fun bump (r as ref c) = c before r := c+1

  fun newReg() = (bump regCnt; bump counter)
  fun newFreg() = (bump fregCnt; bump counter)
  fun newCCreg() = error "newCCreg"

  fun newCell GP = newReg
    | newCell FP = newFreg
    | newCell _ = fn () => bump counter

  fun resetRegs() = let
    val regmap = Intmap.new(64, Cells)
    val enter = Intmap.add regmap
  in
    counter:=firstPseudo; 
    regCnt :=0; 
    fregCnt:=0; 
    app (fn r => enter(r,r)) physicalRegs;
    regmap
  end

  fun maxCell () = !counter
  fun numCell GP = (fn () => !regCnt)
    | numCell FP = (fn () => !fregCnt)
    | numCell _ = raise Cells

  fun cellToString(r,class) = prefix class^Int.toString r
  and prefix GP   = "r"
    | prefix FP   = "f"
    | prefix CC   = "cc"
    | prefix MEM  = "m"
    | prefix CTRL = "ctrl"

  fun zero _ = NONE

  type cellset = int list * int list * int list 
  fun cellset2string(regs, fregs, ccregs) = let
    val cc = "cc=" :: map (fn 0 => "cc ") ccregs
    val gp = "gp=" :: map (fn r => (" $" ^ Int.toString r)) regs
    val fp = " fp=" :: map (fn f => (" $f" ^ Int.toString f)) fregs
  in String.concat(cc @ (gp @ fp))
  end
  val empty = ([], [], [])
  fun addReg(r, (rc, fc, cc)) = (S.enter(r,rc), fc, cc)
  fun addFreg(f, (rc, fc, cc)) = (rc, S.enter(f,fc), cc)
  fun addCCreg(0, (rc, fc, cc)) = (rc, fc, [0])
  fun addCell GP = addReg
    | addCell FP = addFreg
    | addCell CC = addCCreg
    | addCell _ = raise Cells

  fun cellsetToRegs(regmap, (rc, fc, cc)) = let
    fun lookup r = Intmap.map regmap r handle _ => r
    fun ftrans r = let
      val f = lookup r
    in if f < 8 then f+8 else f
    end
    fun cctrans r = let
      val cc = lookup r
    in if cc<1 then cc+16 else cc
    end
  in map cctrans cc @ map ftrans fc @ map lookup rc
  end

end
