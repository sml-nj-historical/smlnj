
structure PPCCells : PPCCELLS = struct
  structure SL = SortedList
  exception Cells
  type register = int
  type regmap = register Intmap.intmap

  datatype cellclass =
      GP | FP | CC| MEM | CTRL	      (* required components*)
    | LR | CTR			      (* ppc specific *)

 
  val stackptrR = 1
  val asmTmpR = 28			
  val fasmTmp = 0
  val lr = 72

  val firstPseudo	= 256

  val counter = ref firstPseudo
  val regCnt = ref 0
  val fregCnt = ref 0
  val ccregCnt = ref 0
  fun bump (r as ref c) = (r := c+1; c)
  fun downto0 0 = [0]
    | downto0 n = n::downto0(n-1)
  val physicalRegs = downto0 31

  fun newReg () = (bump regCnt; bump counter)
  fun newFreg() = (bump fregCnt; bump counter)
  fun newCCreg () = (bump ccregCnt; bump counter)

  fun resetRegs() = let 
    val regmap = Intmap.new(64, Cells)
    val enter = Intmap.add regmap
  in
    counter:=firstPseudo; 
    regCnt :=0; 
    fregCnt:=0; 
    ccregCnt:=0;
    app (fn r => enter(r,r)) physicalRegs;
    regmap
  end

  fun zero _ = NONE

  fun newCell GP  = newReg
    | newCell FP  = newFreg
    | newCell CC  = newCCreg
    | newCell _   = (fn () => bump counter)

  fun numCell GP = (fn () => !regCnt)
    | numCell FP = (fn () => !fregCnt)
    | numCell CC = (fn () => !ccregCnt)
    | numCell _  = raise Cells

  fun maxCell () = !counter

  fun cellToString(r, _) = Int.toString r

  type cellset = register list * register list * register list
  val empty = ([],[],[])

  fun cellset2string(regs, fregs, ccregs) = let
    val gp = "gp="::map Int.toString regs
    val fp = "fp="::map Int.toString fregs
    val cc = "cc="::map Int.toString ccregs
  in String.concat(gp @ fp @cc)
  end
    
  fun addReg(r, (rc,fc, cc)) = (SL.enter(r,rc), fc, cc)
  fun addFreg(f, (rc,fc, cc)) = (rc, SL.enter(f, fc), cc)
  fun addCCreg(c, (rc,fc, cc)) = (rc, fc, SL.enter(c,cc))

  fun addCell GP = addReg
    | addCell FP = addFreg
    | addCell CC = addCCreg
    | addCell _  = raise Cells

  (* mapping:
      0..31   gp
      32..63  fp
      64..71  cc
      72      lr
   *)
  fun cellsetToRegs(regmap, (regs,fregs,ccregs)) = let 
    val lookup = Intmap.map regmap 
    fun trans (K, R) r = let
      val r = lookup r handle _ => r
    in if r < K then r+R else r
    end
    val rtrans = map (trans (32, 0)) regs
    val ftrans = map (trans (32, 32)) fregs
    val cctrans = map (trans (8, 64)) ccregs
  in cctrans @ (ftrans @ rtrans)
  end
end