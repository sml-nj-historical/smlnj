(*
 * This has been upgraded to V9.
 *)

architecture Sparc =
struct

   superscalar    

   big endian

   lowercase assembly

   version "V9"

   instruction delayslot 4  

   storage
     GP = 32 cells of 64 bits in cellset called "register" assembly as 
                   (fn r => if r < 8 then "%g"^Int.toString r
                            else if r = 14 then "%sp"
                            else if r < 16 then "%o"^Int.toString(r-8)
                            else if r < 24 then "%l"^Int.toString(r-16)
                            else if r = 30 then "%fp"
                            else if r < 32 then "%i"^Int.toString(r-24) 
                            else "%r"^Int.toString r) 
    | FP = 32 cells of 32 bits in cellset called "floating point registers"
             assembly as (fn f => "%f"^Int.toString f)
    | Y  = 1 cell of 64 bits called "multiplication register" assembly as "%y"
    | PSR = 1 cell of 64 bits in cellset called "process status register"
         assembly as (fn 0 => "%psr"
                       | n => "%psr"^Int.toString n)
    | FSR = 1 cell of 64 bits called "floating point status register"
         assembly as (fn 0 => "%fsr"
                       | n => "%fsr"^Int.toString n)
    | CC = cells of 64 bits in cellset GP called "conditional code register"
         assembly as "%cc"

   locations
       stackptrR = $GP[14]
   and asmTmpR   = $GP[10]    (* %o2 *)
   and linkReg   = $GP[15]
   and fasmTmp   = $FP[30]
   and y         = $Y[0]
   and psr       = $PSR[0]
   and fsr       = $FSR[0]

   structure Cells =
   struct
      fun zeroReg GP = SOME($GP[0])
        | zeroReg _  = NONE
   end

   semantics Semantics =
   struct
      include "MD++/rtl.md"
      open Basis
      val save : action   (* undefined action *)
      val restore : action   (* undefined action *)
      val trap : exp -> action (* undefined action *)
      fun andn(x,y) = notb(x && y)
      fun orn(x,y) = notb(x || y)
      fun xnor(x,y) = notb(x ^^ y)
      val tadd : exp * exp -> exp
      val taddtv : exp * exp -> exp
      val tsub : exp * exp -> exp
      val tsubtv : exp * exp -> exp
      val indcall : exp -> exp 
      val call : 'a -> exp 
      val CC : cond -> exp
      val FCC : operator 
      val FBRANCH : action
      fun fcmp (x,y) = binop(FCC,x,y)
   end 

   structure Instruction = struct
   datatype load : op3!  = LDSB  0b001001 [[ load8s ]]
                         | LDSH  0b001010 [[ load16s ]]
                         | LDUB  0b000001 [[ load8u ]]
                         | LDUH  0b000010 [[ load16u ]]
                         | LD    0b000000 [[ load32s ]]
                         | LDX   0b001011 (* v9 *)
                         | LDD   0b000011 [[ load64s ]]
   datatype store : op3! = STB   0b000101 [[ store8 ]]
                         | STH   0b000110 [[ store16 ]]
                         | ST    0b000100 [[ store32 ]]
                         | STX   0b001110 (* v9 *)
                         | STD   0b000111 [[ store64 ]]
   datatype fload : op3! = LDF    0b100000 [[ load32s ]]
                         | LDDF   0b100011 [[ load64s ]]
                         | LDQF   0b100010 (* v9 *)
                         | LDFSR  0b100001 (* rd = 0 *)
                         | LDXFSR 0b100001 (* v9 *) (* rd = 1 *)
   datatype fstore : op3! = STF   0b100100 [[ store32 ]]
                          | STDF  0b100111 [[ store64 ]]
                          | STFSR 0b100101 
   datatype arith : op3! = AND      0b000001  [[ andb ]]
                         | ANDCC    0b010001  [[ andb ]]
                         | ANDN     0b000101  [[ andn ]]
                         | ANDNCC   0b010101  [[ andn ]]
                         | OR       0b000010  [[ orb ]]
                         | ORCC     0b010010  [[ orb ]]
                         | ORN      0b000110  [[ orn ]]
                         | ORNCC    0b010110  [[ orn ]]
                         | XOR      0b000011  [[ xorb ]]
                         | XORCC    0b010011  [[ xorb ]]
                         | XNOR     0b000111  [[ xnor ]]
                         | XNORCC   0b010111  [[ xnor ]]
                         | ADD      0b000000  [[ add ]]
                         | ADDCC    0b010000  [[ add ]]
                         | TADD     0b100000  [[ tadd ]]
                         | TADDCC   0b110000  [[ tadd ]]
                         | TADDTV   0b100010  [[ taddtv ]]
                         | TADDTVCC 0b110010  [[ taddtv ]]
                         | SUB      0b000100  [[ sub ]]
                         | SUBCC    0b010100  [[ sub ]]
                         | TSUB     0b100001  [[ tsub ]]
                         | TSUBCC   0b110001  [[ tsub ]]
                         | TSUBTV   0b100011  [[ tsubtv ]]
                         | TSUBTVCC 0b110011  [[ tsubtv ]]
                         | UMUL     0b001010  [[ mulu ]]
                         | UMULCC   0b011010  [[ mulu ]]
                         | SMUL     0b001011  [[ muls ]]
                         | SMULCC   0b011011  [[ muls ]]
                         | UDIV     0b001110  [[ divu ]]
                         | UDIVCC   0b011110  [[ divu ]]
                         | SDIV     0b001111  [[ divs ]]
                         | SDIVCC   0b011111  [[ divs ]]
                         (* v9 extensions *)
                         | MULX     0b001001
                         | SDIVX    0b101101
                         | UDIVX    0b001101
                                  (* op3, x *)
   datatype shift : op3! = SLL     (0wb100101,0w0)  [[ sll ]]
                         | SRL     (0wb100110,0w0)  [[ srl ]]
                         | SRA     (0wb100111,0w0)  [[ sra ]]
                         (* v9 extensions *)
                         | SLLX    (0wb100101,0w1)
                         | SRLX    (0wb100110,0w1)
                         | SRAX    (0wb100111,0w1)
   datatype farith1 : opf! = FiTOs 0b011000100 
                           | FiTOd 0b011001000 
                           | FiTOq 0b011001100
                           | FsTOi 0b011010001
                           | FdTOi 0b011010010
                           | FqTOi 0b011010011
                           | FsTOd 0b011001001
                           | FsTOq 0b011010101
                           | FdTOs 0b011000110
                           | FdTOq 0b011001110
                           | FqTOs 0b011000111
                           | FqTOd 0b011001011
                           | FMOVs 0b000000001 [[ fn x => x ]]
                           | FNEGs 0b000000101 [[ fnegs ]]
                           | FABSs 0b000001001 [[ fabss ]]
                           | FMOVd | FNEGd | FABSd (* composite instructions *)
                           | FMOVq | FNEGq | FABSq (* composite instructions *)
                           | FSQRTs 0b000101001 [[ fsqrts ]]
                           | FSQRTd 0b000101010 [[ fsqrtd ]]
                           | FSQRTq 0b000101011 [[ fsqrtq ]]
   datatype farith2 :opf! = FADDs  0b001000001 [[ fadds ]]
                          | FADDd  0b001000010 [[ faddd ]]
                          | FADDq  0b001000011 [[ faddq ]]
                          | FSUBs  0b001000101 [[ fsubs ]]
                          | FSUBd  0b001000110 [[ fsubd ]]
                          | FSUBq  0b001000111 [[ fsubq ]]
                          | FMULs  0b001001001 [[ fmuls ]]
                          | FMULd  0b001001010 [[ fmuld ]]
                          | FMULq  0b001001011 [[ fmulq ]]
                          | FsMULd 0b001101001 
                          | FdMULq 0b001101110
                          | FDIVs  0b001001101 [[ fdivs ]]
                          | FDIVd  0b001001110 [[ fdivd ]]
                          | FDIVq  0b001001111 [[ fdivq ]]
   datatype fcmp : opf!   = FCMPs  0b001010001 [[ fcmp ]]
                          | FCMPd  0b001010010
                          | FCMPq  0b001010011
                          | FCMPEs 0b001010101
                          | FCMPEd 0b001010110
                          | FCMPEq 0b001010111

   datatype branch [0..15] : cond! = 
        BN   "n"   [[ I.NE ]]
      | BE   "e"   [[ I.EQ ]]
      | BLE  "le"  [[ I.LE ]]
      | BL   "l"   [[ I.LT ]]
      | BLEU "leu" [[ I.LEU ]]
      | BCS  "cs"  
      | BNEG "neg" 
      | BVS  "vs"  
      | BA   ""   
      | BNE  "ne"  
      | BG   "g"   
      | BGE  "ge"  
      | BGU  "gu"  
      | BCC  "cc"  
      | BPOS "pos" 
      | BVC  "vs"

   datatype rcond! = (* V9 integer conditions *)
        RZ    0b001
      | RLEZ  0b010
      | RLZ   0b011
      | RNZ   0b101
      | RGZ   0b110
      | RGEZ  0b111

   datatype cc = (* V9 condition register *) 
       ICC 0b00
     | XCC 0b10

   datatype prediction! = (* V9 branch prediction bit *)
        PT | PN

   datatype fbranch [0..15] : cond! = 
        FBN  
      | FBNE 
      | FBLG 
      | FBUL
      | FBL   
      | FBUG 
      | FBG  
      | FBU
      | FBA "fb"
      | FBE  
      | FBUE 
      | FBGE 
      | FBUGE 
      | FBLE 
      | FBULE
      | FBO

   datatype ea = Direct of $GP
               | FDirect of $GP
               | Displace of {base: $GP, disp: int}

        (* used to encode the opf_low field V9 *)
   datatype fsize! = S 0b00100
                   | D 0b00110
                   | Q 0b00111 

   datatype operand =
      REG of $GP          	``<GP>''		[[ GP ]]
    | IMMED of int            	``<int>''		[[ const int ]]
    | LAB of LabelExp.labexp	``<emit_labexp labexp>'' 
    | LO of LabelExp.labexp	``%lo(<emit_labexp labexp>)''
    | HI of LabelExp.labexp  	``%hi(<emit_labexp labexp>)''
    | CONST of Constant.const	``<emit_const const>''
   end 

   functor Assembly(val V9 : bool) = 
   struct
      (* Some helper functions for assembly generation *)
      fun emit_leaf false = () | emit_leaf true  = emit "l"
      fun emit_nop false = () | emit_nop true = emit "\n\tnop"
      fun emit_a false = () | emit_a true  = emit ",a"
      fun emit_cc false = () | emit_cc true = emit "cc"
   end

   instruction formats 32 bits
      (* Extract the value of an operand *)
      opn{i} =  
      let fun hi22 w = (itow w) ~>> 0w10
          fun lo10 w = (itow w) at [0..9] 
      in  case i of
           I.REG rs2 => error "opn"
         | I.IMMED i => itow i
         | I.LAB l   => itow(LabelExp.valueOf l)
         | I.LO l    => lo10(LabelExp.valueOf l)
         | I.HI l    => hi22(LabelExp.valueOf l)
         | I.CONST c => itow(Constant.valueOf c)
      end

      (* basic formats, integer source registers, target type not determined.*)
   |  rr {op1:2, rd:5, op3:6, rs1:GP 5, i:1=0, asi:8=0, rs2:GP 5}
   |  ri {op1:2, rd:5, op3:6, rs1:GP 5, i:1=1, simm13:signed 13}
   |  rix{op1,op3,r,i,d} = 
        (case i of
            I.REG rs2 => rr{op1,op3,rs1=r,rs2=rs2,rd=d}
         |  _ =>         ri{op1,op3,rs1=r,rd=d,simm13=opn{i}}
        )

      (* GP + imm/GP -> GP *)
   |  rir{op1,op3,r,i,d:GP} = rix{op1,op3,r,i,d}
      (* GP + imm/GP -> FP *)
   |  rif{op1,op3,r,i,d:FP} = rix{op1,op3,r,i,d}

       (* formats found in the Sparc architecture manual *)
   |  load{l:load,r,i,d} = rir{op1=0w3,op3=l,r,i,d}    (* p90 *)
   |  store{s:store,r,i,d} = rir{op1=0w3,op3=s,r,i,d}  (* p95 *)
   |  fload{l:fload,r,i,d} = rif{op1=0w3,op3=l,r,i,d}  (* p92 *)
   |  fstore{s:fstore,r,i,d} = rif{op1=0w3,op3=s,r,i,d} (* p97 *)
   |  sethi {op1:2=0, rd:GP 5, op2:3=0b100, imm22:int signed 22}  (* p104 *)
   |  NOP   {op1:2=0, rd:5=0, op2:3=0b100, imm22:22=0} (* p105 *)
   |  delay {nop} = if nop then NOP{} else () (* delay slot *)
   |  arith {a:arith,r,i,d} =                       (* p106 *)
        rir{op1=0w2,op3=a,r,i,d} 

   |  shiftr {op1:2=2, rd:5, op3:6, rs1:5, i:1=0, x:1, asi:7=0, rs2:GP 5}
   |  shifti {op1:2=2, rd:5, op3:6, rs1:5, i:1=1, x:1, asi:6=0, cnt:signed 6}
   |  shift {s:shift,r:GP,i,d:GP} = 
        let val (op3,x) = s
        in  case i of
              I.REG rs2 => shiftr{op3,rs1=r,rs2=rs2,rd=d,x=x}  (* p218 v9 *)
            | _         => shifti{op3,rs1=r,cnt=opn{i},rd=d,x=x}
        end
   |  save {r,i,d} = rir{op1=0w2,op3=0wb111100,r,i,d} (* p117 *)
   |  restore {r,i,d} = rir{op1=0w2,op3=0wb111101,r,i,d} (* p117 *)
   |  bicc{op1:2=0,a:bool 1, b:branch 4, op2:3=0b010, disp22:signed 22}
   |  fbfcc{op1:2=0,a:bool 1, b:fbranch 4, op2:3=0b110, disp22:signed 22}
   |  call {op1:2=1, disp30:signed 30}                (* p125 *)
   |  jmpl {r,i,d} = rir{op1=0w2,op3=0wb111000,r,i,d} (* p126 *)

   |  ticcr {op1:2, rd:5, op3:6, rs1:GP 5, i:1=0, cc:cc 2, _:6=0, rs2:GP 5}
   |  ticci {op1:2, rd:5, op3:6, rs1:GP 5, i:1=1, cc:cc 2, _:4=0, 
             sw_trap:signed 7}
   |  ticcx{op1,op3,cc,r,i,d} = 
        (case i of
            I.REG rs2 => ticcr{op1,op3,cc,rs1=r,rs2=rs2,rd=d}
         |  _ =>         ticci{op1,op3,cc,rs1=r,rd=d,sw_trap=opn{i}}
        )
   |  ticc {t:branch,cc,r,i} = 
         ticcx{op1=0w2,d=t,op3=0wb111010,cc,r,i} (* p237 (V9) *)

   |  rdy {op2:2=2,d:GP 5,op3:6=0b101000,rs1:5=0,x:0..13=0} (* p131 *)
   |  wdy {r,i} = rix{op1=0w2,op3=0wb110000,r,i,d=0w0} (* p133 *)

        (* one input floating point format *)
   |  fop_1 {op1:2=2, d:5, op3:6=0b110100, rs1:5=0, a:9, r:5}
   |  fop1 {a:farith1,r:FP,d:FP} = fop_1{a,r,d}

        (* generate composite instruction *)
   |  fdouble{a:farith1,r:FP,d:FP} = 
          (fop_1{a,r,d}; 
           fop_1{a=0w1,r=r+0w1,d=d+0w1}
          )
   |  fquad{a:farith1,r:FP,d:FP} = 
          (fop_1{a,r,d}; 
           fop_1{a=0w1,r=r+0w1,d=d+0w1};
           fop_1{a=0w1,r=r+0w2,d=d+0w2}; 
           fop_1{a=0w1,r=r+0w3,d=d+0w3}
          )

        (* two inputs floating point format *)
   |  fop2 {op1:2=2, d:FP 5, op3:6=0b110100, r1:FP 5, a:farith2 9, r2:FP 5}
   |  fcmp {op1:2=2, rd:25..29=0, op3:6=0b110101, rs1:FP 5, opf:fcmp 9,rs2:FP 5}

      (* conditional moves formats (V9) *)
   |  cmovr{op1:2=2,op3:6,rd:5,cc2:1,cond:4,i:1=0,cc1:1,cc0:1,_:6=0,rs2:5}
   |  cmovi{op1:2=2,op3:6,rd:5,cc2:1,cond:4,i:1=1,cc1:1,cc0:1,simm11:signed 11}
   |  cmov{op3,cond,cc2,cc1,cc0,i,rd} =
        (case i of
            I.REG rs2 => cmovr{op3,cond,rs2=emit_GP rs2,rd,cc0,cc1,cc2}
         |  _ => cmovi{op3,cond,rd,cc0,cc1,cc2,simm11=opn{i}}
        )

   |  movicc {b:branch,i,d:GP} =
        cmov{op3=0wb101100,cond=b,i,rd=d,cc2=0w1,cc1=0w0,cc0=0w0}
   |  movfcc {b:fbranch,i,d:GP} = (* use fcc0 *)
        cmov{op3=0wb101100,cond=b,i,rd=d,cc2=0w0,cc1=0w0,cc0=0w0}
   |  fmovicc{sz:fsize,b:branch,r:FP,d:FP} =
        cmovr{op3=0wb101100,cond=b,rs2=r,rd=d,cc2=0w1,cc1=0w0,cc0=0w0}
   |  fmovfcc{sz:fsize,b:fbranch,r:FP,d:FP} = (* use fcc0 *)
        cmovr{op3=0wb101100,cond=b,rs2=r,rd=d,cc2=0w0,cc1=0w0,cc0=0w0}

        (* move integer register on register condition format *)
   |  movrr {op1:2=2, rd:GP 5, op3:6=0b101111, rs1:GP 5, i:1=0, rcond:3, 
             asi:5=0, rs2:GP 5}
   |  movri {op1:2=2, rd:GP 5, op3:6=0b101111, rs1:GP 5, i:1=1, rcond:3, 
             simm10:signed 10}
   |  movr{rcond:rcond,r,i,d} =
        (case i of
           I.REG rs2 => movrr{rcond,rs1=r,rs2=rs2,rd=d}
         | _ =>         movri{rcond,rs1=r,rd=d,simm10=opn{i}}
        ) 

   structure MC = 
   struct
      (* this computes the displacement address *)
      fun disp label = itow((Label.addrOf label - !loc)) ~>> 0w2
   end

   (*
    * Notation:
    *   r -- source register
    *   i -- source operand (immed or register)
    *   d -- destination register (or data register in store instructions)
    *)
   instruction 
      LOAD of { l:load, d: $GP, r: $GP, i:operand, mem:Region.region }
	``<l>\t[<r>+<i>], <d><mem>'' 
	load{l,r,i,d}
	[[ d := load l (r+operand i) ]]

   |  STORE of { s:store, d: $GP, r: $GP, i:operand, mem:Region.region }
	``<s>\t<d>, [<r>+<i>]<mem>'' 
	store{s,r,i,d}
	[[ store s (d,r+ operand i) ]]

   |  FLOAD of { l:fload, r: $GP, i:operand, d: $FP, mem:Region.region }
	``<l>\t[<r>+<i>], <d><mem>'' 
	fload{l,r,i,d}
        [[ d := fload l (r+operand i) ]]

   |  FSTORE of { s:fstore, d: $FP, r: $GP, i:operand, mem:Region.region }
	``<s>\t[<r>+<i>], <d><mem>'' 
	fstore{s,r,i,d}
        [[ fstore s (d,r+operand i) ]]

   |  SETHI of { i:int, d: $GP } 
        ``sethi\t%hi(0x<emit(Word32.toString(Word32.<<(Word32.fromInt i,0w10)))>), <d>'' 
	sethi{imm22=i,rd=d}
	[[ d := I.CONST i << I.CONST 10 ]]

   |  ARITH of { a:arith, r: $GP, i:operand, d: $GP }
	``<a>\t<r>, <i>, <d>'' 
	arith{a,r,i,d}
        [[ d := arith a (r,operand i) ]]

   |  SHIFT of { s:shift, r: $GP, i:operand, d: $GP }
	``<s>\t<r>, <i>, <d>'' 
	shift{s,r,i,d}
       [[ d := shift s (r,operand i) ]]

      (* Conditional moves! *)
   |  MOVicc of {b:branch,  i:operand, d: $GP } (* V9 *)
        ``mov<b>\t<i>, <d>'' 
	movicc{b,i,d}

   |  MOVfcc of {b:fbranch, i:operand, d: $GP } (* V9 *) 
        ``mov<b>\t<i>, <d>''  
	movfcc{b,i,d}

   |  MOVR of {rcond:rcond, r: $GP, i: operand, d: $GP} (* V9 *)
       ``movr<rcond>\t<r>, <i>, <d>''
       movr{rcond,r,i,d}

   |  FMOVicc of {sz:fsize, b:branch, r: $FP, d: $FP } (* V9 *)
	``fmov<sz><b>\t<r>, <d>''
	fmovicc{sz,b,r,d}

   |  FMOVfcc of {sz:fsize, b:fbranch, r: $FP, d: $FP } (* V9 *)
	``fmov<sz><b>\t<r>, <d>''
	fmovfcc{sz,b,r,d}

   |  Bicc of  { b:branch, a:bool, label:Label.label, nop:bool}
	``b<b><a>\t<label><nop>'' 
	(bicc{b,a,disp22=disp label}; delay{nop})
	[[ I.BRANCH(CC(branch b)) ]]
        padded when nop = true
	nullified when a = true andalso (case b of I.BA => false | _ => true)
	when nullified nodelayslot else delayslot
 	candidate of delayslot never

   |  FBfcc of { b:fbranch, a:bool, label:Label.label, nop:bool }
	``<b><a>\t<label><nop>'' 
	(fbfcc{b,a,disp22=disp label}; delay{nop})
	[[ FBRANCH ]]
        padded when nop = true
	nullified when a = true
	when nullified nodelayslot else delayslot
 	candidate of delayslot never

       (* V9 branch on condition in integer register *)
   |  BR of {rcond:rcond, p:prediction, r: $GP, a:bool, 
             label:Label.label, nop:bool} 
        ``b<rcond><a><p>\t<r>, <label><nop>''

        (* V9 branch on integer condition code with prediction *)
   |  BP of {b:branch, p:prediction, cc:cc, a:bool, label:Label.label,nop:bool}
        ``bp<b><a><p>\t%<emit(if cc = I.ICC then "i" else "x")>cc, <label><nop>''
   |  JMP  of { r: $GP, i:operand, labs : Label.label list, nop:bool}
	``jmp\t[<r>+<i>]<nop>'' 
	(jmpl{r,i,d=0}; delay{nop})
	[[ I.JMP(r+operand i) ]]
        padded when nop = true
	never nullified 
 	candidate of delayslot never

   |  JMPL of { r: $GP, i:operand, d: $GP, 
                defs:C.cellset, uses:C.cellset, nop:bool, mem:Region.region
              }
	``jmpl\t[<r>+<i>], <d><mem><nop>'' 
	(jmpl{r,i,d}; delay{nop})
	[[ d := indcall(r+operand i) ]]
        padded when nop = true
	never nullified 
 	candidate of delayslot never

   |  CALL of  { defs:C.cellset, uses:C.cellset, 
                 label:Label.label, nop:bool, mem:Region.region
               }  
	``call\t<label><mem><nop>'' 
	(call{disp30=disp label}; delay{nop})
	[[ $GP[15] := call label ]]
        padded when nop = true
	never nullified 
 	candidate of delayslot never

        (* Note, for V8 the cc bit must be ICC *)
   |  Ticc of { t:branch, cc:cc, r: $GP, i:operand} 
	``t<t>\t<if cc = I.ICC then () else emit "%xcc, "><r>, <i>'' 
	ticc{t,r,cc,i}
        [[ trap(r+operand i) ]]
 	candidate of delayslot never

   |  FPop1 of { a:farith1, r: $FP, d: $FP }
        ``<let fun f(a,r,d) = 
                 (emit(a); emit "\t"; emit_FP(r); emit ", "; emit_FP(d))
               fun g(a,r,d) =
                  let val r = regmap r and d = regmap d
                  in  f(a,r,d); emit "\n\t"; 
                      f("fmovs",r+1,d+1) 
                  end
               fun h(a,r,d) =
                  let val r = regmap r and d = regmap d
                  in  f(a,r,d); emit "\n\t"; 
                      f("fmovs",r+1,d+1); emit "\n\t";
                      f("fmovs",r+2,d+2); emit "\n\t";
                      f("fmovs",r+3,d+3)
                  end
           in if V9 then f(asm_farith1 a,r,d)
              else
              case a of
                I.FMOVd => g("fmovs",r,d)
              | I.FNEGd => g("fnegs",r,d)
              | I.FABSd => g("fabss",r,d)
              | I.FMOVq => h("fmovs",r,d)
              | I.FNEGq => h("fnegs",r,d)
              | I.FABSq => h("fabss",r,d)
              | _       => f(asm_farith1 a,r,d)
           end>''
	(case a of
           (* composite instructions *)
           I.FMOVd => fdouble{a=I.FMOVs,r,d}
         | I.FNEGd => fdouble{a=I.FNEGs,r,d}
         | I.FABSd => fdouble{a=I.FABSs,r,d}
         | I.FMOVq => fquad{a=I.FMOVs,r,d}
         | I.FNEGq => fquad{a=I.FNEGs,r,d}
         | I.FABSq => fquad{a=I.FABSs,r,d}
	 | _ 	   => fop1{a,r,d}
	)
	[[ d := farith1 a r ]]

   |  FPop2 of { a:farith2, r1: $FP, r2: $FP, d: $FP }
	``<a>\t<r1>, <r2>, <d>'' 
	fop2{a,r1,r2,d}
	[[ d := farith2 a (r1,r2) ]]

   |  FCMP of  { cmp:fcmp, r1: $FP, r2: $FP, nop:bool }
	``<cmp>\t<r1>, <r2><nop>'' 
	(fcmp{opf=cmp,rs1=r1,rs2=r2}; delay{nop})
	[[ $CTRL[0] := fcmp cmp (r1,r2) ]]
        padded when nop = true
	never nullified 
 	candidate of delayslot never

   |  COPY of { dst: $GP list, src: $GP list, 
	        impl:instruction list option ref, tmp:ea option}
         ``<emitInstrs (Shuffle.shuffle{regmap,tmp,src,dst})>''
	[[ I.COPY(dst,src) ]]

   |  FCOPY of { dst: $FP list, src: $FP list, 
                 impl:instruction list option ref, tmp:ea option}
         ``<emitInstrs (Shuffle.shufflefp{regmap,tmp,src,dst})>''
        [[ I.COPY(dst,src) ]]

   |  SAVE of {r: $GP, i:operand, d: $GP}
	 ``save\t<r>, <i>, <d>'' 
	save{r,i,d}
	[[ d := r+ operand i; save ]]

   |  RESTORE of {r: $GP, i:operand, d: $GP}
	 ``restore\t<r>, <i>, <d>''  
	restore{r,i,d}
	[[ d := r+operand i; restore ]]

   |  RDY of {d: $GP}		
	``rd\t%y, <d>''
	rdy{d}
	[[ d := $Y[0] ]]

   |  WRY of {r: $GP,i:operand}	
	``wr\t<r>, <i>, %y''
	wdy{r,i}
	[[ $Y[0] := r+operand i ]]

   |  RET of {leaf:bool,nop:bool} 
	 ``ret<leaf><nop>'' 
	(jmpl{r=if leaf then 31 else 15,i=I.IMMED 8,d=0}; delay{nop})
	[[ I.RET ]]
        padded when nop = true
	never nullified 
 	candidate of delayslot never

   |  ANNOTATION of {i:instruction, a:Annotations.annotation}
        ``<(emitInstr i; comment(Annotations.toString a))>''
        (emitInstr i)

   |  GROUP of Annotations.annotation
        ``<comment(Annotations.toString annotation)>''

end
