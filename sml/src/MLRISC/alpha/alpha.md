(* 
 * This machine description now includes 64-bit and single precision 
 * floating point support.  The description
 * is copied from the book ``Alpha Architecture Reference Manual'' edited
 * by Richard L. Sites, Digital Press, 1992.
 *
 * -- Allen Leung
 *)
architecture Alpha =
struct

   name "Alpha"

   superscalar

   little endian    

   lowercase assembly

   (*
    * This specify the cells interface 
    *)
   storage
     GP "r" = 32 cells of 64 bits in cellset called "register" 
              assembly as (fn (30,_) => "$sp"
                            | (r,_)  => "$"^Int.toString r
                          )
              zero 31 
   | FP "f" = 32 cells of 64 bits in cellset called "floating point register" 
              assembly as (fn (f,_) => "$f"^Int.toString f)
              zero 31 
   | CC "c" = cells of 64 bits in cellset GP called "condition code register"
              assembly as "cc"
   | MEM "m" = cells of 8 bits called "memory"
               assembly as (fn (r,_) => "m"^Int.toString r)
   | CTRL "ctrl" = cells of 8 bits called "control"
               assembly as (fn (r,_) => "ctrl"^Int.toString r)

   locations
       stackptrR = $GP[30]
   and asmTmpR   = $GP[28]
   and fasmTmp   = $FP[30]

   structure RTL =
   struct
     include "MD-2.0/basis.md"
     open Basis
     infix 1 ||
     infix 2 := 
     infix 3 << >> ~>>

     rtl COPY{dst,src} = $r[forall dst] := $r[forall src]
     rtl FCOPY{dst,src} = $f[forall dst] := $f[forall src]

     (* How to align addresses *)
     fun align4 addr      = andb(addr,notb 3)
     fun align8 addr      = andb(addr,notb 7)
     fun align8Upper addr = orb(andb(addr,notb 7),4)

     fun % x  = (operand x : #64 bits)
     fun %% l = (label l : #64 bits)

     fun disp(b,d) = $r[b] + %d

     fun byte x  = (x : #8 bits)
     fun word x  = (x : #16 bits)
     fun dword x = (x : #32 bits)
     fun qword x  = (x : #64 bits)
     fun float x = (x : #32 bits)
     fun double x  = (x : #64 bits)

     rtl LDA{r,b,d} = $r[r] := $r[b] + %d
     rtl LDAH{r,b,d} = $r[r] := $r[b] + %d << 16

     (* Integer loads *)
     rtl LDBU{r,b,d,mem}   = $r[r] := zx (byte $m[disp(b,d):mem])
     rtl LDWU{r,b,d,mem}   = $r[r] := zx (word $m[disp(b,d):mem])
     rtl LDL{r,b,d,mem}    = $r[r] := sx (dword $m[disp(b,d):mem])
     rtl LDL_L{r,b,d,mem}  = $r[r] := sx (dword $m[align4(disp(b,d)):mem])
     rtl LDQ{r,b,d,mem}    = $r[r] := qword $m[disp(b,d):mem]
     rtl LDQ_L{r,b,d,mem}  = $r[r] := qword $m[align8(disp(b,d)):mem]
     rtl LDQ_U{r,b,d,mem}  = $r[r] := qword $m[align8Upper(disp(b,d)):mem]

     (* Integer stores *)
     rtl STB{r,b,d,mem}    = $m[disp(b,d):mem] := $r[r] at [0..7]
     rtl STW{r,b,d,mem}    = $m[disp(b,d):mem] := $r[r] at [0..15]
     rtl STL{r,b,d,mem}    = $m[disp(b,d):mem] := $r[r] at [0..31]
     rtl STQ{r,b,d,mem}    = $m[disp(b,d):mem] := $r[r] 
     rtl STQ_U{r,b,d,mem}  = $m[align8(disp(b,d)):mem] := $r[r] 

     (* Floating point loads *)
     rtl LDF{r,b,d,mem} = $f[r] := sx (float $m[disp(b,d):mem])
     rtl LDG{r,b,d,mem} = $f[r] := double $m[disp(b,d):mem] 
     rtl LDS{r,b,d,mem} = $f[r] := double $m[disp(b,d):mem] 
     rtl LDT{r,b,d,mem} = $f[r] := double $m[disp(b,d):mem]

     (* Floating point stores *)
     rtl STF{r,b,d,mem} = $m[disp(b,d):mem] := float(sx $f[r])
     rtl STG{r,b,d,mem} = $m[disp(b,d):mem] := $f[r]
     rtl STS{r,b,d,mem} = $m[disp(b,d):mem] := $f[r]
     rtl STT{r,b,d,mem} = $m[disp(b,d):mem] := $f[r]

     (* Integer operators *)
     rtl ADDL{ra,rb,rc}   = $r[rc] := sx($r[ra] + %rb)
     rtl ADDQ{ra,rb,rc}   = $r[rc] := $r[ra] + %rb
     fun cmp oper {ra,rb,rc} = $r[rc] := cond(oper($r[ra],%rb),  1, 0)

     rtl [CMPBGE, CMPEQ, CMPLE, CMPLT, CMPULE, CMPULT] =
         map cmp [(>=), (==), (<=), (<), (leu), (ltu)]

     fun binop oper {ra,rb,rc} = $r[rc] := oper($r[ra], %rb)

     rtl SUBL{ra,rb,rc}   = $r[rc] := sx($r[ra] - %rb)
     rtl SUBQ{ra,rb,rc}   = $r[rc] := $r[ra] - %rb
     rtl S4ADDL{ra,rb,rc} = $r[rc] := sx($r[ra] << 2 + %rb)
     rtl S4ADDQ{ra,rb,rc} = $r[rc] := $r[ra] << 2 + %rb
     rtl S4SUBL{ra,rb,rc} = $r[rc] := sx($r[ra] << 2 - %rb)
     rtl S4SUBQ{ra,rb,rc} = $r[rc] := $r[ra] << 2 - %rb
     rtl S8ADDL{ra,rb,rc} = $r[rc] := sx($r[ra] << 3 + %rb)
     rtl S8ADDQ{ra,rb,rc} = $r[rc] := $r[ra] << 3 + %rb
     rtl S8SUBL{ra,rb,rc} = $r[rc] := sx($r[ra] << 3 - %rb)
     rtl S8SUBQ{ra,rb,rc} = $r[rc] := $r[ra] << 3 - %rb
     rtl AND{ra,rb,rc}    = $r[rc] := andb($r[ra], %rb)
     rtl BIC{ra,rb,rc}    = $r[rc] := andb($r[ra], notb(%rb)) (* XXX *)
     rtl BIS{ra,rb,rc}    = $r[rc] := orb($r[ra], %rb)
     rtl EQV{ra,rb,rc}    = $r[rc] := eqvb($r[ra], %rb)
     rtl ORNOT{ra,rb,rc}  = $r[rc] := orb($r[ra], notb(%rb))
     rtl XOR{ra,rb,rc}    = $r[rc] := xorb($r[ra], %rb)

     rtl extbl extlh extll extqh extql extwh extwl insbl inslh insll
         insqh insql inswh inswl mskbl msklh mskll mskqh mskql mskwh mskwl 
          : #n bits * #n bits -> #n bits
     rtl [EXTBL, EXTLH, EXTLL, EXTQH, EXTQL, EXTWH, EXTWL,
          INSBL, INSLH, INSLL, INSQH, INSQL, INSWH, INSWL,
          MSKBL, MSKLH, MSKLL, MSKQH, MSKQL, MSKWH, MSKWL] =
         map binop 
         [extbl, extlh, extll, extqh, extql, extwh, extwl,
          insbl, inslh, insll, insqh, insql, inswh, inswl,
          mskbl, msklh, mskll, mskqh, mskql, mskwh, mskwl]

     rtl [SLL, SRA, SRL] = 
          map binop [(<<), (~>>), (>>)]

     rtl zap zapnot umulh : #n bits * #n bits -> #n bits
     rtl ZAP{ra,rb,rc} = $r[rc] := zap($r[ra], %rb) 
     rtl ZAPNOT{ra,rb,rc} = $r[rc] := zapnot($r[ra], %rb) 
     rtl MULL{ra,rb,rc}  = $r[rc] := sx(muls($r[ra], %rb))
     rtl MULQ{ra,rb,rc}  = $r[rc] := muls($r[ra], %rb)
     rtl UMULH{ra,rb,rc} = $r[rc] := umulh($r[ra], %rb) 
     rtl SGNXL{ra,rb,rc} = ADDL{ra,rb,rc}

     (* Integer trapping operators *)
     val overflowtrap = ()
     rtl ADDLV{ra,rb,rc} = ADDL{ra,rb,rc} || overflowtrap
     rtl ADDQV{ra,rb,rc} = ADDQ{ra,rb,rc} || overflowtrap
     rtl SUBLV{ra,rb,rc} = SUBL{ra,rb,rc} || overflowtrap
     rtl SUBQV{ra,rb,rc} = SUBQ{ra,rb,rc} || overflowtrap
     rtl MULLV{ra,rb,rc} = MULL{ra,rb,rc} || overflowtrap
     rtl MULQV{ra,rb,rc} = MULQ{ra,rb,rc} || overflowtrap

     fun lbc(x,y) = andb(x,1) == y
     fun lbs(x,y) = andb(x,1) <> y

     val comparisons = [(==), lbc, lbs, (>=), (>), (<=), (<), (<>)]

     (* Conditional moves *)
     fun cmov oper {ra,rb,rc} = if oper($r[ra], 0) then $r[rc] := %rb else ()

     rtl CMOV ^^ [EQ, LBC, LBS, GE, GT, LE, LT, NE] =
         map cmov comparisons

     (* Integer branches *)
     rtl BR{lab} = Jmp(%%lab)
     rtl BSR{lab} = Call(%%lab)
     fun branch oper {r,lab} = if oper($r[r], 0) then Jmp(%%lab) else ()

     rtl [BEQ, BLBC, BLBS, BGE, BGT, BLE, BLT, BNE] =
         map branch comparisons
 
     (* Floating point operators *)
     rtl DEFFREG{FP} = $f[FP] := ?

     val SU = ()
     val SUD = ()
     fun farith oper {fa,fb,fc} = $f[fc] := oper($f[fa], $f[fb])
     fun funary oper {fb,fc} = $f[fc] := oper($f[fb])
     rtl fops as [ADDS, ADDT, SUBS, SUBT, MULS, MULT, DIVS, DIVT] =
         map farith
         [fadd, fadd, fsub, fsub, fmul, fmul, fdiv, fdiv]
     fun su fop {fa,fb,fc} = fop{fa,fb,fc} || SU
     fun sud fop {fa,fb,fc} = fop{fa,fb,fc} || SUD 
     rtl [ADDSSU, ADDTSU, SUBSSU, SUBTSU, MULSSU, MULTSU, DIVSSU, DIVTSU] =
         map su fops
     rtl [ADDSSUD, ADDTSUD, SUBSSUD, SUBTSUD, 
          MULSSUD, MULTSUD, DIVSSUD, DIVTSUD] =
         map sud fops

     rtl cpys  cpyse  cpysn mf_fpcr mt_fpcr : #64 bits * #64 bits -> #64 bits
     rtl [CPYS,CPYSE, CPYSN, MF_FPCR, MT_FPCR] =
         map farith [cpys, cpyse, cpysn, mf_fpcr, mt_fpcr]

     rtl cvtlq cvtql  cvtqlsv cvtqlv cvtqs cvtqsc 
         cvtqt cvtqtc cvtts  cvttsc 
         cvtst cvtsts cvttq  cvttqc : #64 bits -> #64 bits

     rtl [CVTLQ, CVTQL,  CVTQLSV, CVTQLV, CVTQS, CVTQSC,
          CVTQT, CVTQTC, CVTTS,  CVTTSC, 
          CVTST, CVTSTS, CVTTQ,  CVTTQC ] = 
          map funary 
         [cvtlq, cvtql,  cvtqlsv, cvtqlv, cvtqs, cvtqsc, 
          cvtqt, cvtqtc, cvtts,  cvttsc,
          cvtst, cvtsts, cvttq,  cvttqc]
 
     rtl teq tlt tle tun : #64 bits * #64 bits -> #64 bits
     rtl fcmps as [CMPTEQ, CMPTLT, CMPTLE, CMPTUN] = 
            map farith [teq, tlt, tle, tun]
     rtl [CMPTEQSU, CMPTLTSU, CMPTLESU, CMPTUNSU] = 
            map su fcmps

     (* Floating point branches *)
     fun fbranch oper {f,lab} = if oper($f[f],?) then Jmp(%%lab) else ()
     rtl [FBEQ, FBLT, FBLE, FBNE, FBGE, FBGT] =
         map fbranch [|==|,  |<|,  |<=|, |<>|, |>=|, |>|]

     (* Floating point moves *)
     fun fcmove cmp {fa,fb,fc} = if cmp($f[fa],?) then $f[fc] := $f[fb] else ()
     rtl FCMOV ^^ [EQ, LT, LE, NE, GE, GT] =
         map fcmove [|==|,  |<|,  |<=|, |<>|, |>=|, |>|]

     (* Call/return *)
     rtl JSR{r,b,defs,uses,mem} = 
         Jmp($r[b]) || 
         $r[r] := ? || 
         $cellset[defs] := $cellset[uses] ||
         $m[? :mem] := ($m[? :mem] : #8 bits)
     rtl RET{r,b} = Jmp($r[b]) || $r[r] := ?
     rtl JMPL{r,b} = Jmp($r[b]) || $r[r] := ?
     rtl TRAPB{} = ()

     (* Pseudo arithmetic *)
     fun pseudoOp oper {ra,rb,rc,tmps} = 
         $r[rc] := oper($r[ra], %rb) ||
         $cellset[tmps] := ? (* XXX *)
     rtl PSEUDOARITH_ ^^ [ DIVL, DIVLU, DIVQ, DIVQU,
                           REML, REMLU, REMQ, REMQU ] = 
         map pseudoOp    [ divs,  divu,  divs,  divu,   (* XXX *)
                           rems,  remu,  rems,  remu ]
     (* Pal code 
      * Note: I have no idea what these things are, so I'm just going
      * fake them
      *)
     rtl BPT BUGCHK CALLSYS GENTRAP IMB RDUNIQUE WRUNIQUE : #64 bits -> #64 bits
     fun CALL_PAL code {def,use} =
         Call(qword(code(qword 0))) || 
         $r[forall def] := $r[forall use]  (* XXX *)
     rtl CALL_PAL_ ^^ 
         [BPT, BUGCHK, CALLSYS, GENTRAP, IMB, RDUNIQUE, WRUNIQUE] =
         map CALL_PAL 
         [BPT, BUGCHK, CALLSYS, GENTRAP, IMB, RDUNIQUE, WRUNIQUE] 
   end (* RTL *)

   (* 
    * We now specify the instruction representation, assembly,
    * machine code encoding and ``semantics''
    *)
   structure Instruction = 
   struct
      datatype ea = 
          Direct of $GP 
        | FDirect of $FP         
        | Displace of {base: $GP, disp:int}
    
      datatype operand = 
          REGop of $GP                      ``<GP>''  rtl: $r[GP]
        | IMMop of int                      ``<int>'' rtl: immed int
        | HILABop of LabelExp.labexp        ``hi(<labexp>)''
        | LOLABop of LabelExp.labexp        ``lo(<labexp>)''
        | LABop of LabelExp.labexp          ``<labexp>''
   
      (* 
       * When I say ! after the datatype name XXX, it means generate a
       * function emit_XXX that converts the constructors into the corresponding
       * assembly text.  By default, it uses the same name as the constructor,
       * but may be modified by the lowercase/uppercase assembly directive.
       * 
       *)
      datatype branch! =  (* table C-2 *)
         BR   0x30  
                   | BSR 0x34  
                              | BLBC 0x38
       | BEQ  0x39 | BLT 0x3a | BLE  0x3b
       | BLBS 0x3c | BNE 0x3d | BGE  0x3e 
       | BGT  0x3f
   
      datatype fbranch! = (* table C-2 *)
                     FBEQ 0x31 | FBLT 0x32
       | FBLE 0x33             | FBNE 0x35
       | FBGE 0x36 | FBGT 0x37 
    
      datatype load! =  (* table C-1 *)
         LDBU  0x02 
       | LDWU  0x04
       | LDL   0x28    
       | LDL_L 0x2A  
       | LDQ   0x29    
       | LDQ_L 0x2B 
       | LDQ_U 0x0B
      datatype store! = STB 0x0E | STW 0x0D | STL 0x2C | STQ 0x2D | STQ_U 0x0F
      datatype fload [0x20..0x23] ! = LDF | LDG | LDS | LDT 
      datatype fstore [0x24..0x27] ! = STF | STG | STS | STT 
   
      (* non-trapping opcodes *) 
      datatype operate! = (* table C-5 *)
          ADDL  (0wx10,0wx00)                       | ADDQ (0wx10,0wx20) 
                              | CMPBGE(0wx10,0wx0f) | CMPEQ (0wx10,0wx2d) 
        | CMPLE (0wx10,0wx6d) | CMPLT (0wx10,0wx4d) | CMPULE (0wx10,0wx3d) 
        | CMPULT(0wx10,0wx1d) | SUBL  (0wx10,0wx09) 
        | SUBQ  (0wx10,0wx29) 
        | S4ADDL(0wx10,0wx02) | S4ADDQ (0wx10,0wx22) | S4SUBL (0wx10,0wx0b)
        | S4SUBQ(0wx10,0wx2b) | S8ADDL (0wx10,0wx12) | S8ADDQ (0wx10,0wx32)
        | S8SUBL(0wx10,0wx1b) | S8SUBQ (0wx10,0wx3b) 
   
        | AND   (0wx11,0wx00) | BIC    (0wx11,0wx08) | BIS    (0wx11,0wx20)
                                                     | EQV (0wx11,0wx48)
        | ORNOT (0wx11,0wx28) | XOR    (0wx11,0wx40)
   
        | EXTBL (0wx12,0wx06) | EXTLH  (0wx12,0wx6a) | EXTLL(0wx12,0wx26)
        | EXTQH (0wx12,0wx7a) | EXTQL  (0wx12,0wx36) | EXTWH(0wx12,0wx5a)
        | EXTWL (0wx12,0wx16) | INSBL  (0wx12,0wx0b) | INSLH(0wx12,0wx67)
        | INSLL (0wx12,0wx2b) | INSQH  (0wx12,0wx77) | INSQL(0wx12,0wx3b)
        | INSWH (0wx12,0wx57) | INSWL  (0wx12,0wx1b) | MSKBL(0wx12,0wx02)
        | MSKLH (0wx12,0wx62) | MSKLL  (0wx12,0wx22) | MSKQH(0wx12,0wx72)
        | MSKQL (0wx12,0wx32) | MSKWH  (0wx12,0wx52) | MSKWL(0wx12,0wx12)
        | SLL   (0wx12,0wx39) | SRA    (0wx12,0wx3c) | SRL  (0wx12,0wx34)
        | ZAP   (0wx12,0wx30) | ZAPNOT (0wx12,0wx31)
        | MULL  (0wx13,0wx00)                        | MULQ (0wx13,0wx20)
                              | UMULH  (0wx13,0wx30) 
        | SGNXL "addl" (0wx10,0wx00) (* same as ADDL *)
   
      (* conditional moves *) 
      datatype cmove! =
         CMOVEQ 0wx24 | CMOVLBC 0wx16 | CMOVLBS 0wx14
       | CMOVGE 0wx46 | CMOVGT  0wx66 | CMOVLE  0wx64
       | CMOVLT 0wx44 | CMOVNE  0wx26 
    
      datatype pseudo_op! = DIVL | DIVLU | DIVQ | DIVQU 
                          | REML | REMLU | REMQ | REMQU
    
      datatype operateV! = (* table C-5 opc/func *)
         ADDLV (0wx10,0wx40) | ADDQV (0wx10,0wx60)
       | SUBLV (0wx10,0wx49) | SUBQV (0wx10,0wx69) 
       | MULLV (0wx13,0wx40) | MULQV (0wx13,0wx60)
   
      datatype funary! = (* table C-6/C-7 *)
             (* C-6 *)
         CVTLQ   (0wx17,0wx010) | CVTQL (0wx17,0wx030) | CVTQLSV (0wx17,0wx530)
       | CVTQLV  (0wx17,0wx130)
   
             (* C-7 *)
       | CVTQS   (0wx16,0wxbc)  | CVTQSC  (0wx16,0wx3c)
       | CVTQT   (0wx16,0wxbe)  | CVTQTC  (0wx16,0wx3e)
       | CVTTS   (0wx16,0wxac)  | CVTTSC  (0wx16,0wx2c)
       | CVTST   (0wx16,0wx2ac) | CVTSTS  (0wx16,0wx6ac)
       | CVTTQ   (0wx16,0wxaf)  | CVTTQC  (0wx16,0wx2f)
    
      datatype foperate! =   (* table C-6 *)
         CPYS    (0wx17,0wx20)  | CPYSE    (0wx17,0wx022) | CPYSN (0wx17,0wx021)
       | MF_FPCR (0wx17,0wx025) | MT_FPCR  (0wx17,0wx024)
   
                            (* table C-7 *)
       | CMPTEQ  (0wx16,0wx0a5) | CMPTLT (0wx16,0wx0a6)  | CMPTLE (0wx16,0wx0a7)
       | CMPTUN  (0wx16,0wx0a4)
   
       | CMPTEQSU(0wx16,0wx5a5) | CMPTLTSU(0wx16,0wx5a6) |CMPTLESU(0wx16,0wx5a7)
       | CMPTUNSU(0wx16,0wx5a4)

       | ADDS (0wx16,0wx080) | ADDT (0wx16,0wx0a0) 
       | DIVS (0wx16,0wx083) | DIVT (0wx16,0wx0a3)
       | MULS (0wx16,0wx082) | MULT (0wx16,0wx0a2) 
       | SUBS (0wx16,0wx081) | SUBT (0wx16,0wx0a1)
   
      datatype fcmove! =
         FCMOVEQ 0wx02a | FCMOVGE 0wx02d | FCMOVGT 0wx02f
       | FCMOVLE 0wx02e | FCMOVLT 0wx02c | FCMOVNE 0wx02b
   
      datatype foperateV! = (* table C-7 *)
         ADDSSUD  0wx5c0  | ADDSSU 0wx580 
       | ADDTSUD  0wx5e0  | ADDTSU 0wx5a0 
       | DIVSSUD  0wx5c3  | DIVSSU 0wx583
       | DIVTSUD  0wx5e3  | DIVTSU 0wx5a3
       | MULSSUD  0wx5c2  | MULSSU 0wx582
       | MULTSUD  0wx5e2  | MULTSU 0wx5a2
       | SUBSSUD  0wx5c1  | SUBSSU 0wx581
       | SUBTSUD  0wx5e1  | SUBTSU 0wx5a1
    
      datatype osf_user_palcode! = 
         BPT 0x80 | BUGCHK 0x81 | CALLSYS 0x83 
       | GENTRAP 0xaa | IMB 0x86 | RDUNIQUE 0x9e | WRUNIQUE 0x9f

      type addressing_mode = C.cell * operand
   
   end (* Instruction *)

   (*
    * Alpha has very simple instruction encoding formats.
    *)
   instruction formats 32 bits 
     Memory{opc:6, ra:5, rb:GP 5, disp: signed 16} (* p3-9 *)
      (* derived from Memory *) 
   | Split{le} = let val i = LabelExp.valueOf le
                     val w = itow i 
                     val hi = w ~>> 0w16
                     val lo = w && 0w65535
                 in  if lo < 0w32768 then (hi,lo) else (hi+0w1,lo-0w65536)
                 end 
   | High{le} = let val (hi,_) = Split{le=le} in hi end
   | Low{le}  = let val (_,lo) = Split{le=le} in lo end
   | LoadStore{opc,ra,rb,disp} =
       let val disp = 
           case disp of
             I.REGop rb => emit_GP rb
           | I.IMMop i  => itow i
           | I.HILABop le => High{le=le}
           | I.LOLABop le => Low{le=le}
           | I.LABop le => itow(LabelExp.valueOf le)
       in  Memory{opc,ra,rb,disp}
       end
   | ILoadStore{opc,r:GP,b,d} = LoadStore{opc,ra=r,rb=b,disp=d}
   | FLoadStore{opc,r:FP,b,d} = LoadStore{opc,ra=r,rb=b,disp=d}

   | Jump{opc:6=0wx1a,ra:GP 5,rb:GP 5,h:2,disp:int signed 14}   (* table C-3 *)
   | Memory_fun{opc:6, ra:GP 5, rb:GP 5, func:16}     (* p3-9 *)
   | Branch{opc:branch 6, ra:GP 5, disp:signed 21}           (* p3-10 *)
   | Fbranch{opc:fbranch 6, ra:FP 5, disp:signed 21}          (* p3-10 *)
        (* p3-11 *)
   | Operate0{opc:6,ra:GP 5,rb:GP 5,sbz:13..15=0,_:1=0,func:5..11,rc:GP 5} 
        (* p3-11 *)
   | Operate1{opc:6,ra:GP 5,lit:signed 13..20,_:1=1,func:5..11,rc:GP 5} 
   | Operate{opc,ra,rb,func,rc} =
        (case rb of
          I.REGop rb => Operate0{opc,ra,rb,func,rc}
        | I.IMMop i  => Operate1{opc,ra,lit=itow i,func,rc}
        | I.HILABop le => Operate1{opc,ra,lit=High{le=le},func,rc}
        | I.LOLABop le => Operate1{opc,ra,lit=Low{le=le},func,rc}
        | I.LABop le => Operate1{opc,ra,lit=itow(LabelExp.valueOf le),func,rc}
        )
   | Foperate{opc:6,fa:FP 5,fb:FP 5,func:5..15,fc:FP 5}
   | Funary{opc:6,fa:5=31,fb:FP 5,func:5..15,fc:FP 5}
   | Pal{opc:6=0,func:26}

   structure MC =
   struct
      (* compute displacement address *)
      fun disp lab = itow(Label.addrOf lab - !loc - 4) ~>> 0w2
   end

   structure Assembly =
   struct
      fun isZero(I.LABop le) = LabelExp.valueOf le = 0
        | isZero _ = false
   end

   (*
    * The main instruction set definition consists of the following:
    *  1) constructor-like declaration defines the view of the instruction,
    *  2) assembly directive in funny quotes `` '',
    *  3) machine encoding expression,
    *  4) delay slot directives etc (not necessary in this architecture)
    *) 
   instruction 

        (* Pseudo instruction for the register allocator *)
     DEFFREG of $FP                        (* define a floating point register *)
     asm: ``/* deffreg\t<FP> */''
     mc:  () (* do nothing when emitting code *) 
     rtl: [[ "DEFFREG" ]]
 
   (* Load/Store *)
   | LDA of {r: $GP, b: $GP, d:operand}        (* use of REGop is illegal *)
     asm: if isZero d andalso r = b then ()
          else (``lda\t<r>, <d>''; if b = 31 then () else ``(<b>)'')
     mc:  ILoadStore{opc=0w08,r,b,d}
     rtl: [[ "LDA" ]]

   | LDAH of {r: $GP, b: $GP, d:operand} (* use of REGop is illegal *)
     asm: (``ldah\t<r>, <d>''; if b = 31 then () else ``(<b>)'')
     mc:  ILoadStore{opc=0w09,r,b,d}
     rtl: [[ "LDAH" ]]

   | LOAD of {ldOp:load, r: $GP, b: $GP, d:operand, mem:Region.region}
     asm: ``<ldOp>\t<r>, <d>(<b>)<mem>''
     mc:  ILoadStore{opc=emit_load ldOp,r,b,d}
     rtl: [[ ldOp ]]
     latency: 1

   | STORE of {stOp:store, r: $GP, b: $GP, d:operand, mem:Region.region}
     asm: ``<stOp>\t<r>, <d>(<b>)<mem>''
     mc:  ILoadStore{opc=emit_store stOp,r,b,d}
     rtl: [[ stOp ]]

   | FLOAD of {ldOp:fload, r: $FP, b: $GP, d:operand, mem:Region.region}
     asm: ``<ldOp>\t<r>, <d>(<b>)<mem>''
     mc:  FLoadStore{opc=emit_fload ldOp,r,b,d}
     rtl: [[ ldOp ]]
     latency: 1

   | FSTORE of {stOp:fstore, r: $FP, b: $GP, d:operand, mem:Region.region}
     asm: ``<stOp>\t<r>, <d>(<b>)<mem>''
     mc:  FLoadStore{opc=emit_fstore stOp,r,b,d}
     rtl: [[ stOp ]]
 
   (* Control Instructions *)
   | JMPL of {r: $GP, b: $GP, d:int} * Label.label list
     asm: ``jmp\t<r>, (<b>)''
     mc:  Jump{h=0w0,ra=r,rb=b,disp=d}   (* table C-3 *)
     rtl: [[ "JMPL" ]]

   | JSR of {r: $GP, b: $GP, d:int, 
             defs:C.cellset, uses:C.cellset, mem:Region.region}
     asm: ``jsr\t<r>, (<b>)<mem><emit_defs(defs)><emit_uses(uses)>''
     mc:  Jump{h=0w1,ra=r,rb=b,disp=d}
     rtl: [[ "JSR" ]]

   | RET of {r: $GP, b: $GP, d:int} 
     asm: ``ret\t<r>, (<b>)''
     mc:  Jump{h=0w2,ra=r,rb=b,disp=d}
     rtl: [[ "RET" ]]

   | BRANCH of {b:branch, r: $GP, lab:Label.label}
     asm: ``<b>\t<r>, <lab>''
     mc:  Branch{opc=b,ra=r,disp=disp lab}
     rtl: [[ b ]]

   | FBRANCH of {b:fbranch, f: $FP, lab:Label.label}  
     asm: ``<b>\t<f>, <lab>''
     mc:  Fbranch{opc=b,ra=f,disp=disp lab}
     rtl: [[ b ]]
 
   (* Integer Operate *)
   | OPERATE of {oper:operate, ra: $GP, rb:operand, rc: $GP}
             (* Pretty print ldgp differently *) 
     asm: case (oper,ra,rb,rc) of
            (I.BIS,27,I.REGop 31,29) => ``ldgp\t$29, 0($27)''
          | (I.BIS,26,I.REGop 31,29) => ``ldgp\t$29, 0($26)''
          | _                        => ``<oper>\t<ra>, <rb>, <rc>''
     mc:  let val (opc,func) = emit_operate oper
          in  Operate{opc,func,ra,rb,rc} 
          end
     rtl: [[ oper ]]

   | OPERATEV of {oper:operateV, ra: $GP, rb:operand, rc: $GP}
     asm: ``<oper>\t<ra>, <rb>, <rc>''
     mc:  let val (opc,func) = emit_operateV oper
          in  Operate{opc,func,ra,rb,rc} 
          end
     rtl: [[ oper ]]

   | CMOVE of {oper:cmove, ra: $GP, rb:operand, rc: $GP}
     asm: ``<oper>\t<ra>, <rb>, <rc>''
     mc:  Operate{opc=0wx11,func=emit_cmove oper,ra,rb,rc} 
     rtl: [[ oper ]]
 
   | PSEUDOARITH of {oper: pseudo_op, ra: $GP, rb:operand, rc: $GP, 
                     tmps: C.cellset}
     asm: ``<oper>\t<ra>, <rb>, <rc><emit_cellset("tmps",tmps)>''
     rtl: [[ "PSEUDOARITH_" oper ]]
 
   (* Copy instructions *)
   | COPY of {dst: $GP list, src: $GP list, 
              impl:instruction list option ref, tmp: ea option}
     asm: emitInstrs (Shuffle.shuffle{regmap,tmp,dst,src})
     rtl: [[ "COPY" ]]

   | FCOPY of {dst: $FP list, src: $FP list, 
               impl:instruction list option ref, tmp: ea option}
     asm: emitInstrs (Shuffle.shufflefp{regmap,tmp,dst,src})
     rtl: [[ "FCOPY" ]]
 
   (* Floating Point Unary Operation *)
   | FUNARY of {oper:funary, fb: $FP, fc: $FP}
     asm: ``<oper>\t<fb>, <fc>''
     mc:  let val (opc,func) = emit_funary oper
          in  Funary{opc,func,fb,fc}
          end
     rtl: [[ oper ]]

   (* Floating Point Operate *)
   | FOPERATE of {oper:foperate, fa: $FP, fb: $FP, fc: $FP}
     asm: ``<oper>\t<fa>, <fb>, <fc>''
     mc:  let val (opc,func) = emit_foperate oper
          in  Foperate{opc,func,fa,fb,fc}
          end
     rtl: [[ oper ]]

   (* Trapping versions of the above (what trap -- allen) ??? *)
   | FOPERATEV of {oper:foperateV, fa: $FP, fb: $FP, fc: $FP}
     asm: ``<oper>\t<fa>, <fb>, <fc>''
     mc:  Foperate{opc=0wx16,func=emit_foperateV oper,fa,fb,fc}
     rtl: [[ oper ]]

   | FCMOVE of {oper:fcmove, fa: $FP, fb: $FP, fc: $FP}
     asm: ``<oper>\t<fa>, <fb>, <fc>''
     mc:  Foperate{opc=0wx17,func=emit_fcmove oper,fa,fb,fc}
     rtl: [[ oper ]]
 
   (* Misc *)
   | TRAPB                                (* Trap barrier *)
     asm: ``trapb''
     mc:  Memory_fun{opc=0wx18,ra=31,rb=31,func=0wx0}
     rtl: [[ "TRAPB" ]]
 
   | CALL_PAL of {code:osf_user_palcode, def: $GP list, use: $GP list}
     asm: ``call_pal <code>''
     mc:  Pal{func=emit_osf_user_palcode code}
     rtl: [[ "CALL_PAL_" code ]]

   | ANNOTATION of {i:instruction, a:Annotations.annotation}
     asm: (emitInstr i; comment(Annotations.toString a))
     mc:  (emitInstr i)
     rtl: [[ #i ]]
 end
