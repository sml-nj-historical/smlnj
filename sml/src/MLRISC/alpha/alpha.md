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
     GP = 32 cells of 64 bits in cellset called "register" 
		assembly as (fn 30 => "$sp"
                              | r  => "$"^Int.toString r
                            )
   | FP = 32 cells of 64 bits in cellset called "floating point register" 
		assembly as (fn f => "$f"^Int.toString f)
   | CC = cells of 64 bits in cellset GP called "condition code register"
                assembly as "cc"
   locations
       stackptrR = $GP[30]
   and asmTmpR   = $GP[28]
   and fasmTmp   = $FP[30]

   structure Cells =
   struct
      fun zeroReg GP = SOME($GP[31])
        | zeroReg FP = SOME($FP[31])
        | zeroReg _  = NONE
   end

   semantics Alpha32 =
   struct
      include "MD++/rtl.md"
      open Basis
   end

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
       REGop of $GP			``<GP>'' (GP)
     | IMMop of int			``<int>''
     | HILABop of LabelExp.labexp	``hi(<emit_labexp labexp>)''
     | LOLABop of LabelExp.labexp	``lo(<emit_labexp labexp>)''
     | LABop of LabelExp.labexp		``<emit_labexp labexp>''
     | CONSTop of Constant.const	``<emit_const const>''

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
      LDL   0x28    [[ load32s ]]
    | LDL_L 0x2A  
    | LDQ   0x29    [[ load64s ]]
    | LDQ_L 0x2B 
    | LDQ_U 0x0B
   datatype store! = STL 0x2C | STQ 0x2D | STQ_U 0x0F
   datatype fload[0x20..0x23]! = LDF | LDG | LDS | LDT 
   datatype fstore[0x24..0x27]! = STF | STG | STS | STT 

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
 
   datatype pseudo_op! = DIVL | DIVLU
 
   datatype operateV! = (* table C-5 opc/func *)
        ADDLV (0wx10,0wx40) | ADDQV (0wx10,0wx60)
      | SUBLV (0wx10,0wx49) | SUBQV (0wx10,0wx69) 
      | MULLV (0wx13,0wx40) | MULQV (0wx13,0wx60)

   datatype funary! = (* table C-6/C-7 *)
          (* C-6 *)
      CVTLQ   (0wx17,0wx010) | CVTQL (0wx17,0wx030)    | CVTQLSV (0wx17,0wx530)
    | CVTQLV  (0wx17,0wx130)

          (* C-7 *)
    | CVTQS   (0wx16,0wxbc)  | CVTQSC  (0wx16,0wx3c)
    | CVTQT   (0wx16,0wxbe)  | CVTQTC  (0wx16,0wx3e)
    | CVTTS   (0wx16,0wxac)  | CVTTSC  (0wx16,0wx2c)
    | CVTTQ   (0wx16,0wxaf)  | CVTTQC  (0wx16,0wx2f)
 
   datatype foperate! =   (* table C-6 *)
      CPYS    (0wx17,0wx20)  | CPYSE    (0wx17,0wx022) | CPYSN    (0wx17,0wx021)
    | MF_FPCR (0wx17,0wx025) | MT_FPCR  (0wx17,0wx024)

                         (* table C-7 *)
    | CMPTEQ  (0wx16,0wx0a5) | CMPTLT (0wx16,0wx0a6)   | CMPTLE  (0wx16,0wx0a7)
    | CMPTUN  (0wx16,0wx0a4)

    | CMPTEQSU(0wx16,0wx5a5) | CMPTLTSU(0wx16,0wx5a6)  | CMPTLESU(0wx16,0wx5a7)
    | CMPTUNSU(0wx16,0wx5a4)

   datatype fcmove! =
      FCMOVEQ 0wx02a | FCMOVEGE 0wx02d | FCMOVEGT 0wx02f
    | FCMOVLE 0wx02e | FCMOVELT 0wx02c | FCMOVENE 0wx02b

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
           | I.CONSTop c => itow(Constant.valueOf c)
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
        | I.CONSTop c => Operate1{opc,ra,lit=itow(Constant.valueOf c),func,rc}
        )
   | Foperate{opc:6,fa:FP 5,fb:FP 5,func:5..15,fc:FP 5}
   | Funary{opc:6,fa:5=31,fb:FP 5,func:5..15,fc:FP 5}
   | Pal{opc:6=0,func:26}

   structure MC =
   struct
      (* compute displacement address *)
      fun disp lab = itow(Label.addrOf lab - !loc - 4) ~>> 0w2
   end

   (*
    * The main instruction set definition consists of the following:
    *  1) constructor-like declaration defines the view of the instruction,
    *  2) assembly directive in funny quotes `` '',
    *  3) machine encoding expression,
    *  4) semantics expression in [[ ]],
    *  5) delay slot directives etc (not necessary in this architecture!)
    *) 
   instruction 

        (* Pseudo instruction for the register allocator *)
     DEFFREG of $FP			(* define a floating point register *)
	``/* deffreg\t<FP> */''
        (()) (* do nothing when emitting code *) 
 
   (* Load/Store *)
   | LDA of {r: $GP, b: $GP, d:operand}	(* use of REGop is illegal *)
     ``<(emit "lda\t"; emit_GP r; emit ", "; emit_operand d;
         if b=31 then () else (emit "("; emit_GP b; emit ")"))
       >''
     ILoadStore{opc=0w08,r,b,d}

   | LDAH of {r: $GP, b: $GP, d:operand} (* use of REGop is illegal *)
     ``<(emit "ldah\t"; emit_GP r; emit ", "; emit_operand d;
         if b=31 then () else (emit "("; emit_GP b; emit ")"))
       >''
     ILoadStore{opc=0w09,r,b,d}

   | LOAD of {ldOp:load, r: $GP, b: $GP, d:operand, mem:Region.region}
     ``<ldOp>\t<r>, <d>(<b>)<mem>''
     ILoadStore{opc=emit_load ldOp,r,b,d}

   | STORE of {stOp:store, r: $GP, b: $GP, d:operand, mem:Region.region}
     ``<stOp>\t<r>, <d>(<b>)<mem>''
     ILoadStore{opc=emit_store stOp,r,b,d}

   | FLOAD of {ldOp:fload, r: $FP, b: $GP, d:operand, mem:Region.region}
     ``<ldOp>\t<r>, <d>(<b>)<mem>''
     FLoadStore{opc=emit_fload ldOp,r,b,d}

   | FSTORE of {stOp:fstore, r: $FP, b: $GP, d:operand, mem:Region.region}
     ``<stOp>\t<r>, <d>(<b>)<mem>''
     FLoadStore{opc=emit_fstore stOp,r,b,d}
 
   (* Control Instructions *)
   | JMPL of {r: $GP, b: $GP, d:int} * Label.label list
     ``jmp\t<r>, (<b>)''
     Jump{h=0w0,ra=r,rb=b,disp=d}   (* table C-3 *)

   | JSR of {r: $GP, b: $GP, d:int} * C.cellset * C.cellset * Region.region
     ``jsr\t<r>, (<b>)<region>''
     Jump{h=0w1,ra=r,rb=b,disp=d}

   | RET of {r: $GP, b: $GP, d:int} 
     ``ret\t<r>, (<b>)''
     Jump{h=0w2,ra=r,rb=b,disp=d}

   | BRANCH of branch * $GP * Label.label   
     ``<branch>\t<GP>, <label>''
     Branch{opc=branch,ra=GP,disp=disp label}

   | FBRANCH of fbranch * $FP * Label.label  
     ``<fbranch>\t<FP>, <label>''
     Fbranch{opc=fbranch,ra=FP,disp=disp label}
 
   (* Integer Operate *)
   | OPERATE of {oper:operate, ra: $GP, rb:operand, rc: $GP}
        ``<let fun f(oper,ra,rb,rc) =
                  (emit oper; tab(); 
                   emit_GP ra; emit ", "; emit_operand rb; 
                   emit ", "; emit_GP rc)
           in case (oper,ra,rb,rc) of
                (I.BIS,27,I.REGop 31,29) => emit "ldgp\t$29, 0($27)"
              | (I.BIS,26,I.REGop 31,29) => emit "ldgp\t$26, 0($27)"
              | (I.ADDL,30,I.CONSTop b,30) => 
                  if Constant.valueOf b = 0 then () else f("addl",ra,rb,rc)
              | (I.ADDQ,30,I.CONSTop b,30) => 
                  if Constant.valueOf b = 0 then () else f("addq",ra,rb,rc)
              | (I.SUBL,30,I.CONSTop b,30) => 
                  if Constant.valueOf b = 0 then () else f("subl",ra,rb,rc)
              | (I.SUBQ,30,I.CONSTop b,30) => 
                  if Constant.valueOf b = 0 then () else f("subq",ra,rb,rc)
              | _ => f(asm_operate oper,ra,rb,rc)
           end>''   
        (let val (opc,func) = emit_operate oper
         in  Operate{opc,func,ra,rb,rc} 
         end)

   | OPERATEV of {oper:operateV, ra: $GP, rb:operand, rc: $GP}
	``<oper>\t<ra>, <rb>, <rc>''
        (let val (opc,func) = emit_operateV oper
         in  Operate{opc,func,ra,rb,rc} 
         end)

   | CMOVE of {oper:cmove, ra: $GP, rb:operand, rc: $GP}
	``<oper>\t<ra>, <rb>, <rc>''
         Operate{opc=0wx11,func=emit_cmove oper,ra,rb,rc} 
 
   | PSEUDOARITH of {oper: pseudo_op, ra: $GP, rb:operand, rc: $GP, 
		     tmps: C.cellset}
	``<oper>\t<ra>, <rb>, <rc>''
 
   (* Copy instructions *)
   | COPY of {dst: $GP list, src: $GP list, 
              impl:instruction list option ref, tmp: ea option}
	``<emitInstrs (Shuffle.shuffle{regmap,tmp,dst,src})>''

   | FCOPY of {dst: $FP list, src: $FP list, 
               impl:instruction list option ref, tmp: ea option}
	``<emitInstrs (Shuffle.shufflefp{regmap,tmp,dst,src})>''
 
   (* Floating Point Unary Operation *)
   | FUNARY of {oper:funary, fb: $FP, fc: $FP}
	``<oper>\t<fb>, <fc>''
       (let val (opc,func) = emit_funary oper
        in  Funary{opc,func,fb,fc}
        end)

   (* Floating Point Operate *)
   | FOPERATE of {oper:foperate, fa: $FP, fb: $FP, fc: $FP}
	``<oper>\t<fa>, <fb>, <fc>''
       (let val (opc,func) = emit_foperate oper
        in  Foperate{opc,func,fa,fb,fc}
        end)

   (* Trapping versions of the above (what trap -- allen) ??? *)
   | FOPERATEV of {oper:foperateV, fa: $FP, fb: $FP, fc: $FP}
	``<oper>\t<fa>, <fb>, <fc>''
        Foperate{opc=0wx16,func=emit_foperateV oper,fa,fb,fc}

   | FCMOVE of {oper:fcmove, fa: $FP, fb: $FP, fc: $FP}
	``<oper>\t<fa>, <fb>, <fc>''
        Foperate{opc=0wx17,func=emit_fcmove oper,fa,fb,fc}
 
   (* Misc *)
   | TRAPB				(* Trap barrier *)
	``trapb''
        Memory_fun{opc=0wx18,ra=31,rb=31,func=0wx0}
 
   | CALL_PAL of {code:osf_user_palcode, def: $GP list, use: $GP list}
	``call_pal <code>''
        Pal{func=emit_osf_user_palcode code}

   | ANNOTATION of {i:instruction, a:Annotations.annotation}
       ``<(emitInstr i; comment(Annotations.toString a))>''
        (emitInstr i)

   | GROUP of Annotations.annotation
        ``<comment(Annotations.toString annotation)>''
 end
