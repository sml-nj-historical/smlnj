(*
 * WARNING: This file was automatically generated by MDLGen (v3.0)
 * from the machine description file "sparc/sparc.mdl".
 * DO NOT EDIT this file directly
 *)


signature SPARCINSTR =
sig
   structure C : SPARCCELLS
   structure T : MLTREE
   structure LabelExp : LABELEXP
   structure Constant: CONSTANT
   structure Region : REGION
      sharing LabelExp.T = T
      sharing Constant = T.Constant
      sharing Region = T.Region
   datatype load =
     LDSB
   | LDSH
   | LDUB
   | LDUH
   | LD
   | LDX
   | LDD
   datatype store =
     STB
   | STH
   | ST
   | STX
   | STD
   datatype fload =
     LDF
   | LDDF
   | LDQF
   | LDFSR
   | LDXFSR
   datatype fstore =
     STF
   | STDF
   | STFSR
   datatype arith =
     AND
   | ANDCC
   | ANDN
   | ANDNCC
   | OR
   | ORCC
   | ORN
   | ORNCC
   | XOR
   | XORCC
   | XNOR
   | XNORCC
   | ADD
   | ADDCC
   | TADD
   | TADDCC
   | TADDTV
   | TADDTVCC
   | SUB
   | SUBCC
   | TSUB
   | TSUBCC
   | TSUBTV
   | TSUBTVCC
   | UMUL
   | UMULCC
   | SMUL
   | SMULCC
   | UDIV
   | UDIVCC
   | SDIV
   | SDIVCC
   | MULX
   | SDIVX
   | UDIVX
   datatype shift =
     SLL
   | SRL
   | SRA
   | SLLX
   | SRLX
   | SRAX
   datatype farith1 =
     FiTOs
   | FiTOd
   | FiTOq
   | FsTOi
   | FdTOi
   | FqTOi
   | FsTOd
   | FsTOq
   | FdTOs
   | FdTOq
   | FqTOs
   | FqTOd
   | FMOVs
   | FNEGs
   | FABSs
   | FMOVd
   | FNEGd
   | FABSd
   | FMOVq
   | FNEGq
   | FABSq
   | FSQRTs
   | FSQRTd
   | FSQRTq
   datatype farith2 =
     FADDs
   | FADDd
   | FADDq
   | FSUBs
   | FSUBd
   | FSUBq
   | FMULs
   | FMULd
   | FMULq
   | FsMULd
   | FdMULq
   | FDIVs
   | FDIVd
   | FDIVq
   datatype fcmp =
     FCMPs
   | FCMPd
   | FCMPq
   | FCMPEs
   | FCMPEd
   | FCMPEq
   datatype branch =
     BN
   | BE
   | BLE
   | BL
   | BLEU
   | BCS
   | BNEG
   | BVS
   | BA
   | BNE
   | BG
   | BGE
   | BGU
   | BCC
   | BPOS
   | BVC
   datatype rcond =
     RZ
   | RLEZ
   | RLZ
   | RNZ
   | RGZ
   | RGEZ
   datatype cc =
     ICC
   | XCC
   datatype prediction =
     PT
   | PN
   datatype fbranch =
     FBN
   | FBNE
   | FBLG
   | FBUL
   | FBL
   | FBUG
   | FBG
   | FBU
   | FBA
   | FBE
   | FBUE
   | FBGE
   | FBUGE
   | FBLE
   | FBULE
   | FBO
   datatype ea =
     Direct of C.cell
   | FDirect of C.cell
   | Displace of {base:C.cell, disp:int}
   datatype fsize =
     S
   | D
   | Q
   datatype operand =
     REG of C.cell
   | IMMED of int
   | LAB of T.labexp
   | LO of T.labexp
   | HI of T.labexp
   type addressing_mode = (C.cell * operand)
   datatype instruction =
     LOAD of {l:load, d:C.cell, r:C.cell, i:operand, mem:Region.region}
   | STORE of {s:store, d:C.cell, r:C.cell, i:operand, mem:Region.region}
   | FLOAD of {l:fload, r:C.cell, i:operand, d:C.cell, mem:Region.region}
   | FSTORE of {s:fstore, d:C.cell, r:C.cell, i:operand, mem:Region.region}
   | SETHI of {i:int, d:C.cell}
   | ARITH of {a:arith, r:C.cell, i:operand, d:C.cell}
   | SHIFT of {s:shift, r:C.cell, i:operand, d:C.cell}
   | MOVicc of {b:branch, i:operand, d:C.cell}
   | MOVfcc of {b:fbranch, i:operand, d:C.cell}
   | MOVR of {rcond:rcond, r:C.cell, i:operand, d:C.cell}
   | FMOVicc of {sz:fsize, b:branch, r:C.cell, d:C.cell}
   | FMOVfcc of {sz:fsize, b:fbranch, r:C.cell, d:C.cell}
   | Bicc of {b:branch, a:bool, label:Label.label, nop:bool}
   | FBfcc of {b:fbranch, a:bool, label:Label.label, nop:bool}
   | BR of {rcond:rcond, p:prediction, r:C.cell, a:bool, label:Label.label, 
        nop:bool}
   | BP of {b:branch, p:prediction, cc:cc, a:bool, label:Label.label, nop:bool}
   | JMP of {r:C.cell, i:operand, labs:Label.label list, nop:bool}
   | JMPL of {r:C.cell, i:operand, d:C.cell, defs:C.cellset, uses:C.cellset, 
        nop:bool, mem:Region.region}
   | CALL of {defs:C.cellset, uses:C.cellset, label:Label.label, nop:bool, 
        mem:Region.region}
   | Ticc of {t:branch, cc:cc, r:C.cell, i:operand}
   | FPop1 of {a:farith1, r:C.cell, d:C.cell}
   | FPop2 of {a:farith2, r1:C.cell, r2:C.cell, d:C.cell}
   | FCMP of {cmp:fcmp, r1:C.cell, r2:C.cell, nop:bool}
   | COPY of {dst:C.cell list, src:C.cell list, impl:instruction list option ref, 
        tmp:ea option}
   | FCOPY of {dst:C.cell list, src:C.cell list, impl:instruction list option ref, 
        tmp:ea option}
   | SAVE of {r:C.cell, i:operand, d:C.cell}
   | RESTORE of {r:C.cell, i:operand, d:C.cell}
   | RDY of {d:C.cell}
   | WRY of {r:C.cell, i:operand}
   | RET of {leaf:bool, nop:bool}
   | ANNOTATION of {i:instruction, a:Annotations.annotation}
   | SOURCE of {}
   | SINK of {}
   | PHI of {}
end

functor SparcInstr(LabelExp : LABELEXP
                  ) : SPARCINSTR =
struct
   structure C = SparcCells
   structure LabelExp = LabelExp
   structure T = LabelExp.T
   structure Region = T.Region
   structure Constant = T.Constant
   datatype load =
     LDSB
   | LDSH
   | LDUB
   | LDUH
   | LD
   | LDX
   | LDD
   datatype store =
     STB
   | STH
   | ST
   | STX
   | STD
   datatype fload =
     LDF
   | LDDF
   | LDQF
   | LDFSR
   | LDXFSR
   datatype fstore =
     STF
   | STDF
   | STFSR
   datatype arith =
     AND
   | ANDCC
   | ANDN
   | ANDNCC
   | OR
   | ORCC
   | ORN
   | ORNCC
   | XOR
   | XORCC
   | XNOR
   | XNORCC
   | ADD
   | ADDCC
   | TADD
   | TADDCC
   | TADDTV
   | TADDTVCC
   | SUB
   | SUBCC
   | TSUB
   | TSUBCC
   | TSUBTV
   | TSUBTVCC
   | UMUL
   | UMULCC
   | SMUL
   | SMULCC
   | UDIV
   | UDIVCC
   | SDIV
   | SDIVCC
   | MULX
   | SDIVX
   | UDIVX
   datatype shift =
     SLL
   | SRL
   | SRA
   | SLLX
   | SRLX
   | SRAX
   datatype farith1 =
     FiTOs
   | FiTOd
   | FiTOq
   | FsTOi
   | FdTOi
   | FqTOi
   | FsTOd
   | FsTOq
   | FdTOs
   | FdTOq
   | FqTOs
   | FqTOd
   | FMOVs
   | FNEGs
   | FABSs
   | FMOVd
   | FNEGd
   | FABSd
   | FMOVq
   | FNEGq
   | FABSq
   | FSQRTs
   | FSQRTd
   | FSQRTq
   datatype farith2 =
     FADDs
   | FADDd
   | FADDq
   | FSUBs
   | FSUBd
   | FSUBq
   | FMULs
   | FMULd
   | FMULq
   | FsMULd
   | FdMULq
   | FDIVs
   | FDIVd
   | FDIVq
   datatype fcmp =
     FCMPs
   | FCMPd
   | FCMPq
   | FCMPEs
   | FCMPEd
   | FCMPEq
   datatype branch =
     BN
   | BE
   | BLE
   | BL
   | BLEU
   | BCS
   | BNEG
   | BVS
   | BA
   | BNE
   | BG
   | BGE
   | BGU
   | BCC
   | BPOS
   | BVC
   datatype rcond =
     RZ
   | RLEZ
   | RLZ
   | RNZ
   | RGZ
   | RGEZ
   datatype cc =
     ICC
   | XCC
   datatype prediction =
     PT
   | PN
   datatype fbranch =
     FBN
   | FBNE
   | FBLG
   | FBUL
   | FBL
   | FBUG
   | FBG
   | FBU
   | FBA
   | FBE
   | FBUE
   | FBGE
   | FBUGE
   | FBLE
   | FBULE
   | FBO
   datatype ea =
     Direct of C.cell
   | FDirect of C.cell
   | Displace of {base:C.cell, disp:int}
   datatype fsize =
     S
   | D
   | Q
   datatype operand =
     REG of C.cell
   | IMMED of int
   | LAB of T.labexp
   | LO of T.labexp
   | HI of T.labexp
   type addressing_mode = (C.cell * operand)
   datatype instruction =
     LOAD of {l:load, d:C.cell, r:C.cell, i:operand, mem:Region.region}
   | STORE of {s:store, d:C.cell, r:C.cell, i:operand, mem:Region.region}
   | FLOAD of {l:fload, r:C.cell, i:operand, d:C.cell, mem:Region.region}
   | FSTORE of {s:fstore, d:C.cell, r:C.cell, i:operand, mem:Region.region}
   | SETHI of {i:int, d:C.cell}
   | ARITH of {a:arith, r:C.cell, i:operand, d:C.cell}
   | SHIFT of {s:shift, r:C.cell, i:operand, d:C.cell}
   | MOVicc of {b:branch, i:operand, d:C.cell}
   | MOVfcc of {b:fbranch, i:operand, d:C.cell}
   | MOVR of {rcond:rcond, r:C.cell, i:operand, d:C.cell}
   | FMOVicc of {sz:fsize, b:branch, r:C.cell, d:C.cell}
   | FMOVfcc of {sz:fsize, b:fbranch, r:C.cell, d:C.cell}
   | Bicc of {b:branch, a:bool, label:Label.label, nop:bool}
   | FBfcc of {b:fbranch, a:bool, label:Label.label, nop:bool}
   | BR of {rcond:rcond, p:prediction, r:C.cell, a:bool, label:Label.label, 
        nop:bool}
   | BP of {b:branch, p:prediction, cc:cc, a:bool, label:Label.label, nop:bool}
   | JMP of {r:C.cell, i:operand, labs:Label.label list, nop:bool}
   | JMPL of {r:C.cell, i:operand, d:C.cell, defs:C.cellset, uses:C.cellset, 
        nop:bool, mem:Region.region}
   | CALL of {defs:C.cellset, uses:C.cellset, label:Label.label, nop:bool, 
        mem:Region.region}
   | Ticc of {t:branch, cc:cc, r:C.cell, i:operand}
   | FPop1 of {a:farith1, r:C.cell, d:C.cell}
   | FPop2 of {a:farith2, r1:C.cell, r2:C.cell, d:C.cell}
   | FCMP of {cmp:fcmp, r1:C.cell, r2:C.cell, nop:bool}
   | COPY of {dst:C.cell list, src:C.cell list, impl:instruction list option ref, 
        tmp:ea option}
   | FCOPY of {dst:C.cell list, src:C.cell list, impl:instruction list option ref, 
        tmp:ea option}
   | SAVE of {r:C.cell, i:operand, d:C.cell}
   | RESTORE of {r:C.cell, i:operand, d:C.cell}
   | RDY of {d:C.cell}
   | WRY of {r:C.cell, i:operand}
   | RET of {leaf:bool, nop:bool}
   | ANNOTATION of {i:instruction, a:Annotations.annotation}
   | SOURCE of {}
   | SINK of {}
   | PHI of {}
end

