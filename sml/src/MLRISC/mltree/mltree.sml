(* mltree.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

functor MLTreeF(structure Const : CONSTANT
		structure P : PSEUDO_OPS
		structure R : REGION
		structure B : BLOCK_NAMES
                type rextension 
                type fextension
               ) : MLTREE =
struct
  structure Constant = Const
  structure PseudoOp = P
  structure Region = R
  structure BNames = B

  open MLTreeBasis

  type rextension = rextension
  type fextension = rextension

  type var = int (* variable *)
  type src = var (* source variable *)
  type dst = var (* destination variable *)
  type reg = var (* physical register *)

  (* phi-functions for SSA form *)
  datatype phi =
      PHI  of ty * dst * src list 
    | FPHI of fty * dst * src list 
    | CCPHI of dst * src list 

  (* aliasing declarations 
   * These are used to define physical register bindings for SSA names 
   *)
  datatype alias = ALIAS   of ty * var * reg  
                 | FALIAS  of fty * var * reg 
                 | CCALIAS of var * reg

  (* statements *)
  datatype stm =
      MV      of ty * dst * rexp	
    | CCMV    of dst * ccexp
    | FMV     of fty * dst * fexp	
    | COPY    of ty * dst list * src list
    | FCOPY   of fty * dst list * src list
    | JMP     of rexp * Label.label list
    | CALL    of rexp * mlrisc list * mlrisc list * Region.region
    | RET

    | STORE  of ty * rexp * rexp * Region.region	(* address, data *)
    | STORE_UNALIGNED of ty * rexp * rexp * Region.region
    | FSTORE of fty * rexp * fexp * Region.region	(* address, data *)
    | FSTORE_UNALIGNED of fty * rexp * fexp * Region.region
    | BCC    of cond * ccexp * Label.label 
    | FBCC   of fcond * ccexp * Label.label
    | ANNOTATION of stm * Annotations.annotation

      (* The following are used internally by SSA optimizations; 
       * The frontend should not generate these.
       *)
    | RTL of word ref * word * stm (* an RTL *) 
    | RTLPHI of int (* a phi-function at block id *)
    | RTLPINNED of stm (* pinned statement *)
    | RTLPAR of stm list (* parallel execution *)
   
  and rexp = 
      REG    of ty * src

      (* sizes of constants are inferred by context *)
    | LI     of int   
    | LI32   of Word32.word
    | LI64   of Word64.word
    | LABEL  of LabelExp.labexp
    | CONST  of Constant.const

    | ADD    of ty * rexp * rexp
    | SUB    of ty * rexp * rexp 

      (* signed multiplication etc. *)
    | MULS   of ty * rexp * rexp
    | DIVS   of ty * rexp * rexp
    | REMS   of ty * rexp * rexp

      (* unsigned multiplication etc. *)
    | MULU   of ty * rexp * rexp
    | DIVU   of ty * rexp * rexp 
    | REMU   of ty * rexp * rexp

      (* trapping versions of above. These are all signed *)
    | ADDT   of ty * rexp * rexp 
    | SUBT   of ty * rexp * rexp 
    | MULT   of ty * rexp * rexp
    | DIVT   of ty * rexp * rexp
    | REMT   of ty * rexp * rexp 

    | ANDB   of ty * rexp * rexp
    | ORB    of ty * rexp * rexp
    | XORB   of ty * rexp * rexp
    | NOTB   of ty * rexp

    | SRA   of ty * rexp * rexp		(* value, shift *)
    | SRL   of ty * rexp * rexp
    | SLL   of ty * rexp * rexp

      (* type promotion *)
    | CVTI2I of ty * ext * rexp
    | CVTF2I of ty * rounding_mode * fexp

      (* conditional (eager) evaluation *)
    | COND of ty * ccexp * rexp * rexp

      (* integer load *)
    | LOAD of ty * rexp * Region.region
    | LOAD_UNALIGNED of ty * rexp * Region.region

    | SEQ of stm * rexp

    | EXTENSION of rextension * rexp list

    | MARK of rexp * Annotations.annotation

      (* Used in RTL *)
    | RTLPC (* the program counter; used for describing relative addressing *)
    | RTLMISC of misc_op ref * rexp list

  and fexp =
      FREG   of fty * src
    | FLOAD  of fty * rexp * Region.region
    | FLOAD_UNALIGNED  of fty * rexp * Region.region

    | FADD   of fty * fexp * fexp
    | FMUL   of fty * fexp * fexp
    | FSUB   of fty * fexp * fexp 
    | FDIV   of fty * fexp * fexp
    | FABS   of fty * fexp 
    | FNEG   of fty * fexp
    | FSQRT  of fty * fexp

    | CVTI2F of fty * ext * rexp
    | CVTF2F of fty * rounding_mode * fexp
    | FSEQ   of stm * fexp

    | FEXTENSION of fextension * fexp list

    | FMARK of fexp * Annotations.annotation

      (* used in RTL *)
    | RTLFMISC of misc_op ref * fexp list

  and ccexp =
      CC     of src
    | CMP    of ty * cond * rexp * rexp 
    | FCMP   of fty * fcond * fexp * fexp
    | CCMARK of ccexp * Annotations.annotation
    | RTLCCMISC of misc_op ref * ccexp list

  and mlrisc = CCR of ccexp | GPR of rexp | FPR of fexp

  datatype mltree = 
      BEGINCLUSTER
    | PSEUDO_OP of PseudoOp.pseudo_op
    | DEFINELABEL of Label.label
    | ENTRYLABEL of Label.label
    | CODE of stm list
    | ALIASDECLS of alias list
    | PHIFUNS of phi list
    | BLOCK_NAME of BNames.name
    | BLOCK_ANNOTATION of Annotations.annotation
    | ESCAPEBLOCK of mlrisc list 
    | ENDCLUSTER of int Intmap.intmap * Annotations.annotations

  exception Unsupported of string * rexp

end (* MLTREE *)
