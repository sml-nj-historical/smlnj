(* mltree.sig
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

(*
 * Note, this version MLTREE is now typed.
 * Furthermore, it is also used as an RTL language for SSA optimizations.
 *
 * --- Allen
 *)
signature MLTREE = sig
  structure Constant : CONSTANT
  structure PseudoOp : PSEUDO_OPS
  structure Region   : REGION
  structure BNames   : BLOCK_NAMES
  structure Stream   : INSTRUCTION_STREAM
     sharing Stream.B = BNames
     sharing Stream.P = PseudoOp

  include MLTREE_BASIS

  type rextension 
  type fextension 

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
  type alias = var * reg (* var is aliased to register *) 

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
    | RTL of word ref * word * stm (* a RTL that has been cached *) 
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

      (* 
       * COND(ty,cc,e1,e2):
       * Evaluate into either e1 or e2, depending on cc.  
       * Both e1 and e2 can be evaluated eagerly.
       *)
    | COND of ty * ccexp * rexp * rexp 

      (* integer load *)
    | LOAD of ty * rexp * Region.region
    | LOAD_UNALIGNED of ty * rexp * Region.region

    | SEQ of stm * rexp

    | EXTENSION of ty * rextension * rexp list

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

    | FEXTENSION of fty * fextension * fexp list

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

  exception Unsupported of string * rexp

  type ('i,'regmap) stream = 
       ('i -> unit,'regmap,Annotations.annotations,
        mlrisc list, alias, phi) Stream.stream 

end (* MLTREE *)

