(* mltree.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

functor MLTreeF(structure LabelExp  : LABELEXP
		structure Region    : REGION
                structure Stream    : INSTRUCTION_STREAM
                structure Extension : MLTREE_EXTENSION
               ) : MLTREE =
struct
  structure LabelExp = LabelExp
  structure Constant = LabelExp.Constant
  structure PseudoOp = Stream.P
  structure Stream = Stream
  structure Region = Region
  structure Basis  = MLTreeBasis
  structure Extension = Extension

  type ty  = Basis.ty
  type fty = Basis.fty
  type var = int (* variable *)
  type src = var (* source variable *)
  type dst = var (* destination variable *)
  type reg = var (* physical register *)
  type an = Annotations.annotation

  datatype cond = datatype Basis.cond
  datatype fcond = datatype Basis.fcond
  datatype rounding_mode = datatype Basis.rounding_mode
  datatype ext = datatype Basis.ext

  (* phi-functions for SSA form *)
  datatype phi =
      RPHI  of ty * dst * src list 
    | FPHI  of fty * dst * src list 
    | CCPHI of dst * src list 

  (* aliasing declarations 
   * These are used to define physical register bindings for SSA names 
   *)
  type alias = var * reg (* var is aliased to register *) 

  (* Statements/effects.  These types are parameterized by the statement
   * extension type.  Unfortunately, this has to be made polymorphic to make
   * it possible for recursive type definitions to work. 
   *
   * Terms marked with rtl are used within the rtl language 
   *)
  datatype stm =
      (* assignment *)
      MV      of ty * dst * rexp   (* rtl *)
    | CCMV    of dst * ccexp
    | FMV     of fty * dst * fexp	

      (* parallel copies *)
    | COPY    of ty * dst list * src list   (* rtl *)
    | FCOPY   of fty * dst list * src list

      (* control flow *)
    | JMP     of ctrls * rexp * controlflow (* rtl *)
    | BCC     of ctrls * ccexp * Label.label
    | CALL    of {funct:rexp, targets:controlflow,
                  defs:mlrisc list, uses:mlrisc list,
                  cdefs:ctrls, cuses: ctrls, region: Region.region} (* rtl *)
    | RET     of ctrls * controlflow (* rtl *)
    | JOIN    of ctrls
    | IF      of ctrls * ccexp * stm * stm   (* rtl *)

      (* memory update: ea, data *)
    | STORE  of ty * rexp * rexp * Region.region 
    | FSTORE of fty * rexp * fexp * Region.region 

      (* control dependence *)
    | REGION of stm * ctrl

    | SEQ    of stm list   (* sequencing *)
    | DEFINE of Label.label   (* define local label *)

    | ANNOTATION of stm * an
    | EXT of sext  (* extension *)

      (* RTL operators:
       * The following are used internally for describing instruction semantics.
       * The frontend must not use these.
       *)
    | PHI    of int                    (* a phi-function at some block id *)
    | PINNED of stm      (* pinned statement *)
    | RTL    of {hash:word ref, attribs:Basis.attribs, e:stm}
   
  and rexp = 
      REG    of ty * reg            (* rtl *)

      (* sizes of constants are inferred by context *)
    | LI     of int                 (* rtl *)
    | LI32   of Word32.word         (* rtl *)
    | LI64   of Word64.word         (* rtl *)
    | LABEL  of LabelExp.labexp
    | CONST  of Constant.const

    | NEG    of ty * rexp                      
    | ADD    of ty * rexp * rexp    (* rtl *)
    | SUB    of ty * rexp * rexp    (* rtl *)

      (* signed multiplication etc. *)
    | MULS   of ty * rexp * rexp    (* rtl *)
    | DIVS   of ty * rexp * rexp    (* rtl *)
    | QUOTS  of ty * rexp * rexp    (* rtl *)
    | REMS   of ty * rexp * rexp    (* rtl *)

      (* unsigned multiplication etc. *)
    | MULU   of ty * rexp * rexp    (* rtl *)
    | DIVU   of ty * rexp * rexp    (* rtl *)
    | REMU   of ty * rexp * rexp    (* rtl *)

      (* trapping versions of above. These are all signed *)
    | NEGT   of ty * rexp                       
    | ADDT   of ty * rexp * rexp    (* rtl *)
    | SUBT   of ty * rexp * rexp    (* rtl *)
    | MULT   of ty * rexp * rexp    (* rtl *)
    | DIVT   of ty * rexp * rexp    (* rtl *)
    | QUOTT  of ty * rexp * rexp    (* rtl *)
    | REMT   of ty * rexp * rexp    (* rtl *)

      (* bit operations *)
    | ANDB   of ty * rexp * rexp    (* rtl *)
    | ORB    of ty * rexp * rexp    (* rtl *)
    | XORB   of ty * rexp * rexp    (* rtl *)
    | NOTB   of ty * rexp              (* rtl *)

    | SRA   of ty * rexp * rexp	  (* value, shift *) (* rtl *)
    | SRL   of ty * rexp * rexp     (* rtl *)
    | SLL   of ty * rexp * rexp     (* rtl *)

      (* type promotion/conversion *)
    | CVTI2I of ty * ext * ty * rexp  (* signed extension *) (* rtl *)
    | CVTF2I of ty * rounding_mode * fty * fexp (* rtl *)

      (* 
       * COND(ty,cc,e1,e2):
       * Evaluate into either e1 or e2, depending on cc.  
       * Both e1 and e2 are allowed to be evaluated eagerly.
       *)
    | COND of ty * ccexp * rexp * rexp  (* rtl *)

      (* integer load *)
    | LOAD of ty * rexp * Region.region (* rtl *)

      (* predication *)
    | PRED of rexp * ctrl 

    | LET of stm * rexp

    | REXT of ty * rext

    | MARK of rexp * an

  and fexp =
      FREG   of fty * src
    | FLOAD  of fty * rexp * Region.region 

    | FADD   of fty * fexp * fexp
    | FMUL   of fty * fexp * fexp
    | FSUB   of fty * fexp * fexp 
    | FDIV   of fty * fexp * fexp
    | FABS   of fty * fexp 
    | FNEG   of fty * fexp
    | FSQRT  of fty * fexp
    | FCOND  of fty * ccexp * 
                fexp * fexp
    | FCOPYSIGN of fty * fexp (*sign*) * fexp (*magnitude*)

    | CVTI2F of fty * ty * rexp  (* from signed integer *)
    | CVTF2F of fty * fty * fexp (* float to float conversion *)

    | FPRED of fexp * ctrl
 
    | FEXT of fty * fext

    | FMARK of fexp * an

  and ccexp =
      CC     of Basis.cond * src                        (* rtl *)
    | FCC    of Basis.fcond * src                       (* rtl *)
    | TRUE                                              (* rtl *)
    | FALSE                                             (* rtl *)
    | NOT    of ccexp                     (* rtl *)
    | AND    of ccexp * ccexp   (* rtl *)
    | OR     of ccexp * ccexp   (* rtl *)
    | XOR    of ccexp * ccexp   (* rtl *)
    | CMP    of ty * Basis.cond * rexp * rexp(*rtl*)
    | FCMP   of fty * Basis.fcond * fexp * fexp
    | CCMARK of ccexp * an
    | CCEXT  of ty * ccext

  and mlrisc = 
      CCR of ccexp 
    | GPR of rexp 
    | FPR of fexp 

  withtype controlflow = Label.label list (* control flow info *)
       and ctrl   = var                   (* control dependence info *)
       and ctrls  = ctrl list
       and sext   = (stm, rexp, fexp, ccexp) Extension.sx
       and rext   = (stm, rexp, fexp, ccexp) Extension.rx
       and fext   = (stm, rexp, fexp, ccexp) Extension.fx
       and ccext  = (stm, rexp, fexp, ccexp) Extension.ccx

  (*
   * Instruction streams
   *)
  type ('i,'regmap,'cellset) stream = 
       ('i -> unit,'regmap, an list, 'cellset, alias, phi) Stream.stream 

  (* 
   * Extension mechanism
   *)

  datatype ('instr,'regmap,'cellset,'operand,'addressing_mode) reducer =
    REDUCER of
    { reduceRexp    : rexp -> reg,
      reduceFexp    : fexp -> reg,
      reduceCCexp   : ccexp -> reg,
      reduceStm     : stm * an list -> unit,
      operand       : rexp -> 'operand,
      reduceOperand : 'operand -> reg,
      addressOf     : rexp -> 'addressing_mode,
      emit          : 'instr * an list -> unit,
      instrStream   : ('instr,'regmap,'cellset) stream,
      mltreeStream  : (stm,'regmap,mlrisc list) stream
    }

  (*
   * Useful type abbreviations for working for MLTree.
   *)
  type rewriter =  (* rewriting functions *)
    {stm:stm->stm, rexp:rexp->rexp, fexp:fexp->fexp, ccexp:ccexp->ccexp}
  type 'a folder = (* aggregation functions *)
    {stm:stm*'a->'a, rexp:rexp*'a->'a, fexp:fexp*'a->'a, ccexp:ccexp*'a->'a}
  type hasher =    (* hashing functions *)
    {stm:stm->word, rexp:rexp->word, fexp:fexp->word, ccexp:ccexp->word}
  type equality =  (* comparison functions *)
    {stm:stm * stm->bool, rexp:rexp * rexp->bool, 
     fexp:fexp * fexp->bool, ccexp:ccexp * ccexp->bool}
  type printer =   (* pretty printing functions *)
    {stm:stm->string, rexp:rexp->string, fexp:fexp->string, ccexp:ccexp->string,
     dstReg : ty * var -> string, srcReg : ty * var -> string}

end (* MLTREE *)
