(* mltree.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

functor MLTreeF(structure LabelExp : LABELEXP
		structure R : REGION
                structure S : INSTRUCTION_STREAM
               ) : MLTREE =
struct
  structure LabelExp = LabelExp
  structure Constant = LabelExp.Constant
  structure PseudoOp = S.P
  structure Stream = S
  structure Region = R
  structure Basis  = MLTreeBasis

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
    | FPHI of fty * dst * src list 
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
  datatype ('s,'r,'f,'c) stm =
      (* assignment *)
      MV      of ty * dst * ('s,'r,'f,'c) rexp   (* rtl *)
    | CCMV    of dst * ('s,'r,'f,'c) ccexp
    | FMV     of fty * dst * ('s,'r,'f,'c) fexp	

      (* parallel copies *)
    | COPY    of ty * dst list * src list   (* rtl *)
    | FCOPY   of fty * dst list * src list

      (* control flow *)
    | JMP     of ctrls * ('s,'r,'f,'c) rexp * controlflow (* rtl *)
    | BCC     of ctrls * ('s,'r,'f,'c) ccexp * Label.label
    | CALL    of ('s,'r,'f,'c) rexp * controlflow * ('s,'r,'f,'c) mlrisc list * ('s,'r,'f,'c) mlrisc list * 
                 ctrls * ctrls * Region.region (* rtl *)
    | RET     of ctrls * controlflow (* rtl *)
    | JOIN    of ctrls
    | IF      of ctrls * ('s,'r,'f,'c) ccexp * 
                  ('s,'r,'f,'c) stm * ('s,'r,'f,'c) stm   (* rtl *)

      (* memory update: ea, data *)
    | STORE  of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp * Region.region 
    | FSTORE of fty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) fexp * Region.region 

      (* control dependence *)
    | REGION of ('s,'r,'f,'c) stm * ctrl

    | SEQ    of ('s,'r,'f,'c) stm list   (* sequencing *)
    | DEFINE of Label.label   (* define local label *)

    | ANNOTATION of ('s,'r,'f,'c) stm * an
    | EXT of 's    (* extension *)

      (* RTL operators:
       * The following are used internally for describing instruction semantics.
       * The frontend must not use these.
       *)
    | PHI    of int                    (* a phi-function at some block id *)
    | PINNED of ('s,'r,'f,'c) stm      (* pinned statement *)
    | RTL    of {hash:word ref, attribs:Basis.attribs, e:('s,'r,'f,'c) stm}
   
  and ('s,'r,'f,'c) rexp = 
      REG    of ty * reg            (* rtl *)

      (* sizes of constants are inferred by context *)
    | LI     of int                 (* rtl *)
    | LI32   of Word32.word         (* rtl *)
    | LI64   of Word64.word         (* rtl *)
    | LABEL  of LabelExp.labexp
    | CONST  of Constant.const

    | NEG    of ty * ('s,'r,'f,'c) rexp                      
    | ADD    of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | SUB    of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)

      (* signed multiplication etc. *)
    | MULS   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | DIVS   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | QUOTS  of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | REMS   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)

      (* unsigned multiplication etc. *)
    | MULU   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | DIVU   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | REMU   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)

      (* trapping versions of above. These are all signed *)
    | NEGT   of ty * ('s,'r,'f,'c) rexp                       
    | ADDT   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | SUBT   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | MULT   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | DIVT   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | QUOTT  of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | REMT   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)

      (* bit operations *)
    | ANDB   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | ORB    of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | XORB   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp    (* rtl *)
    | NOTB   of ty * ('s,'r,'f,'c) rexp              (* rtl *)

    | SRA   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp	  (* value, shift *) (* rtl *)
    | SRL   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp     (* rtl *)
    | SLL   of ty * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp     (* rtl *)

      (* type promotion/conversion *)
    | CVTI2I of ty * ext * ty * ('s,'r,'f,'c) rexp  (* signed extension *) (* rtl *)
    | CVTF2I of ty * rounding_mode * fty * ('s,'r,'f,'c) fexp (* rtl *)

      (* 
       * COND(ty,cc,e1,e2):
       * Evaluate into either e1 or e2, depending on cc.  
       * Both e1 and e2 are allowed to be evaluated eagerly.
       *)
    | COND of ty * ('s,'r,'f,'c) ccexp * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp  (* rtl *)

      (* integer load *)
    | LOAD of ty * ('s,'r,'f,'c) rexp * Region.region (* rtl *)

      (* predication *)
    | PRED of ('s,'r,'f,'c) rexp * ctrl 

    | LET of ('s,'r,'f,'c) stm * ('s,'r,'f,'c) rexp

    | REXT of ty * 'r

    | MARK of ('s,'r,'f,'c) rexp * an

  and ('s,'r,'f,'c) fexp =
      FREG   of fty * src
    | FLOAD  of fty * ('s,'r,'f,'c) rexp * Region.region 

    | FADD   of fty * ('s,'r,'f,'c) fexp * ('s,'r,'f,'c) fexp
    | FMUL   of fty * ('s,'r,'f,'c) fexp * ('s,'r,'f,'c) fexp
    | FSUB   of fty * ('s,'r,'f,'c) fexp * ('s,'r,'f,'c) fexp 
    | FDIV   of fty * ('s,'r,'f,'c) fexp * ('s,'r,'f,'c) fexp
    | FABS   of fty * ('s,'r,'f,'c) fexp 
    | FNEG   of fty * ('s,'r,'f,'c) fexp
    | FSQRT  of fty * ('s,'r,'f,'c) fexp
    | FCOND  of fty * ('s,'r,'f,'c) ccexp * 
                ('s,'r,'f,'c) fexp * ('s,'r,'f,'c) fexp
    | FCOPYSIGN of fty * ('s,'r,'f,'c) fexp (*sign*) * 
                         ('s,'r,'f,'c) fexp (*magnitude*)

    | CVTI2F of fty * ty * ('s,'r,'f,'c) rexp  (* from signed integer *)
    | CVTF2F of fty * fty * ('s,'r,'f,'c) fexp (* float to float conversion *)

    | FPRED of ('s,'r,'f,'c) fexp * ctrl
 
    | FEXT of fty * 'f

    | FMARK of ('s,'r,'f,'c) fexp * an

  and ('s,'r,'f,'c) ccexp =
      CC     of Basis.cond * src                        (* rtl *)
    | FCC    of Basis.fcond * src                       (* rtl *)
    | TRUE                                              (* rtl *)
    | FALSE                                             (* rtl *)
    | NOT    of ('s,'r,'f,'c) ccexp                     (* rtl *)
    | AND    of ('s,'r,'f,'c) ccexp * ('s,'r,'f,'c) ccexp   (* rtl *)
    | OR     of ('s,'r,'f,'c) ccexp * ('s,'r,'f,'c) ccexp   (* rtl *)
    | XOR    of ('s,'r,'f,'c) ccexp * ('s,'r,'f,'c) ccexp   (* rtl *)
    | CMP    of ty * Basis.cond * ('s,'r,'f,'c) rexp * ('s,'r,'f,'c) rexp(*rtl*)
    | FCMP   of fty * Basis.fcond * ('s,'r,'f,'c) fexp * ('s,'r,'f,'c) fexp
    | CCMARK of ('s,'r,'f,'c) ccexp * an
    | CCEXT  of ty * 'c

  and ('s,'r,'f,'c) mlrisc = 
      CCR of ('s,'r,'f,'c) ccexp 
    | GPR of ('s,'r,'f,'c) rexp 
    | FPR of ('s,'r,'f,'c) fexp 

  withtype controlflow = Label.label list (* control flow info *)
       and ctrl = var                     (* control dependence info *)
       and ctrls = ctrl list

  (*
   * Instruction streams
   *)
  type ('i,'regmap,'cellset) stream = 
       ('i -> unit,'regmap, an list, 'cellset, alias, phi) Stream.stream 

  (* Extension mechanism *)

  (* These functions are supplied by the instruction selection module *)
  datatype ('instr,'regmap,'cellset,'operand,'addressing_mode,'s,'r,'f,'c) 
    reducer =
    REDUCER of
    { reduceRexp    : ('s,'r,'f,'c) rexp -> reg,
      reduceFexp    : ('s,'r,'f,'c) fexp -> reg,
      reduceCCexp   : ('s,'r,'f,'c) ccexp -> reg,
      reduceStm     : ('s,'r,'f,'c) stm * an list -> unit,
      operand       : ('s,'r,'f,'c) rexp -> 'operand,
      reduceOperand : 'operand -> reg,
      addressOf     : ('s,'r,'f,'c) rexp -> 'addressing_mode,
      emit          : 'instr * an list -> unit,
      instrStream   : ('instr,'regmap,'cellset) stream,
      mltreeStream  : 
        (('s,'r,'f,'c) stm,'regmap,('s,'r,'f,'c) mlrisc list) stream
    }


  (* These functions should be provided by the client *)
  datatype ('instr,'regmap,'cellset,'operand,'addressing_mode,'s,'r,'f,'c)
           extender =
    EXTENDER of
    { compileStm  :
           ('instr,'regmap,'cellset,'operand,'addressing_mode,'s,'r,'f,'c)
           reducer -> { stm : 's, an : an list} -> unit,
      compileRexp :
           ('instr,'regmap,'cellset,'operand,'addressing_mode,'s,'r,'f,'c)
           reducer -> {e:ty * 'r, an:an list, rd:reg} -> unit,
      compileFexp :
           ('instr,'regmap,'cellset,'operand,'addressing_mode,'s,'r,'f,'c)
           reducer -> {e:fty * 'f, an:an list, fd:reg} -> unit,
      compileCCexp :
           ('instr,'regmap,'cellset,'operand,'addressing_mode,'s,'r,'f,'c)
           reducer -> {e:ty * 'c, an:an list, cd:reg} -> unit
    }

end (* MLTREE *)

