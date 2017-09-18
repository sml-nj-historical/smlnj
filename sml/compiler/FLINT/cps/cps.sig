(* Copyright 1996 by Bell Laboratories *)
(* cps.sig *)

signature CPS = sig

datatype record_kind
  = RK_VECTOR
  | RK_RECORD
  | RK_SPILL
  | RK_ESCAPE
  | RK_EXN
  | RK_CONT
  | RK_FCONT
  | RK_KNOWN
  | RK_BLOCK
  | RK_FBLOCK
  | RK_I32BLOCK

datatype pkind = VPT | RPT of int | FPT of int
datatype cty = INTt | INT32t | PTRt of pkind
         | FUNt | FLTt | CNTt | DSPt

structure P : sig

    (* numkind includes kind and size *)
    datatype numkind = INT of int | UINT of int | FLOAT of int

    datatype arithop = + | - | * | / | ~ | abs 
                     | fsqrt | fsin | fcos | ftan 
	             | lshift | rshift | rshiftl | andb | orb | xorb | notb
		     | rem | div | mod

    datatype cmpop = > | >= | < | <= | eql | neq

    (* fcmpop conforms to the IEEE std 754 predicates. *)
    datatype fcmpop 
      = fEQ (* = *)  | fULG (* ?<> *) | fUN (* ? *)   | fLEG (* <=> *) 
      | fGT (* > *)  | fGE  (* >= *)  | fUGT (* ?> *) | fUGE (* ?>= *) 
      | fLT (* < *)  | fLE  (* <= *)  | fULT (* ?< *) | fULE (* ?<= *) 
      | fLG (* <> *) | fUE  (* ?= *) | fsgn


    (* These are two-way branches dependent on pure inputs *)
    datatype branch
      = cmp of {oper: cmpop, kind: numkind}
      | fcmp of {oper: fcmpop, size: int}
      | boxed | unboxed | peql | pneq
      | streq | strneq  
          (* streq(n,a,b) is defined only if strings a and b have
	     exactly the same length n>1 *)

    (* These all update the store *)
    datatype setter
      = numupdate of {kind: numkind}
      | unboxedupdate | boxedupdate | update
      | unboxedassign | assign
      | sethdlr | setvar | setspecial
      | free | acclink | setpseudo | setmark
      | rawstore of {kind: numkind}
      | rawupdate of cty

    (* These fetch from the store, never have functions as arguments. *)
    datatype looker
      = ! | subscript | numsubscript of {kind: numkind} | getspecial
      | gethdlr | getvar | getpseudo
      | rawload of {kind: numkind}

  (* These might raise exceptions, never have functions as arguments.*)
    datatype arith
      = arith of {oper: arithop, kind: numkind}
      | test of int * int
      | testu of int * int
      | test_inf of int
      | round of {floor: bool, fromkind: numkind, tokind: numkind}

  (* These don't raise exceptions and don't access the store. *)
    datatype pure
      = pure_arith of {oper: arithop, kind: numkind}
      | pure_numsubscript of {kind: numkind}
      | length | objlength | makeref
      | extend of int * int
      | trunc of int * int
      | copy of int * int
      | extend_inf of int
      | trunc_inf of int
      | copy_inf of int
      | real of {fromkind: numkind, tokind: numkind}
      | subscriptv
      | gettag | mkspecial | wrap | unwrap | cast | getcon | getexn
      | fwrap | funwrap | iwrap | iunwrap | i32wrap | i32unwrap
      | getseqdata | recsubscript | raw64subscript | newarray0
      | rawrecord of record_kind option 
           (* allocate uninitialized words from the heap; optionally
            * initialize the tag.
            *)
      | condmove of branch (* conditional move *)

    val opp : branch -> branch

    val iadd : arith
    val isub : arith
    val imul : arith
    val idiv : arith
    val ineg : arith
		    
    val fadd : arith
    val fsub : arith
    val fmul : arith
    val fdiv : arith
    val fneg : arith

    val ieql : branch
    val ineq : branch
    val igt  : branch
    val ige  : branch
    val ile  : branch
    val ilt  : branch
(*  val iltu : branch
 *  val igeu : branch
 *)
    val feql : branch
    val fneq : branch
    val fgt  : branch
    val fge  : branch
    val fle  : branch
    val flt  : branch

    val arity : arithop -> int 

end (* P *)

type lvar 

datatype value 
  = VAR of lvar
  | LABEL of lvar
  | INT of int
  | INT32 of Word32.word
  | REAL of string
  | STRING of string
  | OBJECT of Unsafe.Object.object
  | VOID

datatype accesspath 
  = OFFp of int 
  | SELp of int * accesspath

datatype fun_kind
  = CONT           
  | KNOWN          
  | KNOWN_REC      
  | KNOWN_CHECK    
  | KNOWN_TAIL     
  | KNOWN_CONT     
  | ESCAPE         
  | NO_INLINE_INTO 

datatype cexp
  = RECORD of record_kind * (value * accesspath) list * lvar * cexp
  | SELECT of int * value * lvar * cty * cexp
  | OFFSET of int * value * lvar * cexp
  | APP of value * value list
  | FIX of function list * cexp
  | SWITCH of value * lvar * cexp list
  | BRANCH of P.branch * value list * lvar * cexp * cexp
  | SETTER of P.setter * value list * cexp
  | LOOKER of P.looker * value list * lvar * cty * cexp
  | ARITH of P.arith * value list * lvar * cty * cexp
  | PURE of P.pure * value list * lvar * cty * cexp
  (* experimental "raw C call" (Blume, 1/2001) *)
  (* When non-empty, the string contains the linkage info, which
   * is a string of the form:
   *      shared library name/name of the C function. 
   *) 
  | RCC of rcc_kind * string * CTypes.c_proto * value list *
	   (lvar * cty) list * cexp
and rcc_kind = FAST_RCC | REENTRANT_RCC
withtype function = fun_kind * lvar * lvar list * cty list * cexp

val combinepaths : accesspath * accesspath -> accesspath
val lenp : accesspath -> int
val ctyToString : cty -> string
val hasRCC : cexp -> bool
val sizeOf : cty -> int   (* size of its representation in bits *)
val isFloat : cty -> bool (* is it a floating point type? *)
val isTagged : cty -> bool 

val BOGt : cty

val ctyc  : LtyDef.tyc -> cty
val ctype : LtyDef.lty -> cty
end (* signature CPS *)

