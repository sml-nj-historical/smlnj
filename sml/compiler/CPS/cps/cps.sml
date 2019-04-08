(* cps.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CPS : CPS =
  struct

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

  (* kinds of integers: size in bits and tagged vs boxed *)
    type intty = {sz : int, tag : bool}

    datatype cty
      = NUMt of intty	(* integers of the given type *)
      | PTRt of pkind	(* pointer *)
      | FUNt		(* function? *)
      | FLTt of int 	(* float of given size *)
      | CNTt		(* continuation *)

    structure P =
      struct
      (* numkind includes kind and size *)
	datatype numkind = INT of int | UINT of int | FLOAT of int

	datatype arithop
	  = ADD | SUB | MUL | DIV | MOD | QUOT | REM | FDIV
	  | LSHIFT | RSHIFT | RSHIFTL | ANDB | ORB | XORB
	  | NEG | ABS | NOTB
	  | FSQRT | FSIN | FCOS | FTAN
(*
	datatype arithop = + | - | * | / | ~ | abs
			 | fsqrt | fsin | fcos | ftan
			 | lshift | rshift | rshiftl | andb | orb | xorb | notb
			 | rem | div | mod
*)

	datatype cmpop = GT | GTE | LT | LTE | EQL | NEQ
(*
	datatype cmpop = > | >= | < | <= | eql | neq
*)

      (* fcmpop conforms to the IEEE std 754 predicates. *)
	datatype fcmpop
	  = fEQ (* = *)  | fULG (* ?<> *) | fUN (* ? *)   | fLEG (* <=> *)
	  | fGT (* > *)  | fGE  (* >= *)  | fUGT (* ?> *) | fUGE (* ?>= *)
	  | fLT (* < *)  | fLE  (* <= *)  | fULT (* ?< *) | fULE (* ?<= *)
	  | fLG (* <> *) | fUE  (* ?= *)  | fsgn

      (* These are two-way branches dependent on pure inputs *)
	datatype branch
	  = cmp of {oper: cmpop, kind: numkind}    (* numkind cannot be FLOAT *)
	  | fcmp of {oper: fcmpop, size: int}
	  | boxed | unboxed | peql | pneq
	  | streq | strneq
	      (* streq(n,a,b) is defined only if strings a and b have
		 exactly the same length n>1 *)

      (* These all update the store *)
	datatype setter
	  = numupdate of {kind: numkind}
	  | unboxedupdate | update
	  | unboxedassign | assign
	  | sethdlr | setvar | setspecial
	  | rawstore of {kind: numkind}
	  | rawupdate of cty

      (* These fetch from the store, never have functions as arguments. *)
	datatype looker
	  = ! | subscript | numsubscript of {kind: numkind}
	  | getspecial | gethdlr | getvar
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
	  | gettag | mkspecial | cast | getcon | getexn
	  | box | unbox
	(* tagging/boxing of numbers; numkind should be either `INT` or `FLOAT` *)
	  | wrap of numkind | unwrap of numkind
	  | getseqdata | recsubscript | raw64subscript | newarray0
	(* allocate uninitialized words from the heap *)
	  | rawrecord of record_kind option

      end (* P *)

    type lvar = LambdaVar.lvar

    datatype value
      = VAR of lvar
      | LABEL of lvar
      | NUM of intty IntConst.t
      | REAL of int RealConst.t
      | STRING of string
      | VOID

    datatype accesspath
      = OFFp of int
      | SELp of int * accesspath

    datatype fun_kind
      = CONT           (* continuation functions *)
      | KNOWN          (* general known functions *)
      | KNOWN_REC      (* known recursive functions *)
      | KNOWN_CHECK    (* known functions that need a heap limit check *)
      | KNOWN_TAIL     (* tail-recursive kernal *)
      | KNOWN_CONT     (* known continuation functions *)
      | ESCAPE         (* before the closure phase, any user function;
			  after the closure phase, escaping user function *)
      | NO_INLINE_INTO (* before the closure phase,
			  a user function inside of which no in-line expansions
			  should be performed;
			  does not occur after the closure phase *)

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
      | RCC of rcc_kind * string * CTypes.c_proto * value list * (lvar * cty) list * cexp

    and rcc_kind = FAST_RCC | REENTRANT_RCC

    withtype function = fun_kind * lvar * lvar list * cty list * cexp

  end (* structure CPS *)
