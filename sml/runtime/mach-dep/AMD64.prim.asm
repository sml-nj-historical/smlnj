/*! \file AMD64.prim.asm
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "assyntax64.h"
#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "ml-limits.h"
	
#if defined(OPSYS_DARWIN)
/* Note: although the MacOS assembler claims to be the GNU assembler, it appears to be
 * an old version (1.38), which uses different alignment directives.
 */
#undef ALIGNTEXT4
#undef ALIGNDATA4
#define ALIGNTEXT4	.align 2
#define ALIGNDATA4	.align 2
#endif

/*
 *
 * The AMD64 registers are used as follows:
 *
 * RAX - temp1 (see the code generator, amd64/amd64.sml)
 * RBX - misc0
 * RCX - misc1
 * RDX - misc2
 * RSI - standard continuation (ml_cont, see ml_state.h)
 * RBP - standard argument (ml_arg)
 * RDI - free space pointer (ml_allocptr)
 * RSP - stack pointer
 * R8  - standard link
 * R9  - standard closure
 * R10 - misc3
 * R11 - misc4
 * R12 - misc5
 * R13 - misc6
 * R14 - limit ptr
 * R15 - store ptr
 * RIP - program counter (ml_pc)
 */   

/* Registers (see amd64/amd64CpsRegs.sml): */
#define temp		RAX
#define misc0		RBX
#define misc1		RCX
#define misc2		RDX
#define misc3		R10
#define misc4		R11
#define misc5		R12
#define misc6		R13
#define stdcont		RSI
#define stdarg		RBP
#define	stdlink		R8
#define	stdclos		R9
#define allocptr	RDI
#define limitptr	R14
#define storeptr	R15
#define stackptr        RSP

/* other reg uses */
#define creturn 	RAX

/* Stack fram offsets are w.r.t. the stack pointer.  See
 *
 *	https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/dev-notes/amd64-stack-frame.numbers
 *
 * for details.
 */
#define tempmem0	REGOFF(0,RSP)
#define tempmem1	REGOFF(8,RSP)
#define tempmem2	REGOFF(16,RSP)
#define tempmem3	REGOFF(24,RSP)
#define baseptr		REGOFF(32,RSP)
#define exncont		REGOFF(40,RSP)
#define pc		REGOFF(48,RSP) /* gcLink */
#define varptr		REGOFF(56,RSP)
#define start_gc	REGOFF(64,RSP) /* ?? */
#define signBit		REGOFF(208,RSP) /* ?? */
#define negateSignBit	REGOFF(216,RSP) /* ?? */

/* we put the request code in tempmem before jumping to set_request */
#define request_w	tempmem0

#define ML_STATE_OFFSET 176
#define mlstate_ptr	REGOFF(ML_STATE_OFFSET, RSP)
#define ML_SPILL_SIZE	8192
#define CALLEE_SAVE_SZB 48	/* rbx, rbp, r12-15 */
#define FRAME_SIZE	(

#include "mlstate-offsets.h"	/** this file is generated **/


/*
 * AMD64 function call conventions (System V ABI):
 *
 * 	Caller save registers: rax, rcx, rdx, rsi, rdi, r8-r11
 * 	Callee save registers: rbx, rbp, r12-15.
 *	Save frame pointer (ebx) first to match standard function prelude
 * 	Floating point state is caller-save.
 * 	The first six integer arguments are passed in registers: rdi, rsi,
 *	    rdx, rcx, r8, and r9.  Additional arguments are passed on the
 *	    stack (rightmost argument pushed first).
 * 	Word-sized result returned in %rax.
 *	The stack frame must be multiple of 16 bytes
 */

#define cresult	RAX


#define CALLEE_SAVE	\
	PUSH_Q(RBP);	\
	PUSH_Q(RBX);	\
	PUSH_Q(R12);	\
	PUSH_Q(R13);	\
	PUSH_Q(R14);	\
	PUSH_Q(R15)

#define CALLEE_RESTORE	\
	POP_Q(R15);	\
	POP_Q(R14);	\
	POP_Q(R13);	\
	POP_Q(R12);	\
	POP_Q(RBX);	\
	POP_Q(RBP);

/* MOVE copies one memory location to another, using a specified temporary. */

#define MOVE(src,tmp,dest)	\
	MOV_Q(src, tmp);	\
	MOV_Q(tmp, dest)

#define CONTINUE				\
	JMP(CODEPTR(stdcont))

#define CHECKLIMIT				\
 1:;						\
	MOVE(stdlink, temp, pc);		\
	CMP_Q(limitptr, allocptr);		\
	JB(9f);					\
	CALL(CSYM(saveregs));			\
	JMP(1b);				\
 9:

/**********************************************************************/
	SEG_TEXT
	ALIGNTEXT8

ML_CODE_HDR(sigh_return_a)
	MOV_Q(CONST(ML_unit),stdlink)
	MOV_Q(CONST(ML_unit),stdclos)
	MOV_Q(CONST(ML_unit),pc)
	MOV_L(CONST(REQ_SIG_RETURN), request_w)
	JMP(CSYM(set_request))

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */
ENTRY(sigh_resume)
	MOV_L(CONST(REQ_SIG_RESUME), request_w)
	JMP(CSYM(set_request))

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	MOV_Q(CONST(REQ_POLL_RETURN), request_w)
	MOV_Q(CONST(ML_unit),stdlink)
	MOV_Q(CONST(ML_unit),stdclos)
	MOV_Q(CONST(ML_unit),pc)
	JMP(CSYM(set_request))

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	MOV_L(CONST(REQ_POLL_RESUME), request_w)
	JMP(CSYM(set_request))

ML_CODE_HDR(handle_a)
	MOV_L(CONST(REQ_EXN), request_w)
	MOVE(stdlink,temp,pc)
	JMP(CSYM(set_request))

ML_CODE_HDR(return_a)
	MOV_Q(CONST(REQ_RETURN), request_w)
	MOV_Q(CONST(ML_unit),stdlink)
	MOV_Q(CONST(ML_unit),stdclos)
	MOV_Q(CONST(ML_unit),pc)
	JMP(CSYM(set_request))

/* Request a fault.  The floating point coprocessor must be reset
 * (thus trashing the FP registers) since we do not know whether a 
 * value has been pushed into the temporary "register".	 This is OK 
 * because no floating point registers will be live at the start of 
 * the exception handler.
 */
ENTRY(request_fault)
	CALL(CSYM(FPEEnable))          /* Does not trash any general regs. */
	MOV_L(CONST(REQ_FAULT), request_w)
	MOVE(stdlink,temp,pc)
	JMP(CSYM(set_request))

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT
	MOV_L(CONST(REQ_BIND_CFUN), request_w)
	JMP(CSYM(set_request))

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT
	MOV_L(CONST(REQ_BUILD_LITERALS), request_w)
	JMP(CSYM(set_request))

ML_CODE_HDR(callc_a)
	CHECKLIMIT
	MOV_L(CONST(REQ_CALLC), request_w)
	JMP(CSYM(set_request))

ENTRY(saveregs)
	POP_Q(pc)
	MOV_L(CONST(REQ_GC), request_w)
	/* fall into set_request */

ENTRY(set_request)
	/* temp holds mlstate_ptr, valid request in request_w  */
	/* Save registers */
	MOV_Q(mlstate_ptr, temp)
	MOV_Q(allocptr, REGOFF(AllocPtrOffMSP,temp))
	MOV_Q(stdarg, REGOFF(StdArgOffMSP,temp))
	MOV_Q(stdcont, REGOFF(StdContOffMSP,temp))

#define	temp2 allocptr
	/* note that we have left ML code */
	MOV_Q(REGOFF(VProcOffMSP,temp), temp2)
	MOV_Q(CONST(0), REGOFF(InMLOffVSP,temp2))

	MOV_Q(misc0, REGOFF(Misc0OffMSP,temp))
	MOV_Q(misc1, REGOFF(Misc1OffMSP,temp))
	MOV_Q(misc2, REGOFF(Misc2OffMSP,temp))

	/* Save vregs before the stack frame is popped. */
	MOVE(limitptr,temp2, REGOFF(LimitPtrOffMSP,temp))
	MOVE(exncont, temp2, REGOFF(ExnPtrOffMSP,temp)) 
	MOVE(stdclos, temp2, REGOFF(StdClosOffMSP,temp))
	MOVE(stdlink, temp2, REGOFF(LinkRegOffMSP,temp))
	MOVE(pc, temp2, REGOFF(PCOffMSP,temp))
	MOVE(storeptr,temp2, REGOFF(StorePtrOffMSP,temp))
	MOVE(varptr,  temp2, REGOFF(VarPtrOffMSP,temp))
#undef	temp2	
	
	/* return val of function is request code */
	MOV_Q(request_w,creturn)

	/* Pop the stack frame and return to run_ml(). */
#if defined(OPSYS_DARWIN)
	LEA_L(REGOFF(ML_FRAME_SIZE+12,ESP),ESP)
#else
	MOV_Q(rspsave, RSP)
#endif
	CALLEE_RESTORE
	RET

	SEG_TEXT
	ALIGNTEXT8
ENTRY(restoreregs)
	
	/* MOV_Q(REGOFF(4,RSP), temp */	/* Get argument (MLState ptr). */
	CALLEE_SAVE
	MOV_Q(%rdi, temp)
#if defined(OPSYS_DARWIN)
      /* MacOS X frames must be 16-byte aligned.  We have 20 bytes on
       * the stack for the return PC and callee-saves, so we need a
       * 12-byte pad.
       */
	SUB_L(CONST(ML_FRAME_SIZE+12), ESP)
#else
	/* Align sp on 8 byte boundary. Assumes that the stack
	 * starts out being at least word aligned. But who knows ...
	 */
/*
	MOV_L(ESP,EBX)
	OR_L(CONST(4), ESP)		
	SUB_L(CONST(4), ESP)		/ * stack grows from high to low * /
	SUB_L(CONST(ML_FRAME_SIZE), ESP)
	MOV_L(EBX,espsave)
*/
	MOV_Q(RSP,RBX)
	SUB_Q(CONST(ML_FRAME_SIZE),RSP)
	MOV_Q(RBX,rspsave)
#endif
	
#define temp2	RBX
      /* Initialize the ML stack frame. */
	MOVE(REGOFF(ExnPtrOffMSP, temp),  temp2, exncont)
	MOVE(REGOFF(LimitPtrOffMSP, temp), temp2, limitptr)
	MOVE(REGOFF(StorePtrOffMSP, temp), temp2, storeptr)
	MOVE(REGOFF(VarPtrOffMSP, temp),   temp2, varptr)
	LEA_Q(CSYM(saveregs), temp2)
	MOV_Q(temp2,start_gc)
	MOV_Q(temp, mlstate_ptr)

/* unclear what the following are being used for */
	MOV_Q($0x8000000000000000, temp2)
	MOV_Q(temp2, signBit)
	MOV_Q($0x7fffffffffffffff, temp2)
	MOV_Q(temp2, negateSignBit)

	/* vregs */
	MOVE	(REGOFF(LinkRegOffMSP,temp),  temp2, stdlink)
	MOVE	(REGOFF(StdClosOffMSP,temp),  temp2, stdclos)

	/* PC */
	MOVE    (REGOFF(PCOffMSP,temp), temp2, pc)
#undef	temp2

	/* Load ML registers. */
	MOV_Q(REGOFF(AllocPtrOffMSP,temp), allocptr)
	MOV_Q(REGOFF(StdContOffMSP,temp), stdcont)
	MOV_Q(REGOFF(StdArgOffMSP,temp), stdarg)
	MOV_Q(REGOFF(Misc0OffMSP,temp), misc0)
	MOV_Q(REGOFF(Misc1OffMSP,temp), misc1)
	MOV_Q(REGOFF(Misc2OffMSP,temp), misc2)

/* FIXME: probably don't need this instruction, since limitptr is in %r14 */
	MOV_Q(RSP, CSYM(ML_X86Frame))	/* frame ptr for signal handler. */

	PUSH_Q(misc2)			/* free up a register   */
	PUSH_Q(temp)			/* save msp temporarily */

#define	tmpreg	misc2

	/* note that we are entering ML */
	MOV_Q(REGOFF(VProcOffMSP,temp), temp)  /* temp is now vsp */
#define vsp	temp
	movl	CONST(1),REGOFF(InMLOffVSP,vsp)

	/* handle signals */
	movl	REGOFF(SigsRecvOffVSP,vsp),%edx
	cmpl	REGOFF(SigsHandledOffVSP,vsp),%edx
	
#undef  tmpreg
	JNE(pending)

restore_and_jmp_ml:
	popq	temp			/* restore temp to msp */
	POP_Q(misc2)
	
jmp_ml:
	cmpq	limitptr, allocptr
	JMP(CODEPTR(REGOFF(PCOffMSP,temp)))	/* Jump to ML code. */


pending:
					/* Currently handling signal? */
	CMP_L(CONST(0), REGOFF(InSigHandlerOffVSP,vsp))   
	JNE(restore_and_jmp_ml)
					/* handler trap is now pending */
	movl	IMMED(1),HandlerPendingOffVSP(vsp) 

	/* must restore here because limitptr is on stack */ /* XXX */
	popq	temp			/* restore temp to msp */
	POP_Q(misc2)

	MOV_Q(allocptr,limitptr)
	JMP(jmp_ml)			/* Jump to ML code. */
#undef  vsp

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT
	MOV_Q(REGIND(stdarg),temp)         /* temp := length in words */
	sarq	CONST(1),temp		     /* temp := length untagged */
	cmpq	CONST(SMALL_OBJ_SZW),temp     /* is this a small object */
	JGE(3f)

#define temp1 misc0
#define temp2 misc1
	pushq	misc0			     /* save misc0 */ 
	pushq	misc1			     /* save misc1 */
	
	MOV_Q(temp, temp1)
	salq	CONST(TAG_SHIFTW),temp1      /* build descriptor in temp1 */
	orq	CONST(MAKE_TAG(DTAG_arr_data)),temp1
	MOV_Q(temp1,REGIND(allocptr)	     /* store descriptor */
	addq	CONST(8),allocptr	     /* allocptr++ */
	MOV_Q(allocptr, temp1		     /* temp1 := array data ptr */
	MOV_Q(REGOFF(8,stdarg), temp2	     /* temp2 := initial value */ 
2:	
	MOV_Q(temp2, REGIND(allocptr)     /* initialize array */
	addq	CONST(8), allocptr
	subq	CONST(1), temp
	JNE(2b)

	/* Allocate array header */
	MOV_Q(CONST(DESC_polyarr),REGIND(allocptr) /* descriptor in temp */
	addq	CONST(8), allocptr	     /* allocptr++ */
	MOV_Q(REGIND(stdarg), temp	     /* temp := length */
	MOV_Q(allocptr, stdarg   	     /* result = header addr */ 
	MOV_Q(temp1, REGIND(allocptr)	     /* store pointer to data */
	MOV_Q(temp, REGOFF(8,allocptr)	     /* store length */
	addq	CONST(16), allocptr

	POP_Q(misc1)
	POP_Q(misc0)
	CONTINUE
#undef  temp1
#undef  temp2
3:
	MOV_Q(CONST(REQ_ALLOC_ARRAY),request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))
	

/* create_r : int -> realarray */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT
#define temp1 misc0
        pushq	misc0			/* free temp1 */
	MOV_Q(stdarg,temp		/* temp := length */
	sarq	CONST(1),temp		/* temp := untagged length */
	cmpq	CONST(SMALL_OBJ_SZW),temp
	JGE(2f)

	/* allocate the data object */
	MOV_Q(temp, temp1)
	shlq	CONST(TAG_SHIFTW),temp1  /* temp1 := descriptor */
	orq	CONST(MAKE_TAG(DTAG_raw64)),temp1
	MOV_Q(temp1,REGIND(allocptr)	/* store descriptor */
	addq	CONST(8), allocptr	/* allocptr++ */
	MOV_Q(allocptr, temp1		/* temp1 := data object */
	shlq	CONST(3),temp		/* temp := length in bytes */
	addq	temp, allocptr		/* allocptr += length */

	/* allocate the header object */
	MOV_Q(CONST(DESC_real64arr),REGIND(allocptr)/* header descriptor */
	addq	CONST(8), allocptr	/* allocptr++ */
	MOV_Q(temp1, REGIND(allocptr)	/* header data field */
	MOV_Q(stdarg, REGOFF(8,allocptr)	/* header length field */
	MOV_Q(allocptr, stdarg		/* stdarg := header object */
	addq	CONST(16), allocptr	/* allocptr += 2 */

	popq	misc0			/* restore temp1 */
	CONTINUE
2:
	popq	misc0			/* restore temp1 */
	MOV_Q(CONST(REQ_ALLOC_REALDARRAY), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))
#undef temp1


/* create_b : int -> bytearray */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT
	MOV_Q(stdarg,temp		/* temp := length(tagged int) */
	sarq	CONST(1),temp		/* temp := length(untagged) */
	addq	CONST(3),temp
	sarq	CONST(2),temp		/* temp := length(words) */
	cmpq	CONST(SMALL_OBJ_SZW),temp /* small object? */
	JMP(2f)
	JGE(2f)				/* XXXXX */

#define	temp1	misc0
	PUSH_Q(misc0)
	/* allocate teh data object */
	MOV_Q(temp, temp1		/* temp1 :=  descriptor */
	shlq	CONST(TAG_SHIFTW),temp1
	orq	CONST(MAKE_TAG(DTAG_raw32)),temp1
	MOV_Q(temp1, REGIND(allocptr)	/* store descriptor */
	addq	CONST(4), allocptr	/* allocptr++ */
	MOV_Q(allocptr, temp1		/* temp1 := data object */
	shlq	CONST(2), temp		/* temp := length in bytes */
	addq	temp, allocptr		/* allocptr += length */

	/* allocate the header object */
	MOV_Q(CONST(DESC_word8arr), REGIND(allocptr))/* header descriptor */
	addq	CONST(4),allocptr	/* allocptr++ */
	MOV_Q(temp1, REGIND(allocptr))	/* header data field */
	MOV_Q(stdarg, REGOFF(4,allocptr))	/* header length field */
	MOV_Q(allocptr, stdarg)		/* stdarg := header object */
	addq	CONST(8),allocptr	/* allocptr := 2 */
	POP_Q(misc0)
	CONTINUE
#undef  temp1
2:
	MOV_L(CONST(REQ_ALLOC_BYTEARRAY), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))


/* create_s : int -> string */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT
	MOV_Q(stdarg,temp)
	sarq	CONST(1),temp		/* temp := length(untagged) */
	addq	CONST(8),temp		
	sarq	CONST(3),temp		/* temp := length(words) */
	cmpq	CONST(SMALL_OBJ_SZW),temp
	JGE(2f)

	pushq	misc0			/* free misc0 */
#define	temp1	misc0

	MOV_Q(temp, temp1)
	shlq	CONST(TAG_SHIFTW),temp1	/* build descriptor in temp1 */
	orq	CONST(MAKE_TAG(DTAG_raw32)), temp1
	MOV_Q(temp1, REGIND(allocptr)/* store the data pointer */
	addq	CONST(8),allocptr	/* allocptr++ */

	MOV_Q(allocptr, temp1		/* temp1 := data object */
	shlq	CONST(3),temp		/* temp := length in bytes */
	addq	temp, allocptr		/* allocptr += length */
	MOV_Q(CONST(0),REGOFF(-8,allocptr)	/* zero out the last word */

	/* allocate the header object */
	MOV_Q(CONST(DESC_string), temp	/* header descriptor */
	MOV_Q(temp, REGIND(allocptr))
	addq	CONST(8), allocptr	/* allocptr++ */
	MOV_Q(temp1, REGIND(allocptr)/* header data field */
	MOV_Q(stdarg, REGOFF(8,allocptr)	/* header length field */
	MOV_Q(allocptr, stdarg		/* stdarg := header object */
	addq	CONST(16), allocptr		
	
	popq	misc0			/* restore misc0 */
#undef  temp1
	CONTINUE
2:
	MOV_Q(CONST(REQ_ALLOC_STRING), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT
	PUSH_Q(misc0)
	PUSH_Q(misc1)
#define	temp1	misc0
#define temp2   misc1	
	MOV_Q(REGIND(stdarg),temp		/* temp := length(tagged) */
	MOV_Q(temp, temp1)
	sarq	CONST(1),temp1		/* temp1 := length(untagged) */
	cmpq	CONST(SMALL_OBJ_SZW),temp1
	JGE(3f)


	shlq	CONST(TAG_SHIFTW),temp1	/* build descriptor in temp1 */
	orq	CONST(MAKE_TAG(DTAG_vec_data)),temp1
	MOV_Q(temp1,REGIND(allocptr)	/* store descriptor */
	addq	CONST(8),allocptr	/* allocptr++ */
	MOV_Q(REGOFF(8,stdarg),temp1		/* temp1 := list */
	MOV_Q(allocptr,stdarg		/* stdarg := vector */

2:
	MOV_Q(REGIND(temp1),temp2		/* temp2 := hd(temp1) */
	MOV_Q(temp2, REGIND(allocptr)	/* store word in vector */
	addq	CONST(8), allocptr	/* allocptr++ */
	MOV_Q(REGOFF(8,temp1),temp1		/* temp1 := tl(temp1) */
	cmpq	CONST(ML_nil),temp1	/* temp1 = nil? */
	JNE(2b)

	/* allocate header object */
	MOV_Q(CONST(DESC_polyvec),temp1/* descriptor in temp1 */
	MOV_Q(temp1, REGIND(allocptr)	/* store descriptor */
	addq	CONST(8),allocptr	/* allocptr++ */
	MOV_Q(stdarg, REGIND(allocptr)	/* header data field */
	MOV_Q(temp, REGOFF(8,allocptr)	/* header length */
	MOV_Q(allocptr, stdarg		/* result = header object */
	addq	CONST(16),allocptr	/* allocptr += 2 */

	POP_Q(misc1)
	POP_Q(misc0)
	CONTINUE
3:
	POP_Q(misc1)
	POP_Q(misc0)
	MOV_Q(CONST(REQ_ALLOC_VECTOR), request_w)
	MOVE(stdlink, temp, pc)
	JMP(CSYM(set_request))
#undef  temp1
#undef  temp2	
	
/* try_lock: spin_lock -> bool. 
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.	For now, we only provide a uni-processor trivial version.
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV_Q(REGIND(stdarg), temp	/* Get old value of lock. */
	MOV_L(CONST(1), REGIND(stdarg))	/* Set the lock to ML_false. */
	MOV_Q(temp, stdarg		/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV_L(CONST(3), REGIND(stdarg))		/* Store ML_true into lock. */
	MOV_Q(CONST(1), stdarg		/* Return unit. */
	CONTINUE
#endif


/********************* Floating point functions. *********************/

#define FPOP	fstp %st	/* Pop the floating point register stack. */


/* Temporary storage for the old and new floating point control
   word.  We don't use the stack to for this, since doing so would 
   change the offsets of the pseudo-registers. */
	DATA
	.align 8
old_controlwd:	
	.word	0
new_controlwd:	
	.word	0
	TEXT
	.align 8

/* FIXME: replace the following with <fenv.h> function calls! */
/*
 * Initialize the 80387 floating point coprocessor.  First, the floating
 * point control word is initialized (undefined fields are left
 * unchanged).	Rounding control is set to "nearest" (although floor_a
 * needs "toward negative infinity").  Precision control is set to
 * "double".  The precision, underflow, denormal 
 * overflow, zero divide, and invalid operation exceptions
 * are masked.  Next, seven of the eight available entries on the
 * floating point register stack are claimed (see x86/x86.sml).
 *
 * NB: this cannot trash any registers because it's called from request_fault.
 */
ENTRY(FPEEnable)
	FINIT
	subq	CONST(8), RSP	/* Temp space.	Keep stack aligned. */
	FSTCW(REGIND(RSP))	/* Store FP control word. */
				/* Keep undefined fields, clear others. */
	AND_W(CONST(0xf0c0), REGIND(RSP))
	OR_W(CONST(0x023f), REGIND(RSP)) /* Set fields (see above). */
	FLDCW(REGIND(RSP))	/* Install new control word. */
	addq	CONST(8), RSP
	RET

#if (defined(OPSYS_LINUX) || defined(OPSYS_CYGWIN) || defined(OPSYS_SOLARIS))
ENTRY(fegetround)
	SUB_L(CONST(4), ESP)	/* allocate temporary space */
	FSTCW(REGIND(ESP))	/* store fp control word */
	SAR_L(CONST(10),REGIND(ESP))/* rounding mode is at bit 10 and 11 */
	AND_L(CONST(3), REGIND(ESP))/* mask two bits */
	MOV_L(REGIND(ESP),EAX)	/* return rounding mode */
	ADD_L(CONST(4), ESP)	/* deallocate space */	
	RET
  	
ENTRY(fesetround)
	SUB_L(CONST(4), ESP)	/* allocate temporary space */	
	FSTCW(REGIND(ESP))	/* store fp control word */
	AND_W(CONST(0xf3ff), REGIND(ESP))	/* Clear rounding field. */
	MOV_L(REGOFF(8,ESP), EAX)	/* new rounding mode */
	SAL_L(CONST(10), EAX)	/* move to right place */
	OR_L(EAX,REGIND(ESP))	/* new control word */
	FLDCW(REGIND(ESP))	/* load new control word */
	ADD_L(CONST(4), ESP)	/* deallocate space */
	RET
#endif


/* floor : real -> int
   Return the nearest integer that is less or equal to the argument.
	 Caller's responsibility to make sure arg is in range. */

ML_CODE_HDR(floor_a)
	FSTCW(old_controlwd)		/* Get FP control word. */
	MOV_W(old_controlwd, AX)
	AND_W(CONST(0xf3ff), AX)	/* Clear rounding field. */
	OR_W(CONST(0x0400), AX)	/* Round towards neg. infinity. */
	MOV_W(AX, new_controlwd)
	FLDCW(new_controlwd)		/* Install new control word. */

	FLD_D(REGIND(stdarg))
	sub	$8,RSP
	fistpq	REGIND(RSP)			/* Round, store, and pop. */
	POP_Q(stdarg)
	salq	CONST(1), stdarg		/* Tag the resulting integer. */
	incq	stdarg

	FLDCW(old_controlwd)		/* Restore old FP control word. */
	CONTINUE

/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 * Note: Using fxtract, and fistl does not work for inf's and nan's.
 */
ML_CODE_HDR(logb_a)
	MOV_Q(REGOFF(0,stdarg),temp		/* msb for little endian arch */
	sarq	CONST(52), temp		/* throw out 20 bits */
	andq	CONST(0x7ff),temp	/* clear all but 11 low bits */
	subq	CONST(1023), temp	/* unbias */
	salq	CONST(1), temp		/* room for tag bit */
	addq	CONST(1), temp		/* tag bit */
	MOV_Q(temp, stdarg
	CONTINUE
	

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */

ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	sub	$8,RSP
	movsd	%xmm0,(RSP) /* do these need save */
	sub	$8,RSP
	movsd	%xmm1,(RSP)

	sarq	$1,REGOFF(8,stdarg)			 /* untag */
	salq	$52,REGOFF(8,stdarg) /* XXX not tested */ /* should put scalar in exponent */
	MOV_Q($0x3ff0000000000000,temp
	addq	REGOFF(8,stdarg),temp
	PUSH_Q(temp
)
	movsd	REGIND(RSP),%xmm1			/* Load it ... */
/*	fstp	%st(1) */		/* ... into 1st FP reg. */
	MOV_Q(REGIND(stdarg), temp		/* Get pointer to real. */
	movsd	/* was FLD_D */ REGIND(temp),%xmm0			/* Load it into temp. */

	mulsd	%xmm0,%xmm1				/* Multiply exponent by scalar. */
	MOV_Q(CONST(DESC_reald), REGIND(allocptr)
	movsd /* FSTP_D */ %xmm0,REGOFF(8,allocptr)		/* Store resulting float. */
	addq	CONST(8), allocptr	/* Allocate word for tag. */
	MOV_Q(allocptr, stdarg		/* Return a pointer to the float. */
	addq	CONST(8), allocptr	/* Allocate room for float. */
	addq	CONST(8),RSP		/* discard copy of scalar */
	movsd	(RSP),%xmm1 /* do these need save */
	add	$8,RSP
	movsd	(RSP),%xmm0 /* do these need save */
	add	$8,RSP
	CONTINUE

/* end of AMD64.prim.asm */
