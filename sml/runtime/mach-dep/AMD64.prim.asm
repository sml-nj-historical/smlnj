/* X86.prim.asm
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This was derived from I386.prim.s, by Mark Leone (mleone@cs.cmu.edu)
 *
 * Completely rewritten and changed to use assyntax.h, by Lal George.
 */

#include "assyntax.h"
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
 * The 386 registers are used as follows:
 *
 * EAX - temp1 (see the code generator, x86/x86.sml)
 * EBX - misc0
 * ECX - misc1
 * EDX - misc2
 * ESI - standard continuation (ml_cont, see ml_state.h)
 * EBP - standard argument (ml_arg)
 * EDI - free space pointer (ml_allocptr)
 * ESP - stack pointer
 * EIP - program counter (ml_pc)
 */   

/* Registers (see x86/x86.sml): */
#define temp		RAX
#define misc0		RBX
#define misc1		RCX
#define misc2		RDX
#define stdcont		RSI
#define stdarg		RBP
#define allocptr	RDI
#define stackptr        RSP

/* other reg uses */
#define creturn 	RAX

	/* Stack frame */
#define tempmem		REGOFF(0,RSP)
#define baseptr		REGOFF(8,RSP)
#define exncont		REGOFF(16,RSP)
#define limitptr	REGOFF(24,RSP)
#define pc		REGOFF(32,RSP)
#define unused_1	REGOFF(40,RSP)
#define storeptr	REGOFF(48,RSP)
#define varptr		REGOFF(56,RSP)
#define start_gc	REGOFF(64,RSP)
#define unused_2	REGOFF(72,RSP)
#define eaxSpill	REGOFF(80,RSP) /* eax=0 */
#define	ecxSpill	REGOFF(88,RSP) /* ecx=1 */
#define	edxSpill	REGOFF(96,RSP) /* edx=2 */
#define	ebxSpill	REGOFF(104,RSP) /* ebx=3 */
#define	espSpill	REGOFF(112,RSP) /* esp=4 */
#define	ebpSpill	REGOFF(120,RSP) /* ebp=5 */
#define	esiSpill	REGOFF(128,RSP) /* esi=6 */
#define	ediSpill	REGOFF(136,RSP) /* edi=7 */
#define	r8Spill		REGOFF(144,RSP)
#define	r9Spill		REGOFF(152,RSP)
#define	r10Spill	REGOFF(160,RSP)
#define	r11Spill	REGOFF(168,RSP)
#define	r12Spill	REGOFF(176,RSP)
#define	r13Spill	REGOFF(184,RSP)
#define	r14Spill	REGOFF(192,RSP)
#define	r15Spill	REGOFF(200,RSP)
#define signBit		REGOFF(208,RSP)
#define negateSignBit	REGOFF(216,RSP)
/*
#define stdlink		REGOFF(144,RSP)
#define	stdclos		REGOFF(152,RSP)
*/
#define	stdlink		%r8
#define	stdclos		%r9

#define rspsave		REGOFF(504,RSP)

#define ML_STATE_OFFSET 176
#define mlstate_ptr	REGOFF(ML_STATE_OFFSET, RSP)
#define freg8           184	     /* double word aligned */ 
#define	freg9           192
#define freg31          368          /* 152 + (31-8)*8 */
#define	fpTempMem	376	     /* freg31 + 8 */
#define SpillAreaStart	512	     /* starting offset */	
#define ML_FRAME_SIZE	(8192)

#define	via

	SEG_DATA
	ALIGNDATA8
request_w:		/* place to put the request code */
	.quad 0
	GLOBL CSYM(ML_X86Frame)
LABEL(CSYM(ML_X86Frame)) /* ptr to the ml frame (gives C access to limitptr) */
	.quad 0		


#include "mlstate-offsets.h"	/** this file is generated **/


/*
 * 386 function call conventions:  
 *  [true for gcc and dynix3 cc; untested for others]
 *
 * 	Caller save registers: eax, ecx, edx
 * 	Callee save registers: ebx, esi, edi, and ebp.
 *	Save frame pointer (ebx) first to match standard function prelude
 * 	Floating point state is caller-save.
 * 	Arguments passed on stack.  Rightmost argument pushed first.
 * 	Word-sized result returned in %eax.
 *	On Darwin, stack frame must be multiple of 16 bytes
 */

#define cresult	EAX

#define CALLEE_SAVE_SZB 48	/* ebp, ebx, esi, edi */

#define CALLEE_SAVE	\
	pushq	RBX;	\
	pushq	RBP;	\
	pushq	%r10;	\
	pushq	%r13;	\
	pushq	%r14;	\
	pushq	%r15

#define CALLEE_RESTORE	\
	popq	%r15;	\
	popq	%r14;	\
	popq	%r13;	\
	popq	%r10;	\
	popq	RBP;	\
	popq	RBX

/* MOVE copies one memory location to another, using a specified temporary. */

#define MOVE(src,tmp,dest)	\
	movq	src, tmp;	\
	movq	tmp, dest

#define CONTINUE				\
	JMP(CODEPTR(stdcont))

#define CHECKLIMIT				\
 1:;						\
	MOVE(stdlink, temp, pc)	;		\
	cmpq	limitptr, allocptr;		\
	JB(9f);					\
	CALL(CSYM(saveregs));			\
	JMP(1b);				\
 9:

/**********************************************************************/
	SEG_TEXT
	ALIGNTEXT8

ML_CODE_HDR(sigh_return_a)
	movq	CONST(ML_unit),stdlink
	movq	CONST(ML_unit),stdclos
	movq	CONST(ML_unit),pc
	movq	CONST(REQ_SIG_RETURN), request_w
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
	movq	CONST(REQ_POLL_RETURN), request_w
	movq	CONST(ML_unit),stdlink
	movq	CONST(ML_unit),stdclos
	movq	CONST(ML_unit),pc
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
	movq	CONST(REQ_RETURN), request_w
	movq	CONST(ML_unit),stdlink
	movq	CONST(ML_unit),stdclos
	movq	CONST(ML_unit),pc
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
	popq	pc
	MOV_L(CONST(REQ_GC), request_w)
	/* fall into set_request */

ENTRY(set_request)
	/* temp holds mlstate_ptr, valid request in request_w  */
	/* Save registers */
	movq	mlstate_ptr, temp
	movq	allocptr, REGOFF(AllocPtrOffMSP,temp)
	movq	stdarg, REGOFF(StdArgOffMSP,temp)
	movq	stdcont, REGOFF(StdContOffMSP,temp)

#define	temp2 allocptr
	/* note that we have left ML code */
	movq	REGOFF(VProcOffMSP,temp), temp2
	movq	CONST(0), REGOFF(InMLOffVSP,temp2)

	movq	misc0, REGOFF(Misc0OffMSP,temp)
	movq	misc1, REGOFF(Misc1OffMSP,temp)
	movq	misc2, REGOFF(Misc2OffMSP,temp)

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
	movq	request_w,creturn

	/* Pop the stack frame and return to run_ml(). */
#if defined(OPSYS_DARWIN)
	LEA_L(REGOFF(ML_FRAME_SIZE+12,ESP),ESP)
#else
	movq	rspsave, RSP
#endif
	CALLEE_RESTORE
	RET

	SEG_TEXT
	ALIGNTEXT8
ENTRY(restoreregs)
	/* movq	REGOFF(4,RSP), temp */	/* Get argument (MLState ptr). */
	CALLEE_SAVE
	movq	%rdi, temp
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
	movq	%rsp,%rbx
	subq	CONST(ML_FRAME_SIZE),%rsp
	movq	%rbx,rspsave
#endif
	
#define temp2	RBX
      /* Initialize the ML stack frame. */
	MOVE(REGOFF(ExnPtrOffMSP, temp),  temp2, exncont)
	MOVE(REGOFF(LimitPtrOffMSP, temp), temp2, limitptr)
	MOVE(REGOFF(StorePtrOffMSP, temp), temp2, storeptr)
	MOVE(REGOFF(VarPtrOffMSP, temp),   temp2, varptr)
	leaq	CSYM(saveregs), temp2
	movq	temp2,start_gc
	movq	temp, mlstate_ptr

	movq	$0x8000000000000000, temp2
	movq	temp2, signBit
	movq	$0x7fffffffffffffff, temp2
	movq	temp2, negateSignBit

	/* vregs */
	MOVE	(REGOFF(LinkRegOffMSP,temp),  temp2, stdlink)
	MOVE	(REGOFF(StdClosOffMSP,temp),  temp2, stdclos)

	/* PC */
	MOVE    (REGOFF(PCOffMSP,temp), temp2, pc)
#undef	temp2

	/* Load ML registers. */
	movq	REGOFF(AllocPtrOffMSP,temp), allocptr
	movq	REGOFF(StdContOffMSP,temp), stdcont
	movq	REGOFF(StdArgOffMSP,temp), stdarg
	movq	REGOFF(Misc0OffMSP,temp), misc0
	movq	REGOFF(Misc1OffMSP,temp), misc1
	movq	REGOFF(Misc2OffMSP,temp), misc2

	movq	RSP,CSYM(ML_X86Frame)	/* frame ptr for signal handler. */

	pushq	misc2			/* free up a register   */
	pushq	temp			/* save msp temporarily */

#define	tmpreg	misc2

	/* note that we are entering ML */
	movq	REGOFF(VProcOffMSP,temp),temp  /* temp is now vsp */
#define vsp	temp
	movl	CONST(1),REGOFF(InMLOffVSP,vsp)

	/* handle signals */
	movl	REGOFF(SigsRecvOffVSP,vsp),%edx
	cmpl	REGOFF(SigsHandledOffVSP,vsp),%edx
	
#undef  tmpreg
	JNE(pending)

restore_and_jmp_ml:
	popq	temp			/* restore temp to msp */
	popq	misc2
	
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
	popq	misc2

	movq	allocptr,limitptr
	JMP(jmp_ml)			/* Jump to ML code. */
#undef  vsp

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT
	movq	REGIND(stdarg),temp         /* temp := length in words */
	sarq	CONST(1),temp		     /* temp := length untagged */
	cmpq	CONST(SMALL_OBJ_SZW),temp     /* is this a small object */
	JGE(3f)

#define temp1 misc0
#define temp2 misc1
	pushq	misc0			     /* save misc0 */ 
	pushq	misc1			     /* save misc1 */
	
	movq	temp, temp1
	salq	CONST(TAG_SHIFTW),temp1      /* build descriptor in temp1 */
	orq	CONST(MAKE_TAG(DTAG_arr_data)),temp1
	movq	temp1,REGIND(allocptr)	     /* store descriptor */
	addq	CONST(8),allocptr	     /* allocptr++ */
	movq	allocptr, temp1		     /* temp1 := array data ptr */
	movq	REGOFF(8,stdarg), temp2	     /* temp2 := initial value */ 
2:	
	movq	temp2, REGIND(allocptr)     /* initialize array */
	addq	CONST(8), allocptr
	subq	CONST(1), temp
	JNE(2b)

	/* Allocate array header */
	movq	CONST(DESC_polyarr),REGIND(allocptr) /* descriptor in temp */
	addq	CONST(8), allocptr	     /* allocptr++ */
	movq	REGIND(stdarg), temp	     /* temp := length */
	movq	allocptr, stdarg   	     /* result = header addr */ 
	movq	temp1, REGIND(allocptr)	     /* store pointer to data */
	movq	temp, REGOFF(8,allocptr)	     /* store length */
	addq	CONST(16), allocptr

	popq	misc1
	popq	misc0
	CONTINUE
#undef  temp1
#undef  temp2
3:
	movq	CONST(REQ_ALLOC_ARRAY),request_w
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))
	

/* create_r : int -> realarray */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT
#define temp1 misc0
        pushq	misc0			/* free temp1 */
	movq	stdarg,temp		/* temp := length */
	sarq	CONST(1),temp		/* temp := untagged length */
	cmpq	CONST(SMALL_OBJ_SZW),temp
	JGE(2f)

	/* allocate the data object */
	movq	temp, temp1
	shlq	CONST(TAG_SHIFTW),temp1  /* temp1 := descriptor */
	orq	CONST(MAKE_TAG(DTAG_raw64)),temp1
	movq	temp1,REGIND(allocptr)	/* store descriptor */
	addq	CONST(8), allocptr	/* allocptr++ */
	movq	allocptr, temp1		/* temp1 := data object */
	shlq	CONST(3),temp		/* temp := length in bytes */
	addq	temp, allocptr		/* allocptr += length */

	/* allocate the header object */
	movq	CONST(DESC_real64arr),REGIND(allocptr)/* header descriptor */
	addq	CONST(8), allocptr	/* allocptr++ */
	movq	temp1, REGIND(allocptr)	/* header data field */
	movq	stdarg, REGOFF(8,allocptr)	/* header length field */
	movq	allocptr, stdarg		/* stdarg := header object */
	addq	CONST(16), allocptr	/* allocptr += 2 */

	popq	misc0			/* restore temp1 */
	CONTINUE
2:
	popq	misc0			/* restore temp1 */
	movq	CONST(REQ_ALLOC_REALDARRAY), request_w
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))
#undef temp1


/* create_b : int -> bytearray */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT
	movq	stdarg,temp		/* temp := length(tagged int) */
	sarq	CONST(1),temp		/* temp := length(untagged) */
	addq	CONST(3),temp
	sarq	CONST(2),temp		/* temp := length(words) */
	cmpq	CONST(SMALL_OBJ_SZW),temp /* small object? */
	JMP(2f)
	JGE(2f)				/* XXXXX */

#define	temp1	misc0
	pushq	misc0

	/* allocate teh data object */
	movq	temp, temp1		/* temp1 :=  descriptor */
	shlq	CONST(TAG_SHIFTW),temp1
	orq	CONST(MAKE_TAG(DTAG_raw32)),temp1
	movq	temp1, REGIND(allocptr)	/* store descriptor */
	addq	CONST(4), allocptr	/* allocptr++ */
	movq	allocptr, temp1		/* temp1 := data object */
	shlq	CONST(2), temp		/* temp := length in bytes */
	addq	temp, allocptr		/* allocptr += length */

	/* allocate the header object */
	MOV_L(CONST(DESC_word8arr), REGIND(allocptr))/* header descriptor */
	addq	CONST(4),allocptr	/* allocptr++ */
	movq	temp1, REGIND(allocptr)	/* header data field */
	movq	stdarg, REGOFF(4,allocptr)	/* header length field */
	movq	allocptr, stdarg		/* stdarg := header object */
	addq	CONST(8),allocptr	/* allocptr := 2 */
	popq	misc0
	CONTINUE
#undef  temp1
2:
	MOV_L(CONST(REQ_ALLOC_BYTEARRAY), request_w)
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))


/* create_s : int -> string */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT
	movq	stdarg,temp
	sarq	CONST(1),temp		/* temp := length(untagged) */
	addq	CONST(8),temp		
	sarq	CONST(3),temp		/* temp := length(words) */
	cmpq	CONST(SMALL_OBJ_SZW),temp
	JGE(2f)

	pushq	misc0			/* free misc0 */
#define	temp1	misc0

	movq	temp, temp1
	shlq	CONST(TAG_SHIFTW),temp1	/* build descriptor in temp1 */
	orq	CONST(MAKE_TAG(DTAG_raw32)), temp1
	movq	temp1, REGIND(allocptr)/* store the data pointer */
	addq	CONST(8),allocptr	/* allocptr++ */

	movq	allocptr, temp1		/* temp1 := data object */
	shlq	CONST(3),temp		/* temp := length in bytes */
	addq	temp, allocptr		/* allocptr += length */
	movq	CONST(0),REGOFF(-8,allocptr)	/* zero out the last word */

	/* allocate the header object */
	movq	CONST(DESC_string), temp	/* header descriptor */
	movq	temp, REGIND(allocptr)
	addq	CONST(8), allocptr	/* allocptr++ */
	movq	temp1, REGIND(allocptr)/* header data field */
	movq	stdarg, REGOFF(8,allocptr)	/* header length field */
	movq	allocptr, stdarg		/* stdarg := header object */
	addq	CONST(16), allocptr		
	
	popq	misc0			/* restore misc0 */
#undef  temp1
	CONTINUE
2:
	movq	CONST(REQ_ALLOC_STRING), request_w
	MOVE	(stdlink, temp, pc)
	JMP(CSYM(set_request))

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT
	pushq	misc0
	pushq	misc1
#define	temp1	misc0
#define temp2   misc1	
	movq	REGIND(stdarg),temp		/* temp := length(tagged) */
	movq	temp, temp1
	sarq	CONST(1),temp1		/* temp1 := length(untagged) */
	cmpq	CONST(SMALL_OBJ_SZW),temp1
	JGE(3f)


	shlq	CONST(TAG_SHIFTW),temp1	/* build descriptor in temp1 */
	orq	CONST(MAKE_TAG(DTAG_vec_data)),temp1
	movq	temp1,REGIND(allocptr)	/* store descriptor */
	addq	CONST(8),allocptr	/* allocptr++ */
	movq	REGOFF(8,stdarg),temp1		/* temp1 := list */
	movq	allocptr,stdarg		/* stdarg := vector */

2:
	movq	REGIND(temp1),temp2		/* temp2 := hd(temp1) */
	movq	temp2, REGIND(allocptr)	/* store word in vector */
	addq	CONST(8), allocptr	/* allocptr++ */
	movq	REGOFF(8,temp1),temp1		/* temp1 := tl(temp1) */
	cmpq	CONST(ML_nil),temp1	/* temp1 = nil? */
	JNE(2b)

	/* allocate header object */
	movq	CONST(DESC_polyvec),temp1/* descriptor in temp1 */
	movq	temp1, REGIND(allocptr)	/* store descriptor */
	addq	CONST(8),allocptr	/* allocptr++ */
	movq	stdarg, REGIND(allocptr)	/* header data field */
	movq	temp, REGOFF(8,allocptr)	/* header length */
	movq	allocptr, stdarg		/* result = header object */
	addq	CONST(16),allocptr	/* allocptr += 2 */

	popq	misc1
	popq	misc0
	CONTINUE
3:
	popq	misc1
	popq	misc0
	movq	CONST(REQ_ALLOC_VECTOR), request_w
	MOVE	(stdlink, temp, pc)
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
	movq	REGIND(stdarg), temp	/* Get old value of lock. */
	MOV_L(CONST(1), REGIND(stdarg))	/* Set the lock to ML_false. */
	movq	temp, stdarg		/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV_L(CONST(3), REGIND(stdarg))		/* Store ML_true into lock. */
	movq	CONST(1), stdarg		/* Return unit. */
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
	sub	$8,%rsp
	fistpq	REGIND(RSP)			/* Round, store, and pop. */
	popq	stdarg
	salq	CONST(1), stdarg		/* Tag the resulting integer. */
	incq	stdarg

	FLDCW(old_controlwd)		/* Restore old FP control word. */
	CONTINUE

/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 * Note: Using fxtract, and fistl does not work for inf's and nan's.
 */
ML_CODE_HDR(logb_a)
	movq	REGOFF(0,stdarg),temp		/* msb for little endian arch */
	sarq	CONST(52), temp		/* throw out 20 bits */
	andq	CONST(0x7ff),temp	/* clear all but 11 low bits */
	subq	CONST(1023), temp	/* unbias */
	salq	CONST(1), temp		/* room for tag bit */
	addq	CONST(1), temp		/* tag bit */
	movq	temp, stdarg
	CONTINUE
	

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */

ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	sub	$8,%rsp
	movsd	%xmm0,(%rsp) /* do these need save */
	sub	$8,%rsp
	movsd	%xmm1,(%rsp)

	sarq	$1,REGOFF(8,stdarg)			 /* untag */
	salq	$52,REGOFF(8,stdarg) /* XXX not tested */ /* should put scalar in exponent */
	movq	$0x3ff0000000000000,temp
	addq	REGOFF(8,stdarg),temp
	pushq	temp

	movsd	REGIND(RSP),%xmm1			/* Load it ... */
/*	fstp	%st(1) */		/* ... into 1st FP reg. */
	movq	REGIND(stdarg), temp		/* Get pointer to real. */
	movsd	/* was FLD_D */ REGIND(temp),%xmm0			/* Load it into temp. */

	mulsd	%xmm0,%xmm1				/* Multiply exponent by scalar. */
	movq	CONST(DESC_reald), REGIND(allocptr)
	movsd /* FSTP_D */ %xmm0,REGOFF(8,allocptr)		/* Store resulting float. */
	addq	CONST(8), allocptr	/* Allocate word for tag. */
	movq	allocptr, stdarg		/* Return a pointer to the float. */
	addq	CONST(8), allocptr	/* Allocate room for float. */
	addq	CONST(8),%rsp		/* discard copy of scalar */
	movsd	(%rsp),%xmm1 /* do these need save */
	add	$8,%rsp
	movsd	(%rsp),%xmm0 /* do these need save */
	add	$8,%rsp
	CONTINUE

/* end of AMD64.prim.asm */
