/* X86.prim.asm
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This was derived from I386.prim.s, by Mark Leone (mleone@cs.cmu.edu)
 *
 * Heavily modified and changed to use assyntax.h, by Lal George.
 */

#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "ml-limits.h"

/* enable/disable virtual (memory-based) registers.
 * the number of vregs, etc. must concur with:
 *	src/runtime/include/ml-state.h
 *	src/runtime/kernel/ml-state.c
 *      src/sml-nj/x86/x86.sml
 */

/* XXX:	 May no longer be needed */
#ifndef VREGS
#  define VREGS
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
#define temp		%eax
#define misc0		%ebx
#define misc1		%ecx
#define misc2		%edx
#define stdcont		%esi
#define stdarg		%ebp
#define allocptr	%edi
#define stackptr        %esp	

/* other reg uses */
#define creturn 	%eax

	/* Stack frame */
#define tempmem		0(%esp)
#define baseptr		4(%esp)
#define exncont		8(%esp)
#define limitptr	12(%esp)
#define pc		16(%esp)
#define unused_1	20(%esp)
#define storeptr	24(%esp)
#define varptr		28(%esp)
#define start_gc	32(%esp)
#define unused_2	36(%esp)
#define eaxSpill	40(%esp) /* eax=0 */
#define	ecxSpill	44(%esp) /* ecx=1 */
#define	edxSpill	48(%esp) /* edx=2 */
#define	ebxSpill	52(%esp) /* ebx=3 */
#define	espSpill	56(%esp) /* esp=4 */
#define	ebpSpill	60(%esp) /* ebp=5 */
#define	esiSpill	64(%esp) /* esi=6 */
#define	ediSpill	68(%esp) /* edi=7 */
#define stdlink		72(%esp)
#define	stdclos		76(%esp)

#define ML_STATE_OFFSET 176
#define mlstate_ptr	ML_STATE_OFFSET(%esp)
#define freg8           184	     /* double word aligned */ 
#define	freg9           192
#define freg31          368          /* 152 + (31-8)*8 */
#define	fpTempMem	376	     /* freg31 + 8 */
#define SpillAreaStart	512	     /* starting offset */	
#define ML_FRAME_SIZE	(8192)

#define	via

	DATA
	ALIGN4
request_w:		/* place to put the request code */
	.long 0
	GLOBAL(ML_X86Frame)
LABEL(CSYM(ML_X86Frame)) /* ptr to the ml frame (gives C access to limitptr) */
	.long 0		

SavedSP:
	.long 0		/* Value of stack pointer to restore */


#include "mlstate-offsets.h"	/** this file is generated **/


/*
 * 386 function call conventions:  
 *  [true for gcc and dynix3 cc; untested for others]
 *
 * 	Caller save registers: eax, ecx, edx
 * 	Callee save registers: ebx, esi, edi, and ebp. 
 * 	Floating point state is caller-save.
 * 	Arguments passed on stack.  Rightmost argument pushed first.
 * 	Word-sized result returned in %eax.
 */

#define cresult	%eax

#define CALLEE_SAVE	\
	pushl	%ebx;	\
	pushl	%esi;	\
	pushl	%edi;	\
	pushl	%ebp	

#define CALLEE_RESTORE	\
	popl	%ebp;	\
	popl	%edi;	\
	popl	%esi;	\
	popl	%ebx 

/* MOVE copies one memory location to another, using a specified temporary. */

#define MOVE(src,tmp,dest)	\
	movl	src, tmp;	\
	movl	tmp, dest

#define CONTINUE				\
	jmp	via stdcont

#define CHECKLIMIT				\
 1:;						\
	MOVE(stdlink, temp, pc)	;		\
	cmpl	limitptr, allocptr;		\
	jb	9f;				\
	call	via CSYM(saveregs);		\
	jmp	via 1b;				\
 9:

/**********************************************************************/
	TEXT
	ALIGN4

ML_CODE_HDR(sigh_return_a)
	movl	IMMED(ML_unit),stdlink
	movl	IMMED(ML_unit),stdclos
	movl	IMMED(ML_unit),pc
	movl	IMMED(REQ_SIG_RETURN), request_w
	jmp	CSYM(set_request)

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */

ENTRY(sigh_resume)
	movl	IMMED(REQ_SIG_RESUME), request_w
	movl	IMMED(ML_unit),stdlink
	movl	IMMED(ML_unit),stdclos
	movl	IMMED(ML_unit),pc
	jmp	CSYM(set_request)

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	movl	IMMED(REQ_POLL_RETURN), request_w
	movl	IMMED(ML_unit),stdlink
	movl	IMMED(ML_unit),stdclos
	movl	IMMED(ML_unit),pc
	jmp	CSYM(set_request)

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	movl	IMMED(REQ_POLL_RESUME), request_w
	movl	IMMED(ML_unit),stdlink
	movl	IMMED(ML_unit),stdclos
	movl	IMMED(ML_unit),pc
	jmp	CSYM(set_request)

ML_CODE_HDR(handle_a)
	movl	IMMED(REQ_EXN), request_w
	MOVE	(stdlink,temp,pc)
	jmp	CSYM(set_request)

ML_CODE_HDR(return_a)
	movl	IMMED(REQ_RETURN), request_w
	movl	IMMED(ML_unit),stdlink
	movl	IMMED(ML_unit),stdclos
	movl	IMMED(ML_unit),pc
	jmp	CSYM(set_request)

/* Request a fault.  The floating point coprocessor must be reset
 * (thus trashing the FP registers) since we don't know whether a 
 * value has been pushed into the temporary "register".	 This is OK 
 * because no floating point registers will be live at the start of 
 * the exception handler.
 */
ENTRY(request_fault)
	call    CSYM(FPEEnable)          /* Doesn't trash any general regs. */
	movl	IMMED(REQ_FAULT), request_w
	MOVE	(stdlink,temp,pc)
	jmp	CSYM(set_request)

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT
	movl	IMMED(REQ_BIND_CFUN), request_w
	jmp	CSYM(set_request)

ML_CODE_HDR(build_literals_a)
	CHECKLIMIT
	movl	IMMED(REQ_BUILD_LITERALS), request_w
	jmp	CSYM(set_request)

ML_CODE_HDR(callc_a)
	CHECKLIMIT
	movl	IMMED(REQ_CALLC), request_w
	jmp	CSYM(set_request)

ENTRY(saveregs)
	popl	pc
	movl	IMMED(REQ_GC), request_w
	/* fall into set_request */

ENTRY(set_request)
	/* temp holds mlstate_ptr, valid request in request_w  */
	/* Save registers */
	movl	mlstate_ptr, temp
	movl	allocptr,AllocPtrOffMSP(temp)
	movl	stdarg,StdArgOffMSP(temp)
	movl	stdcont,StdContOffMSP(temp)

#define	temp2 allocptr
	/* note that we have left ML code */
	movl	VProcOffMSP(temp),temp2
	movl	IMMED(0), InMLOffVSP(temp2)	

	movl	misc0, Misc0OffMSP(temp)
	movl	misc1, Misc1OffMSP(temp)
	movl	misc2, Misc2OffMSP(temp)

	/* Save vregs before the stack frame is popped. */
	MOVE(limitptr,temp2, LimitPtrOffMSP(temp))
	MOVE(exncont, temp2, ExnPtrOffMSP(temp)) 
	MOVE(stdclos, temp2, StdClosOffMSP(temp))
	MOVE(stdlink, temp2, LinkRegOffMSP(temp))
	MOVE(pc, temp2, PCOffMSP(temp))
	MOVE(storeptr,temp2, StorePtrOffMSP(temp))
	MOVE(varptr,  temp2, VarPtrOffMSP(temp))
#undef	temp2	
	
	/* return val of function is request code */
	movl	request_w,creturn

	/* Pop the stack frame and return to run_ml(). */
	movl	SavedSP, %esp	
	CALLEE_RESTORE
	ret

	TEXT
	ALIGN4
ENTRY(restoreregs)
	movl	4(%esp), temp		/* Get argument (MLState ptr). */
	CALLEE_SAVE

	movl	%esp, SavedSP		/* save stack pointer */

	/* Align on 8 byte boundary. Assumes that the stack
	 * starts out being at least word aligned. But who knows ...
	 */
	orl	IMMED(4), %esp		
	subl	IMMED(4), %esp		/* stack grows from high to low */
	
#define temp2	%ebx
	/* Allocate and initialize the ML stack frame. */
	subl	IMMED(ML_FRAME_SIZE), %esp
	MOVE	(ExnPtrOffMSP(temp),  temp2, exncont)
	MOVE	(LimitPtrOffMSP(temp), temp2, limitptr)
	MOVE	(StorePtrOffMSP(temp), temp2, storeptr)
	MOVE	(VarPtrOffMSP(temp),   temp2, varptr)
	lea	CSYM(saveregs), temp2
	movl	temp2,start_gc
	movl	temp, mlstate_ptr

	/* vregs */
	MOVE	(LinkRegOffMSP(temp),  temp2, stdlink)
	MOVE	(StdClosOffMSP(temp),  temp2, stdclos)
#undef	temp2

	/* Load ML registers. */
	movl	AllocPtrOffMSP(temp), allocptr
	movl	StdContOffMSP(temp), stdcont
	movl	StdArgOffMSP(temp), stdarg
	movl	Misc0OffMSP(temp), misc0
	movl	Misc1OffMSP(temp), misc1
	movl	Misc2OffMSP(temp), misc2

	movl	%esp,CSYM(ML_X86Frame)	/* frame ptr for signal handler. */

	pushl	misc2			/* free up a register   */
	pushl	temp			/* save msp temporarily */

#define	tmpreg	misc2

	/* note that we're entering ML */
	movl	VProcOffMSP(temp),temp  /* temp is now vsp */
#define vsp	temp
	movl	IMMED(1),InMLOffVSP(vsp)

	/* handle signals */
	movl	NPendingSysOffVSP(vsp),tmpreg
	addl	NPendingOffVSP(vsp),tmpreg
	cmpl	IMMED(0),tmpreg
#undef  tmpreg
	jne	pending

restore_and_jmp_ml:
	popl	temp			/* restore temp to msp */
	popl	misc2
	
jmp_ml:
	cmpl	limitptr, allocptr
	jmpl	PCOffMSP(temp)		      /* Jump to ML code. */


pending:
	cmpl	IMMED(0),InSigHandlerOffVSP(vsp)   /* Currently handling signal? */
	jne	restore_and_jmp_ml
	movl	IMMED(1),HandlerPendingOffVSP(vsp) /* handler trap is now pending */

	/* must restore here because limitptr is on stack */ /* XXX */
	popl	temp			/* restore temp to msp */
	popl	misc2

	movl	allocptr,limitptr
	jmp	jmp_ml			/* Jump to ML code. */
#undef  vsp

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT
	movl 	0(stdarg),temp               /* temp := length in words */
	sarl	IMMED(1),temp		     /* temp := length untagged */
	cmpl	IMMED(SMALL_OBJ_SZW),temp    /* is this a small object */
	jge	3f

#define temp1 misc0
#define temp2 misc1
	pushl	misc0			     /* save misc0 */ 
	pushl	misc1			     /* save misc1 */
	
	movl	temp, temp1
	sall	IMMED(TAG_SHIFTW),temp1      /* build descriptor in temp1 */
	orl	IMMED(MAKE_TAG(DTAG_arr_data)),temp1
	movl	temp1,0(allocptr)	     /* store descriptor */
	addl	IMMED(4),allocptr	     /* allocptr++ */
	movl	allocptr, temp1		     /* temp1 := array data ptr */
	movl	4(stdarg), temp2	     /* temp2 := initial value */ 
2:	
	movl	temp2,0(allocptr)	     /* initialize array */
	addl	IMMED(4), allocptr
	subl	IMMED(1), temp
	jne	2b

	/* Allocate array header */
	movl	IMMED(DESC_polyarr),0(allocptr) /* descriptor in temp */
	addl	IMMED(4), allocptr	     /* allocptr++ */
	movl	0(stdarg), temp		     /* temp := length */
	movl	allocptr, stdarg	     /* result = header addr */ 
	movl	temp1, 0(allocptr)	     /* store pointer to data */
	movl	temp, 4(allocptr)	     /* store length */
	addl	IMMED(8), allocptr

	popl	misc1
	popl	misc0	
	CONTINUE
#undef  temp1
#undef  temp2
3:
	movl	IMMED(REQ_ALLOC_ARRAY), request_w
	MOVE	(stdlink, temp, pc)
	jmp	CSYM(set_request)
	

/* create_r : int -> realarray */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT
#define temp1 misc0
        push	misc0			/* free temp1 */
	movl 	stdarg,temp		/* temp := length */
	sarl	IMMED(1),temp		/* temp := untagged length */
	shll	IMMED(1),temp		/* temp := length in words */
	cmpl	IMMED(SMALL_OBJ_SZW),temp
	jge	2f

	orl	IMMED(4),allocptr	/* align allocptr */

	/* allocate the data object */
	movl	temp, temp1
	shll	IMMED(TAG_SHIFTW),temp1 /* temp1 := descriptor */
	orl	IMMED(MAKE_TAG(DTAG_raw64)),temp1
	movl	temp1,0(allocptr)	/* store descriptor */
	addl	IMMED(4), allocptr	/* allocptr++ */
	movl	allocptr, temp1		/* temp1 := data object */
	shll	IMMED(2),temp		/* temp := length in bytes */
	addl	temp, allocptr		/* allocptr += length */

	/* allocate the header object */
	movl	IMMED(DESC_real64arr),0(allocptr)/* header descriptor */
	addl	IMMED(4), allocptr	/* allocptr++ */
	movl	temp1, 0(allocptr)	/* header data field */
	movl	stdarg, 4(allocptr)	/* header length field */
	movl	allocptr, stdarg		/* stdarg := header object */
	addl	IMMED(8), allocptr	/* allocptr += 2 */

	popl	misc0			/* restore temp1 */
	CONTINUE

2:
	popl	misc0			/* restore temp1 */
	movl	IMMED(REQ_ALLOC_REALDARRAY), request_w
	MOVE	(stdlink, temp, pc)
	jmp	CSYM(set_request)
#undef temp1


/* create_b : int -> bytearray */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT
	movl 	stdarg,temp		/* temp := length(tagged int) */
	sarl	IMMED(1),temp		/* temp := length(untagged) */
	addl	IMMED(3),temp
	sarl	IMMED(2),temp		/* temp := length(words) */
	cmpl	IMMED(SMALL_OBJ_SZW),temp /* small object? */
	jmp	2f
	jge	2f

#define	temp1	misc0
	pushl	misc0

	/* allocate teh data object */
	movl	temp, temp1		/* temp1 :=  descriptor */
	shll	IMMED(TAG_SHIFTW),temp1
	orl	IMMED(MAKE_TAG(DTAG_raw32)),temp1
	movl	temp1, 0(allocptr)	/* store descriptor */
	addl	IMMED(4), allocptr	/* allocptr++ */
	movl	allocptr, temp1		/* temp1 := data object */
	shll	IMMED(2), temp		/* temp := length in bytes */
	addl	temp, allocptr		/* allocptr += length */

	/* allocate the header object */
	movl	IMMED(DESC_word8arr), 0(allocptr)/* header descriptor */
	addl	IMMED(4),allocptr	/* allocptr++ */
	movl	temp1, 0(allocptr)	/* header data field */
	movl	stdarg,4(allocptr)	/* header length field */
	movl	allocptr, stdarg	/* stdarg := header object */
	addl	IMMED(8),allocptr	/* allocptr := 2 */
	popl	misc0
	CONTINUE
#undef  temp1
2:
	movl	IMMED(REQ_ALLOC_BYTEARRAY), request_w
	MOVE	(stdlink, temp, pc)
	jmp	CSYM(set_request)


/* create_s : int -> string */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT
	movl 	stdarg,temp
	sarl	IMMED(1),temp		/* temp := length(untagged) */
	addl	IMMED(4),temp		
	sarl	IMMED(2),temp		/* temp := length(words) */
	cmpl	IMMED(SMALL_OBJ_SZW),temp
	jge	2f

	pushl	misc0			/* free misc0 */
#define	temp1	misc0

	movl	temp, temp1
	shll	IMMED(TAG_SHIFTW),temp1	/* build descriptor in temp1 */
	orl	IMMED(MAKE_TAG(DTAG_raw32)), temp1
	movl	temp1, 0(allocptr)	/* store the data pointer */
	addl	IMMED(4),allocptr	/* allocptr++ */

	movl	allocptr, temp1		/* temp1 := data object */
	shll	IMMED(2),temp		/* temp := length in bytes */
	addl	temp, allocptr		/* allocptr += length */
	movl	IMMED(0),-4(allocptr)	/* zero out the last word */

	/* allocate the header object */
	movl	IMMED(DESC_string), temp	/* header descriptor */
	movl	temp, 0(allocptr)
	addl	IMMED(4), allocptr	/* allocptr++ */
	movl	temp1, 0(allocptr)	/* header data field */
	movl	stdarg, 4(allocptr)	/* header length field */
	movl	allocptr, stdarg	/* stdarg := header object */
	addl	IMMED(8), allocptr		
	
	popl	misc0			/* restore misc0 */
#undef  temp1
	CONTINUE
2:
	movl	IMMED(REQ_ALLOC_STRING), request_w
	MOVE	(stdlink, temp, pc)
	jmp	CSYM(set_request)

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT
	pushl	misc0
	pushl	misc1
#define	temp1	misc0
#define temp2   misc1	
	movl 	0(stdarg),temp		/* temp := length(tagged) */
	movl	temp, temp1
	sarl	IMMED(1),temp1		/* temp1 := length(untagged) */
	cmpl	IMMED(SMALL_OBJ_SZW),temp1
	jge	3f


	shll	IMMED(TAG_SHIFTW),temp1	/* build descriptor in temp1 */
	orl	IMMED(MAKE_TAG(DTAG_vec_data)),temp1
	movl	temp1,0(allocptr)	/* store descriptor */
	addl	IMMED(4),allocptr	/* allocptr++ */
	movl	4(stdarg),temp1		/* temp1 := list */
	movl	allocptr,stdarg		/* stdarg := vector */

2:
	movl	0(temp1),temp2		/* temp2 := hd(temp1) */
	movl	temp2, 0(allocptr)	/* store word in vector */
	addl	IMMED(4), allocptr	/* allocptr++ */
	movl	4(temp1),temp1		/* temp1 := tl(temp1) */
	cmpl	IMMED(ML_nil),temp1	/* temp1 = nil? */
	jne	2b

	/* allocate header object */
	movl	IMMED(DESC_polyvec),temp1/* descriptor in temp1 */
	movl	temp1, 0(allocptr)	/* store descriptor */
	addl	IMMED(4),allocptr	/* allocptr++ */
	movl	stdarg, 0(allocptr)	/* header data field */
	movl	temp, 4(allocptr)	/* header length */
	movl	allocptr, stdarg	/* result = header object */
	addl	IMMED(8),allocptr	/* allocptr += 2 */

	popl	misc1
	popl	misc0
	CONTINUE
3:
	popl	misc1
	popl	misc0
	movl	IMMED(REQ_ALLOC_VECTOR), request_w
	MOVE	(stdlink, temp, pc)
	jmp	CSYM(set_request)
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
	movl	(stdarg), temp		/* Get old value of lock. */
	movl	IMMED(1), (stdarg)	/* Set the lock to ML_false. */
	movl	temp, stdarg		/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock 
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	movl	IMMED(3), (stdarg)		/* Store ML_true into lock. */
	movl	IMMED(1), stdarg		/* Return unit. */
	CONTINUE
#endif


/********************* Floating point functions. *********************/

#define FPOP	fstp %st	/* Pop the floating point register stack. */


/* Temporary storage for the old and new floating point control
   word.  We don't use the stack to for this, since doing so would 
   change the offsets of the pseudo-registers. */
	DATA
	ALIGN4
old_controlwd:	
	.word	0
new_controlwd:	
	.word	0
	TEXT
	ALIGN4

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
	finit
	subl	IMMED(4), %esp	/* Temp space.	Keep stack aligned. */
	fstcw	(%esp)		/* Store FP control word. */
	andw	IMMED(0xf0c0), (%esp)	/* Keep undefined fields, clear others. */
	orw	IMMED(0x023f), (%esp)	/* Set fields (see above). */
	fldcw	(%esp)		/* Install new control word. */
	addl	IMMED(4), %esp
	ret

#if (defined(OPSYS_LINUX) || defined(OPSYS_SOLARIS))
ENTRY(fegetround)
	subl	IMMED(4), %esp	/* allocate temporary space */
	fstcw	(%esp)		/* store fp control word */
	sarl	IMMED(10),(%esp)/* rounding mode is at bit 10 and 11 */
	andl	IMMED(3), (%esp)/* mask two bits */
	movl    (%esp),%eax	/* return rounding mode */
	addl    IMMED(4), %esp	/* deallocate space */	
	ret
  	
ENTRY(fesetround)
	subl	IMMED(4), %esp	/* allocate temporary space */	
	fstcw	(%esp)		/* store fp control word */
	andw	IMMED(0xf3ff), (%esp)	/* Clear rounding field. */
	movl    8(%esp), %eax	/* new rounding mode */
	sall	IMMED(10), %eax	/* move to right place */
	orl     %eax,(%esp)	/* new control word */
	fldcw	(%esp)		/* load new control word */
	addl	IMMED(4), %esp	/* deallocate space */
	ret
#endif

/* Save the state of the floating point unit. */
ENTRY(savefpregs)
	movl	4(%esp), temp		/* Get pointer argument. */
	fsave	(temp)
	ret

/* Restore the state of the floating point unit. */
ENTRY(restorefpregs)
	movl	4(%esp), temp		/* Arg is an ML string. */
	frstor	(temp)
	ret

/* floor : real -> int
   Return the nearest integer that is less or equal to the argument.
	 Caller's responsibility to make sure arg is in range. */

ML_CODE_HDR(floor_a)
	fstcw	old_controlwd		/* Get FP control word. */
	movw	old_controlwd, %ax
	andw	IMMED(0xf3ff), %ax	/* Clear rounding field. */
	orw	IMMED(0x0400), %ax	/* Round towards neg. infinity. */
	movw	%ax, new_controlwd
	fldcw	new_controlwd		/* Install new control word. */

	fldl	(stdarg)		/* Load argument. */
	subl	IMMED(4), %esp
	fistpl	(%esp)			/* Round, store, and pop. */
	popl	stdarg
	sall	IMMED(1), stdarg		/* Tag the resulting integer. */
	incl	stdarg

	fldcw	old_controlwd		/* Restore old FP control word. */
	CONTINUE

/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 * Note: Using fxtract, and fistl does not work for inf's and nan's.
 */
ML_CODE_HDR(logb_a)
	movl    4(stdarg),temp		/* msb for little endian arch */
	sarl	IMMED(20), temp		/* throw out 20 bits */
	andl    IMMED(0x7ff),temp	/* clear all but 11 low bits */
	subl	IMMED(1023), temp	/* unbias */
	sall    IMMED(1), temp		/* room for tag bit */
	addl	IMMED(1), temp		/* tag bit */
	movl	temp, stdarg
	CONTINUE
	

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */

ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	pushl	4(stdarg)		/* Get copy of scalar. */
	sarl	IMMED(1), (%esp)	/* Untag it. */
	fildl	(%esp)			/* Load it ... */
/*	fstp	%st(1) */		/* ... into 1st FP reg. */
	movl	(stdarg), temp		/* Get pointer to real. */
	fldl	(temp)			/* Load it into temp. */

	fscale				/* Multiply exponent by scalar. */
	movl	IMMED(DESC_reald), (allocptr)
	fstpl	4(allocptr)		/* Store resulting float. */
	addl	IMMED(4), allocptr	/* Allocate word for tag. */
	movl	allocptr, stdarg	/* Return a pointer to the float. */
	addl	IMMED(8), allocptr	/* Allocate room for float. */
	fstpl   (%esp)			
	addl	IMMED(4), %esp		/* Discard copy of scalar. */

	CONTINUE

/* end of X86.prim.asm */
