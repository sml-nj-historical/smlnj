/*! \file assyntax64.h
 *
 * \author John Reppy
 *
 * Macros to support the choice between different assembly syntaxes for
 * the AMD64 (aka x86-64) architecture.  This file is derived from
 * "assyntax.h" file for 32-bit x86, but has been heavily redacted to
 * cover just those instructions used in AMD64.prim.asm.
 *
 * TODO: add support for MASM syntax
 */

/*
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef __ASSYNTAX64_H__
#define __ASSYNTAX64_H__

/* either ATT_ASSEMBLER or GNU_ASSEMBLER should be specified */
#if !(defined(ATT_ASSEMBLER) || defined(GNU_ASSEMBLER))
#  error must specify assembler syntax (either ATT_ASSEMBLER or GNU_ASSEMBLER)
#endif

#if defined(__STDC__)
#define CONCAT(x, y)	x ## y
#else
#define CONCAT(x, y)	x/**/y
#endif

/* 32-bit registers */
#define EAX		%eax
#define EBX		%ebx
#define ECX		%ecx
#define EDX		%edx
#define EBP		%ebp
#define ESI		%esi
#define EDI		%edi
#define ESP		%esp
/* 64-bit registers */
#define RDI		%rdi
#define RSI		%rsi
#define RSP		%rsp
#define RAX		%rax
#define RBX		%rbx
#define RCX		%rcx
#define RDX		%rdx
#define RBP		%rbp
#define R8		%r8
#define R9		%r9
#define R10		%r10
#define R11		%r11
#define R12		%r12
#define R13		%r13
#define R14		%r14
#define R15		%r15
/* 128-bit SSE Registers */
#define XMM0		%xmm0
#define XMM1		%xmm1
#define XMM2		%xmm2
#define XMM3		%xmm3
#define XMM4		%xmm4
#define XMM5		%xmm5
#define XMM6		%xmm6
#define XMM7		%xmm7
#define XMM8		%xmm8
#define XMM9		%xmm9
#define XMM10		%xmm10
#define XMM11		%xmm11
#define XMM12		%xmm12
#define XMM13		%xmm13
#define XMM14		%xmm14
#define XMM15		%xmm15

#ifdef GNU_ASSEMBLER
/*
 * GNU ASSEMBLER SYNTAX
 * *********************
 */
#define CHOICE(a,b)	b
/* AT&T src, dst order for operands */
#define ARG2(src, dst)	src,dst	

#define ADDR_TOGGLE	aword
#define OPSZ_TOGGLE	word

#else
/*
 * AT&T ASSEMBLER SYNTAX
 * *********************
 */
#define CHOICE(a,b)	a
/* AT&T src, dst order for operands */
#define ARG2(src, dst)	src,dst	

#define ADDR_TOGGLE	addr16
#define OPSZ_TOGGLE	data16

#endif /* GNU_ASSEMBLER */

#if defined(GLOBALS_HAVE_USCORE)
#define GLNAME(a)	CONCAT(_,a)
#else
#define GLNAME(a)	a
#endif


	/****************************************/
	/*					*/
	/*	Select the various choices	*/
	/*					*/
	/****************************************/


/* Redefine assembler directives */
/*********************************/
#define GLOBL		CHOICE(.globl, .globl)
#define GLOBAL		GLOBL
#define EXTERN		GLOBL
/*
#define ALIGNTEXT32	CHOICE(.align 32, .align ARG2(5,0x90), .align 32)
*/
#define ALIGNTEXT64	CHOICE(.align 64, .balign 64)
#define ALIGNTEXT32	CHOICE(.align 32, .balign 32)
#define ALIGNTEXT16	CHOICE(.align 16, .balign 16)
#define ALIGNTEXT8	CHOICE(.align 8, .balign 8)
#define ALIGNTEXT4	CHOICE(.align 4, .balign 4)
#define ALIGNTEXT2	CHOICE(.align 2, .balign 2)
/* ALIGNTEXT4ifNOP is the same as ALIGNTEXT4, but only if the space is
 * guaranteed to be filled with NOPs.  Otherwise it does nothing.
 */
#define ALIGNTEXT32ifNOP	CHOICE(.align 32, .balign ARG2(32,0x90))
#define ALIGNTEXT16ifNOP	CHOICE(.align 16, .balign ARG2(16,0x90))
#define ALIGNTEXT8ifNOP	CHOICE(.align 8, .balign ARG2(8,0x90))
#define ALIGNTEXT4ifNOP	CHOICE(.align 4, .balign ARG2(4,0x90))
#define ALIGNDATA32	CHOICE(.align 32, .balign ARG2(32,0x0))
#define ALIGNDATA16	CHOICE(.align 16, .balign ARG2(16,0x0))
#define ALIGNDATA8	CHOICE(.align 8, .balign ARG2(8,0x0))
#define ALIGNDATA4	CHOICE(.align 4, .balign ARG2(4,0x0))
#define ALIGNDATA2	CHOICE(.align 2, .balign ARG2(2,0x0))
#define FILE(s)		CHOICE(.file s, .file s)
#define STRING(s)	CHOICE(.string s, .asciz s)
#define D_LONG		CHOICE(.long, .long)
#define D_WORD		CHOICE(.value, .short)
#define D_BYTE		CHOICE(.byte, .byte)
#define SPACE		CHOICE(.comm, .space)
#define COMM		CHOICE(.comm, .comm)
#define SEG_DATA	CHOICE(.data, .data)
#define SEG_TEXT	CHOICE(.text, .text)
#define SEG_BSS		CHOICE(.bss, .bss)

#ifdef GNU_ASSEMBLER
#define D_SPACE(n)	. = . + n
#else
#define D_SPACE(n)	.space n
#endif

/* Addressing Modes */
/* Immediate Mode */
#define ADDR(a)		CHOICE(CONCAT($,a), CONCAT($,a))
#define CONST(a)	CHOICE(CONCAT($,a), CONCAT($,a))

/* Indirect Mode */
#define CONTENT(a)	CHOICE(a, a)	 	/* take contents of variable */
#define REGIND(a)	CHOICE((a), (a))	/* Register a indirect */
/* Register b indirect plus displacement a */
#define REGOFF(a, b)	CHOICE(a(b), a(b))
/* Reg indirect Base + Index + Displacement  - this is mainly for 16-bit mode
 * which has no scaling
 */
#define REGBID(b,i,d)	CHOICE(d(b,i), d(b,i))
/* Reg indirect Base + (Index * Scale) */
#define REGBIS(b,i,s)	CHOICE((b,i,s), (b,i,s))
/* Reg indirect Base + (Index * Scale) + Displacement */
#define REGBISD(b,i,s,d) CHOICE(d(b,i,s), d(b,i,s))
/* Displaced Scaled Index: */
#define REGDIS(d,i,s)	CHOICE(d(,i,s), d(,i,s))
/* Indexed Base: */
#define REGBI(b,i)	CHOICE((b,i), (b,i))
/* Displaced Base: */
#define REGDB(d,b)	CHOICE(d(b), d(b))
/* Variable indirect: */
#define VARINDIRECT(var) CHOICE(*var, *var)
/* Use register contents as jump/call target: */
#define CODEPTR(reg)	CHOICE(*reg, *reg)

#define ARG3(a,b,c)	a,b,c

/* the integer and control operations used in AMD64.prim.s */
#define ADD_Q(a, b)	addq ARG2(a,b)
#define AND_Q(a, b)	andq ARG2(a,b)
#define CALL(a)		call a
#define CMP_Q(a, b)	cmpq ARG2(a,b)
#define JB(a)		jb a
#define JGE(a)		jge a
#define JMP(a)		jmp a
#define LEA_Q(a, b)	leaq ARG2(a,b)
#define MOV_L(a, b)	movl ARG2(a,b)
#define MOV_Q(a, b)	movq ARG2(a,b)
#define OR_Q(a,b)	orq ARG2(a,b)
#define POP_Q(a)	popq a
#define PUSH_Q(a)	pushq a
#define SAL_Q(a, b)	salq ARG2(a,b)
#define SAR_Q(a, b)	sarq ARG2(a,b)
#define SHL_Q(a, b)	shlq ARG2(a,b)
#define SUB_Q(a, b)	subq ARG2(a,b)


/* Floating Point Instructions */
#define F2XM1		CHOICE(f2xm1, f2xm1, f2xm1)
#define FABS		CHOICE(fabs, fabs, fabs)
#define FADD_D(a)	CHOICE(faddl a, faddl a, faddd a)
#define FADD_S(a)	CHOICE(fadds a, fadds a, fadds a)
#define FADD2(a, b)	CHOICE(fadd ARG2(a,b), fadd ARG2(a,b), fadd ARG2(b,a))
#define FADDP(a, b)	CHOICE(faddp ARG2(a,b), faddp ARG2(a,b), faddp ARG2(b,a))
#define FIADD_L(a)	CHOICE(fiaddl a, fiaddl a, fiaddl a)
#define FIADD_W(a)	CHOICE(fiadd a, fiadds a, fiadds a)
#define FBLD(a)		CHOICE(fbld a, fbld a, fbld a)
#define FBSTP(a)	CHOICE(fbstp a, fbstp a, fbstp a)
#define FCHS		CHOICE(fchs, fchs, fchs)
#define FCLEX		CHOICE(fclex, wait; fnclex, wait; fclex)
#define FNCLEX		CHOICE(fnclex, fnclex, fclex)
#define FCOM(a)		CHOICE(fcom a, fcom a, fcom a)
#define FCOM_D(a)	CHOICE(fcoml a, fcoml a, fcomd a)
#define FCOM_S(a)	CHOICE(fcoms a, fcoms a, fcoms a)
#define FCOMP(a)	CHOICE(fcomp a, fcomp a, fcomp a)
#define FCOMP_D(a)	CHOICE(fcompl a, fcompl a, fcompd a)
#define FCOMP_S(a)	CHOICE(fcomps a, fcomps a, fcomps a)
#define FCOMPP		CHOICE(fcompp, fcompp, fcompp)
#define FCOS		CHOICE(fcos, fcos, fcos)
#define FDECSTP		CHOICE(fdecstp, fdecstp, fdecstp)
#define FDIV_D(a)	CHOICE(fdivl a, fdivl a, fdivd a)
#define FDIV_S(a)	CHOICE(fdivs a, fdivs a, fdivs a)
#define FDIV2(a, b)	CHOICE(fdiv ARG2(a,b), fdiv ARG2(a,b), fdiv ARG2(b,a))
#define FDIVP(a, b)	CHOICE(fdivp ARG2(a,b), fdivp ARG2(a,b), fdivp ARG2(b,a))
#define FIDIV_L(a)	CHOICE(fidivl a, fidivl a, fidivl a)
#define FIDIV_W(a)	CHOICE(fidiv a, fidivs a, fidivs a)
#define FDIVR_D(a)	CHOICE(fdivrl a, fdivrl a, fdivrd a)
#define FDIVR_S(a)	CHOICE(fdivrs a, fdivrs a, fdivrs a)
#define FDIVR2(a, b)	CHOICE(fdivr ARG2(a,b), fdivr ARG2(a,b), fdivr ARG2(b,a))
#define FDIVRP(a, b)	CHOICE(fdivrp ARG2(a,b), fdivrp ARG2(a,b), fdivrp ARG2(b,a))
#define FIDIVR_L(a)	CHOICE(fidivrl a, fidivrl a, fidivrl a)
#define FIDIVR_W(a)	CHOICE(fidivr a, fidivrs a, fidivrs a)
#define FFREE(a)	CHOICE(ffree a, ffree a, ffree a)
#define FICOM_L(a)	CHOICE(ficoml a, ficoml a, ficoml a)
#define FICOM_W(a)	CHOICE(ficom a, ficoms a, ficoms a)
#define FICOMP_L(a)	CHOICE(ficompl a, ficompl a, ficompl a)
#define FICOMP_W(a)	CHOICE(ficomp a, ficomps a, ficomps a)
#define FILD_Q(a)	CHOICE(fildll a, fildq a, fildq a)
#define FILD_L(a)	CHOICE(fildl a, fildl a, fildl a)
#define FILD_W(a)	CHOICE(fild a, filds a, filds a)
#define FINCSTP		CHOICE(fincstp, fincstp, fincstp)
#define FINIT		CHOICE(finit, wait; fninit, wait; finit)
#define FNINIT		CHOICE(fninit, fninit, finit)
#define FIST_L(a)	CHOICE(fistl a, fistl a, fistl a)
#define FIST_W(a)	CHOICE(fist a, fists a, fists a)
#define FISTP_Q(a)	CHOICE(fistpll a, fistpq a, fistpq a)
#define FISTP_L(a)	CHOICE(fistpl a, fistpl a, fistpl a)
#define FISTP_W(a)	CHOICE(fistp a, fistps a, fistps a)
#define FLD_X(a)	CHOICE(fldt a, fldt a, fldx a) /* 80 bit data type! */
#define FLD_D(a)	CHOICE(fldl a, fldl a, fldd a)
#define FLD_S(a)	CHOICE(flds a, flds a, flds a)
#define FLD1		CHOICE(fld1, fld1, fld1)
#define FLDL2T		CHOICE(fldl2t, fldl2t, fldl2t)
#define FLDL2E		CHOICE(fldl2e, fldl2e, fldl2e)
#define FLDPI		CHOICE(fldpi, fldpi, fldpi)
#define FLDLG2		CHOICE(fldlg2, fldlg2, fldlg2)
#define FLDLN2		CHOICE(fldln2, fldln2, fldln2)
#define FLDZ		CHOICE(fldz, fldz, fldz)
#define FLDCW(a)	CHOICE(fldcw a, fldcw a, fldcw a)
#define FLDENV(a)	CHOICE(fldenv a, fldenv a, fldenv a)
#define FMUL_S(a)	CHOICE(fmuls a, fmuls a, fmuls a)
#define FMUL_D(a)	CHOICE(fmull a, fmull a, fmuld a)
#define FMUL2(a, b)	CHOICE(fmul ARG2(a,b), fmul ARG2(a,b), fmul ARG2(b,a))
#define FMULP(a, b)	CHOICE(fmulp ARG2(a,b), fmulp ARG2(a,b), fmulp ARG2(b,a))
#define FIMUL_L(a)	CHOICE(fimull a, fimull a, fimull a)
#define FIMUL_W(a)	CHOICE(fimul a, fimuls a, fimuls a)
#define FNOP		CHOICE(fnop, fnop, fnop)
#define FPATAN		CHOICE(fpatan, fpatan, fpatan)
#define FPREM		CHOICE(fprem, fprem, fprem)
#define FPREM1		CHOICE(fprem1, fprem1, fprem1)
#define FPTAN		CHOICE(fptan, fptan, fptan)
#define FRNDINT		CHOICE(frndint, frndint, frndint)
#define FRSTOR(a)	CHOICE(frstor a, frstor a, frstor a)
#define FSAVE(a)	CHOICE(fsave a, wait; fnsave a, wait; fsave a)
#define FNSAVE(a)	CHOICE(fnsave a, fnsave a, fsave a)
#define FSCALE		CHOICE(fscale, fscale, fscale)
#define FSIN		CHOICE(fsin, fsin, fsin)
#define FSINCOS		CHOICE(fsincos, fsincos, fsincos)
#define FSQRT		CHOICE(fsqrt, fsqrt, fsqrt)
#define FST_D(a)	CHOICE(fstl a, fstl a, fstd a)
#define FST_S(a)	CHOICE(fsts a, fsts a, fsts a)
#define FSTP_X(a)	CHOICE(fstpt a, fstpt a, fstpx a)
#define FSTP_D(a)	CHOICE(fstpl a, fstpl a, fstpd a)
#define FSTP_S(a)	CHOICE(fstps a, fstps a, fstps a)
#define FSTP(a)		CHOICE(fstp a, fstp a, fstp a)
#define FSTCW(a)	CHOICE(fstcw a, wait; fnstcw a, wait; fstcw a)
#define FNSTCW(a)	CHOICE(fnstcw a, fnstcw a, fstcw a)
#define FSTENV(a)	CHOICE(fstenv a, wait; fnstenv a, fstenv a)
#define FNSTENV(a)	CHOICE(fnstenv a, fnstenv a, fstenv a)
#define FSTSW(a)	CHOICE(fstsw a, wait; fnstsw a, wait; fstsw a)
#define FNSTSW(a)	CHOICE(fnstsw a, fnstsw a, fstsw a)
#define FSUB_S(a)	CHOICE(fsubs a, fsubs a, fsubs a)
#define FSUB_D(a)	CHOICE(fsubl a, fsubl a, fsubd a)
#define FSUB2(a, b)	CHOICE(fsub ARG2(a,b), fsub ARG2(a,b), fsub ARG2(b,a))
#define FSUBP(a, b)	CHOICE(fsubp ARG2(a,b), fsubp ARG2(a,b), fsubp ARG2(b,a))
#define FISUB_L(a)	CHOICE(fisubl a, fisubl a, fisubl a)
#define FISUB_W(a)	CHOICE(fisub a, fisubs a, fisubs a)
#define FSUBR_S(a)	CHOICE(fsubrs a, fsubrs a, fsubrs a)
#define FSUBR_D(a)	CHOICE(fsubrl a, fsubrl a, fsubrd a)
#define FSUBR2(a, b)	CHOICE(fsubr ARG2(a,b), fsubr ARG2(a,b), fsubr ARG2(b,a))
#define FSUBRP(a, b)	CHOICE(fsubrp ARG2(a,b), fsubrp ARG2(a,b), fsubrp ARG2(b,a))
#define FISUBR_L(a)	CHOICE(fisubrl a, fisubrl a, fisubrl a)
#define FISUBR_W(a)	CHOICE(fisubr a, fisubrs a, fisubrs a)
#define FTST		CHOICE(ftst, ftst, ftst)
#define FUCOM(a)	CHOICE(fucom a, fucom a, fucom a)
#define FUCOMP(a)	CHOICE(fucomp a, fucomp a, fucomp a)
#define FUCOMPP		CHOICE(fucompp, fucompp, fucompp)
#define FWAIT		CHOICE(wait, wait, wait)
#define FXAM		CHOICE(fxam, fxam, fxam)
#define FXCH(a)		CHOICE(fxch a, fxch a, fxch a)
#define FXTRACT		CHOICE(fxtract, fxtract, fxtract)
#define FYL2X		CHOICE(fyl2x, fyl2x, fyl2x)
#define FYL2XP1		CHOICE(fyl2xp1, fyl2xp1, fyl2xp1)

#endif /* __ASSYNTAX_H__ */
