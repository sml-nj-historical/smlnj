(* argPassing.sml --- parameter passing convention for standard
 *		and known functions.
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)
functor ArgPassing (structure Cells : CELLS
		    structure C : CPSREGS
		    structure MS : MACH_SPEC) : ARG_PASSING = 
struct
  structure T : MLTREE = C.T

  fun error msg = ErrorMsg.impossible ("ArgPassing." ^ msg)

  val k = MS.numCalleeSaves
  val kf = MS.numFloatCalleeSaves

  val stdlink = T.GPR C.stdlink
  val stdclos = T.GPR C.stdclos
  val stdarg = T.GPR C.stdarg
  val stdcont = T.GPR C.stdcont

  val gpregs = stdlink::stdclos::stdarg::stdcont::map T.GPR C.miscregs
  val fpregs = map T.FPR (C.savedfpregs @ C.floatregs)

  fun fromto(i, j, regs) = let
    fun from(0, acc) = acc
      | from(n, x::xs) = from(n-1, xs)
    fun to(k, []) = []
      | to(k, r::rs) = if k > j then [] else r::to(k+1, rs)
  in 
    to(i, from(i,regs))
  end

  fun gprfromto(i, j) = fromto(i, j, gpregs)
  fun fprfromto(i, j) = fromto(i, j, fpregs)
  val calleesaveregs = gprfromto(4, k+3) @ fprfromto(0, kf-1)

  fun cut_head(n,l) = 
    if n = 0 then l
    else (case l of a::r => cut_head(n-1,r)
                  | _ => error "codegen cuthead 444")

  fun isFlt CPS.FLTt = true  | isFlt _ = false

  fun scan(t::z, gp, fp) =
      if isFlt t then (hd fp)::(scan(z,gp,tl fp)) 
      else (hd gp)::(scan(z,tl gp,fp))
    | scan([], _, _) = []

  fun standardEscape args = let
    val rest = cut_head(k+kf+3, args)
    val len = length(args)
    val gpr = stdarg :: gprfromto(k+4, len)
    val fpr = fprfromto(kf,len)
  in stdlink::stdclos::stdcont::calleesaveregs @ scan(rest,gpr,fpr)
  end

  fun standardCont args = let
    val rest = if k > 0 then cut_head(k+kf+1,args) else cut_head(2,args)
    val len = length(args)
    val gpr = stdarg::gprfromto(k+4, 1+len)
    val fpr = fprfromto(kf,len)
  in 
   if k > 0 then stdcont::(calleesaveregs @ scan(rest,gpr,fpr))
   else stdlink::stdcont::scan(rest,gpr,fpr)
  end
  
  fun standard(CPS.CNTt, tl) = standardCont tl 
    | standard(_, tl) = standardEscape tl

  (* known functions have parameters passed in fresh temporaries. *)
  fun known(CPS.FLTt::rest) = T.FPR(T.FREG(64,Cells.newFreg())):: known rest
    | known(_::rest) = T.GPR(T.REG(32,Cells.newReg())):: known rest
    | known [] = []

  (* use an arbitary but fixed set of registers. *)
  fun fixed ctys = let
    fun iter(CPS.FLTt::rest, regs, f::fregs) = f::iter(rest, regs, fregs)
      | iter(_::rest, r::regs, fregs) = r::iter(rest, regs, fregs)
      | iter([], _, _) = []
  in iter(ctys, gpregs, fpregs)
  end
end

