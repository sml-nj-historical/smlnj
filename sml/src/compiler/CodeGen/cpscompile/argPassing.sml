(* argPassing.sml --- parameter passing convention for standard
 *		and known functions.
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)
functor ArgPassing (structure C : CPSREGS
		    structure MS : MACH_SPEC) : ARG_PASSING = 
struct
  structure T : MLTREE = C.T

  fun error msg = ErrorMsg.impossible ("ArgPassing." ^ msg)

  val k = MS.numCalleeSaves
  val kf = MS.numFloatCalleeSaves

  fun stdlink(vfp) = T.GPR (C.stdlink(vfp))
  fun stdclos(vfp) = T.GPR (C.stdclos(vfp))
  fun stdarg(vfp)  = T.GPR (C.stdarg(vfp))
  fun stdcont(vfp) = T.GPR (C.stdcont(vfp))

  fun gpregs(vfp) = 
    stdlink(vfp)::stdclos(vfp)::stdarg(vfp)::stdcont(vfp)::map T.GPR C.miscregs
  val fpregs = map T.FPR (C.savedfpregs @ C.floatregs)

  fun fromto(i, j, regs) = let
    fun from(0, acc) = acc
      | from(n, x::xs) = from(n-1, xs)
    fun to(k, []) = []
      | to(k, r::rs) = if k > j then [] else r::to(k+1, rs)
  in 
    to(i, from(i,regs))
  end

  fun gprfromto(i, j, vfp) = fromto(i, j, gpregs(vfp))
  fun fprfromto(i, j, vfp) = fromto(i, j, fpregs)
  fun calleesaveregs(vfp) = gprfromto(4, k+3, vfp) @ fprfromto(0, kf-1, vfp)

  fun cut_head(n,l) = 
    if n = 0 then l
    else (case l of a::r => cut_head(n-1,r)
                  | _ => error "codegen cuthead 444")

  fun isFlt CPS.FLTt = true  | isFlt _ = false

  fun scan(t::z, gp, fp) =
      if isFlt t then (hd fp)::(scan(z,gp,tl fp)) 
      else (hd gp)::(scan(z,tl gp,fp))
    | scan([], _, _) = []

  fun standardEscape(vfp, args) = let
    val rest = cut_head(k+kf+3, args)
    val len = length(args)
    val gpr = stdarg(vfp) :: gprfromto(k+4, len, vfp)
    val fpr = fprfromto(kf,len, vfp)
  in stdlink(vfp)::stdclos(vfp)::stdcont(vfp)::calleesaveregs(vfp) @ scan(rest,gpr,fpr)
  end

  fun standardCont(vfp, args) = let
    val rest = if k > 0 then cut_head(k+kf+1,args) else cut_head(2,args)
    val len = length(args)
    val gpr = stdarg(vfp)::gprfromto(k+4, 1+len, vfp)
    val fpr = fprfromto(kf,len, vfp)
  in 
   if k > 0 then stdcont(vfp)::(calleesaveregs(vfp) @ scan(rest,gpr,fpr))
   else stdlink(vfp)::stdcont(vfp)::scan(rest,gpr,fpr)
  end
  
  fun standard{fnTy=CPS.CNTt, vfp, argTys} = standardCont(vfp, argTys)
    | standard{vfp, argTys, ...} = standardEscape(vfp, argTys)

  (* use an arbitary but fixed set of registers. *)
  fun fixed{vfp, argTys} = let
    fun iter(CPS.FLTt::rest, regs, f::fregs) = f::iter(rest, regs, fregs)
      | iter(_::rest, r::regs, fregs) = r::iter(rest, regs, fregs)
      | iter([], _, _) = []
  in iter(argTys, gpregs(vfp), fpregs)
  end
end

