structure SMLObjType =
struct

   type objtype = CPS.cty 
  
   (* Copied from PPCps *)
   fun toString(CPS.INTt) =  "[I]"
     | toString(CPS.INT32t) = "[I32]"
     | toString(CPS.FLTt) = "[R]"
     | toString(CPS.PTRt (CPS.RPT k)) = "[PR"^(Int.toString(k))^"]"
     | toString(CPS.PTRt (CPS.FPT k)) = "[PF"^(Int.toString(k))^"]"
     | toString(CPS.PTRt (CPS.VPT)) = "[PV]"
     | toString(CPS.FUNt) = "[F]"
     | toString(CPS.CNTt) = "[C]"
     | toString(CPS.DSPt) = "[D]"

end

structure SMLGCType = GCType(SMLObjType)
