
functor MLTreeMult'(type instr) (val addv   : unit  -> instr list)   =
struct
 
end


signature PPCINSTR' =
sig
    type instr 
end

functor PPC'(PPCInstr : PPCINSTR') = 
struct
  functor Multiply32 = MLTreeMult'(PPCInstr)
 
end

