(* bug603.sml *)
(* 603. System.Symbol.makestring gives eroneous answer *)

let open Compiler.Symbol 
    val makestring = symbolToString
in 
if (makestring(varSymbol "s")^makestring(tycSymbol "s")^
    makestring(sigSymbol "s")^makestring(strSymbol "s")^
    makestring(fctSymbol "s")^makestring(fixSymbol "s")) 
   = "VAL$sTYC$sSIG$sSTR$sFCT$sFIX$s"
then "System.Symbol.makestring seems O.K." 
else "Verify the behaviour of System.Symbol.makestring"
handle _ => "System.Symbol.makestring has a major bug"
end

