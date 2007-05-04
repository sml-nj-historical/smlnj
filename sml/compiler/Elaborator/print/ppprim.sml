structure PPPrim =
struct

local 
  structure PP = PrettyPrintNew
  structure PU = PPUtilNew   
  open PPUtilNew
in

fun ppPrim ppstrm prim =
    let val pps = PU.pps ppstrm
    in (case prim
	  of PrimOpId.NonPrim => pps "<NonPrim>"
	   | PrimOpId.Prim(name) =>
             (pps "<PrimE "; pps name; pps ">"))
    end (* function ppPrim *)

fun ppStrPrimInfo ppstrm strPrimInfo =
    let val {openHOVBox, closeBox, pps, ...} = en_pp ppstrm
	fun ppStrPrimElem ppstrm (PrimOpId.PrimE p) = ppPrim ppstrm p
	  | ppStrPrimElem ppstrm (PrimOpId.StrE ps) = ppStrPrimInfo ppstrm ps
    in
	ppSequence ppstrm 
         {sep = fn ppstrm => (PP.string ppstrm ", ";
                              PP.break ppstrm {nsp=1, offset=0}),
          pr = (fn _ => fn elem => 
                           (openHOVBox 1; 
                            pps "(";
                            ppStrPrimElem ppstrm;
                            pps ")";
                            closeBox())),
          style = INCONSISTENT}
	 strPrimInfo
    end (* function ppStrPrimInfo *)

end (* local *)

end (* structure PPPrim *)
