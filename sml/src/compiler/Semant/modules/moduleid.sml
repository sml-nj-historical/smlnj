(* Copyright 1996 by AT&T Bell Laboratories *)
(* moduleid.sml *)

signature MODULE_ID =
sig
  type stamp  (* = Stamps.stamp *)
  datatype modId
    = STRid of {rlzn: stamp, sign: stamp} 
    | SIGid of stamp                    
    | FCTid of {rlzn: stamp, sign: modId}
    | FSIGid of {paramsig: stamp, bodysig: stamp}   
    | TYCid of stamp
    | EENVid of stamp

  val equalId : modId * modId -> bool 
  val cmp: modId * modId -> order
  val idToString : modId -> string

end (* signature MODULE_ID *)

structure ModuleId : MODULE_ID =
struct

  type stamp = Stamps.stamp

  datatype modId
    = STRid of {rlzn: stamp, sign: stamp} 
    | SIGid of stamp                    
    | FCTid of {rlzn: stamp, sign: modId}
    | FSIGid of {paramsig: stamp, bodysig: stamp}   
    | TYCid of stamp
    | EENVid of stamp

  (* 
   * This is still unnecessarily expensive; since one has to
   * build a closure for "f" first before calling &. We can
   * always use "if-then-else" to implement the short-circut 
   * for a three-value logic, though. (ZHONG)
   *)
  fun & (GREATER,f) = GREATER
    | & (EQUAL,f) = f()
    | & (LESS,f) = LESS

  infix &

  fun stampsCmp(a::r1,b::r2) = Stamps.cmp(a,b) & (fn () => stampsCmp(r1,r2))
    | stampsCmp(nil,nil) = EQUAL
    | stampsCmp(_,nil) = GREATER
    | stampsCmp(nil,_) = LESS

  val rec cmp =
   fn (STRid{rlzn=a1,sign=b1},
       STRid{rlzn=a2,sign=b2}) =>
        Stamps.cmp(a1,a2) &(fn()=> Stamps.cmp(b1,b2))
    | (STRid _, _) => GREATER
    | (_, STRid _) => LESS
    | (SIGid a, SIGid b) => Stamps.cmp(a,b)
    | (SIGid _, _) => GREATER
    | (_, SIGid _) => LESS
    | (FCTid{rlzn=a1,sign=b1},
       FCTid{rlzn=a2,sign=b2}) => Stamps.cmp(a1,a2) & (fn()=>cmp(b1,b2))
    | (FCTid _, _) => GREATER
    | (_, FCTid _) => LESS
    | (FSIGid{paramsig=a1,bodysig=b1},
       FSIGid{paramsig=a2,bodysig=b2}) => 
        Stamps.cmp(a1,a2) & (fn()=>Stamps.cmp(b1,b2))
    | (FSIGid _, _) => GREATER
    | (_, FSIGid _) => LESS
    | (TYCid a, TYCid b) => Stamps.cmp(a,b)
    | (TYCid _, _) => GREATER
    | (_, TYCid _) => LESS
    | (EENVid a, EENVid b) => Stamps.cmp(a,b)

  fun equalId (a,b) = cmp(a,b)=EQUAL

  val stampToString = Stamps.stampToShortString

  fun idToString(STRid{rlzn,sign}) =
        String.concat["STRid{rlzn=", stampToString rlzn, ",sign=",
	 stampToString sign,"}"]
    | idToString(SIGid st) = String.concat["SIGid(",stampToString st,")"]
    | idToString(FCTid{rlzn,sign}) =
        String.concat["FCTid{rlzn=",stampToString rlzn,",sign=",
	 idToString sign,"}"]
    | idToString(FSIGid{paramsig,bodysig}) =
        String.concat["FSIGid{paramsig=",stampToString paramsig,",bodysig=",
         stampToString bodysig,"}"]
    | idToString(TYCid stamp) =
	String.concat["TYCid(", stampToString stamp,")"]
    | idToString(EENVid st) = String.concat["EENVid(",stampToString st,")"]

end (* structure ModuleId *)

(*
 * $Log$
 *)
