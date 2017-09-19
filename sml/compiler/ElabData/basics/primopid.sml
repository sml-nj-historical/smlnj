(* primopid.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* [dbm, 6/19/06]
     Folded ii.sml into this structure, eliminating exn hack.
     Changed name of pureInfo to isPrimCast.
     Eliminated redundant INL_PRIM, INL_STR, INL_NO. *)

structure PrimOpId : PRIMOPID = 
struct

  (* in the front end, primops are identified by a unique primop name,
     represented as a string. See the file DEVNOTES/Flint/primop-list
     for the catalog of primop names with their types and primop specs *)

  datatype primId = Prim of PrimopBinding.primop_bind | NonPrim

  datatype strPrimElem = PrimE of primId
                       | StrE of strPrimInfo

  withtype strPrimInfo = strPrimElem list

  fun bug s = ErrorMsg.impossible ("PrimOpId: " ^ s)

  (* isPrimop : primId -> bool *)
  fun isPrimop (Prim _) = true
    | isPrimop NonPrim  = false

  (* Used in TopLevel/main/compile.sml to identify callcc/capture primops *)
  fun isPrimCallcc (Prim p) =
      (case PrimopBinding.defnOf p
	 of (PrimOp.CALLCC | PrimOp.CAPTURE) => true
	  |  _ => false)
    | isPrimCallcc _ = false

  (* Used in ElabData/modules/moduleutil.sml to identify cast primop *)
  fun isPrimCast (Prim p) =
      (case PrimOp.defnOf p
	of PrimOp.CAST => true
	|  _ => false)
    | isPrimCast _ = false

  (* selStrPrimId : strPrimInfo * int -> strPrimInfo *)
  (* Select the prim ids for a substructure *)
  fun selStrPrimId([], slot) = []  (* not a bug? DBM *)
    | selStrPrimId(elems, slot) = 
      (case List.nth(elems, slot) 
	of StrE elems' => elems'
	 | PrimE _ => bug "PrimOpId.selStrPrimId: unexpected PrimE")
      handle Subscript => (bug "PrimOpId.selStrPrimId Subscript")
	(* This bug happens if we got a primid for a value 
	   component when we expected a strPrimElem for a 
	   structure *)

  (* Select the prim id for a value component *)
  fun selValPrimFromStrPrim([], slot) = NonPrim (* not a bug? DBM *)
    | selValPrimFromStrPrim(elems, slot) =
      (case List.nth(elems, slot)
	of PrimE(id) => id
	 | StrE _ => 
	   bug "PrimOpId.selValPrimFromStrPrim: unexpected StrE")
      handle Subscript => bug "PrimOpId.selValPrimFromStrPrim Subscript"
        (* This bug occurs if we got a substructure's
           strPrimElem instead of an expected value component's
           primId *)

  fun ppPrim NonPrim = "<NonPrim>"
    | ppPrim (Prim p) = ("<PrimE " ^ PrimOp.nameOf p ^">")

  fun ppStrInfo strelems = 
      let fun ppElem [] = ()
	    | ppElem ((PrimE p)::xs) = (print (ppPrim p); ppElem xs)
	    | ppElem ((StrE s)::xs) = (ppStrInfo s; ppElem xs)
      in (print "[ "; ppElem strelems; print " ]\n")
      end

end (* structure PrimOpId *)
