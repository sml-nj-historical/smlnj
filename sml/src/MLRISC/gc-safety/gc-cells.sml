(*
 * This makes a new cell module that automatically propagate gc type info.
 *)
functor GCCells(structure C : CELLS
                structure GCMap : GC_MAP) : GC_CELLS =
struct

   structure C  = C
   structure GC = GCMap.GC
   structure GCMap = GCMap
  
   fun error msg = MLRiscErrorMsg.error("GCCells",msg)

   val gcmap = ref NONE : GCMap.gcmap option ref

   fun setGCMap map = gcmap := SOME map

   fun getGCMap() = 
        case !gcmap of
          NONE => error "no gc map"
        | SOME gcmap => gcmap

   fun clearGCMap() = gcmap := NONE

   (*
    * Generate a new virtual register and update the gc map at the same time.
    *)
   fun newCell k = 
   let val new = C.newCell k
       val gcmap = getGCMap()
       val add  = IntHashTable.insert gcmap
       fun genVar gc =
       let val r = new()
       in  add(r,gc); r end
   in  genVar
   end

   (*
    * Create a new GC map
    *)
   fun newGCMap() =
   let val gcmap = IntHashTable.mkTable(129,GCMap.GCMap)
   in  case C.zeroReg C.GP of
         SOME r => IntHashTable.insert gcmap (r,GC.CONST 0)
       | _ => ();
       gcmap
   end

end
