(*
 * This makes a new cell module that automatically propagate gc type info.
 *)
functor GCCells(structure C  : CELLS
                structure GC : GC_TYPE) : GC_CELLS =
struct

   structure C  = C
   structure GC = GC

   val gcmap = ref NONE : GC.gcmap option ref

   fun setGCMap map = gcmap := SOME map
   fun getGCMap() = Option.valOf(!gcmap) 

   (*
    * Generate a new virtual register and update the gc map at the same time.
    *)
   fun newCell k = 
   let val new = C.newCell k
       val gcmap = getGCMap()
       val add  = Intmap.add gcmap
       fun genVar gc =
       let val r = new()
       in  add(r,gc); r end
   in  genVar
   end

   (*
    * Create a new GC map
    *)
   fun newGCMap() =
   let val gcmap = Intmap.new(129,GC.GCTYPE)
   in  case C.zeroReg C.GP of
         SOME r => Intmap.add gcmap (r,GC.CONST 0)
       | _ => ();
       gcmap
   end

end
