functor GCMap(GC : GC_TYPE) : GC_MAP =
struct

   structure GC = GC
   exception GCMap
   type gcmap = GC.gctype Intmap.intmap

   val GCMAP       = Annotations.new NONE : gcmap Annotations.property
   val GCSAFEPOINT = Annotations.newFlag ""

   fun toString gcmap =
   let val lookup = Intmap.map gcmap
       fun f r = "{"^GC.toString(lookup r)^"}" handle _ => "{?}"
   in  f end

end
