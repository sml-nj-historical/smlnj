functor GCMap(GC : GC_TYPE) : GC_MAP =
struct

   structure GC = GC
   exception GCMap
   type gcmap = GC.gctype Intmap.intmap

   val GCMAP = Annotations.new(SOME(fn _ => "gcmap")) 
                 : gcmap Annotations.property

   fun toString gcmap =
   let val lookup = Intmap.map gcmap
       fun f r = "{"^GC.toString(lookup r)^"}" handle _ => "{?}"
   in  f end

end
