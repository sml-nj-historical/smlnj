signature GC_MAP =
sig

   structure GC : GC_TYPE

   exception GCMap

   type gcmap = GC.gctype Intmap.intmap

   val GCMAP       : gcmap Annotations.property

   val toString    : gcmap -> (int -> string)

end
