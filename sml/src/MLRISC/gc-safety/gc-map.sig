signature GC_MAP =
sig

   structure GC : GC_TYPE

   exception GCMap

   type gcmap = GC.gctype IntHashTable.hash_table

   val GCMAP       : gcmap Annotations.property

   val GCLIVEIN    : (int * GC.gctype) list Annotations.property 

   val GCLIVEOUT   : (int * GC.gctype) list Annotations.property 

   val toString    : gcmap -> (int -> string)

end
