functor GCTypeMap
   (structure C : CELLS
    structure GC : GC_TYPE
   ) : GC_TYPEMAP =
struct

   structure C  = C
   structure GC = GC

   fun error msg = MLRiscErrorMsg.error("GCTypeMap",msg)

   (* Sorted by register *)
   type typemap = (C.cell * GC.gctype) list

   val empty = []

   fun fromList(l:typemap) = 
       ListMergeSort.uniqueSort (fn ((r1,_),(r2,_)) => Int.compare(r1,r2)) l

   fun ==(a,b) = 
   let fun loop([]:typemap,[]:typemap) = true
         | loop((r1,gc1)::a,(r2,gc2)::b) =
            r1 = r2 andalso GC.==(gc1,gc2) andalso loop(a,b)
         | loop _ = false
   in  loop(a,b) end

   fun meet(a,b) =
   let fun loop(a,[]) = []
         | loop([],a) = []
         | loop(a as (x as (r1,g1))::u, b as (y as (r2,g2))::v) =
            if r1 = r2 then (r1,GC.meet(g1,g2))::loop(u,v)
            else if r1 < r2 then loop(u,b)
            else loop(a,v)
   in  loop(a,b) end

   fun join(a,b) =
   let fun loop(a,[]) = a
         | loop([],a) = a
         | loop(a as (x as (r1,g1))::u, b as (y as (r2,g2))::v) =
            if r1 = r2 then (r1,GC.join(g1,g2))::loop(u,v)
            else if r1 < r2 then x::loop(u,b)
            else y::loop(a,v)
   in  loop(a,b) end

   fun meets [] = []
     | meets [a] = a
     | meets (a::l) = meet(a,meets l)

   fun joins [] = []
     | joins [a] = a
     | joins (a::l) = join(a,joins l)

   fun gen(a,b) =
   let fun loop(a:typemap,[]:typemap) = a 
         | loop([],a) = a
         | loop(a as (x as (r1,_))::u,b as (y as (r2,_))::v) =
            if r1 = r2 then y::loop(u,v)
            else if r1 < r2 then x::loop(u,b)
            else (* r1 > r2 *) y::loop(a,v)
   in  loop(a,b) end

   fun kill(a,b) = 
   let fun loop(a : typemap,[] : typemap) = a
         | loop([],_) = []
         | loop(a as (x as (r1,_))::u,b as (y as (r2,_))::v) =
           if r1 = r2 then loop(u,v)
           else if r1 < r2 then x::loop(u,b)
           else (* r1 > r2 *) loop(a,v)
   in  loop(a,b) end

end
        
