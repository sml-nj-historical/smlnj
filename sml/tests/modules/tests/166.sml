signature SORTED_SET_ITEM =
sig
  type t
end

functor SortedSet (I:SORTED_SET_ITEM) =
struct
 open I
end
