(* bug1482.1.sml *)

signature SUM =
sig

  type type1
  type type2

  datatype t =
    Type1 of type1
  | Type2 of type2

end


signature SET =
sig

  type element

  type t

  val empty: t

  val extend: t * element -> t

end


structure UnitSet =
struct

  type element = unit

  type t = bool

  val empty = false

  fun extend(_, ()) = true

end


functor SumSet
  (
    structure Sum:  SUM
    structure Set1: SET
		      where type element = Sum.type1
    structure Set2: SET
		      where type element = Sum.type2
  ) :> SET
	 where type element = Sum.t
    =
struct

  type element = Sum.t

  type t = Set1.t * Set2.t

  val empty = (Set1.empty, Set2.empty)

  fun extend((set1, set2), Sum.Type1 element1) =
      (Set1.extend(set1, element1), set2)
    | extend((set1, set2), Sum.Type2 element2) =
      (set1, Set2.extend(set2, element2))

end


structure UnitUnit =
struct

  type type1 = unit
  type type2 = unit

  datatype t =
    Type1 of type1
  | Type2 of type2

end


structure UnitUnitSet =
SumSet
  (
    structure Sum  = UnitUnit
    structure Set1 = UnitSet
    structure Set2 = UnitSet
  )
