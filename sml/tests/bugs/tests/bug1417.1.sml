(* bug1417.1.sml *)

signature DB =
sig
  type foo = int
  datatype partition_classes = STATIC_CLASSES of foo
  and alloc_class = ALLOC_CLASS of unit
end;

functor Evolve(Db : DB) = 
struct 
  datatype partition_classes = datatype Db.partition_classes
end;

