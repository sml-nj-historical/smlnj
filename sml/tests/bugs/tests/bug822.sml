(* bug822.sml *)
(* 822. Compiler bug: compType/checkweak (as secondary error) *)

signature STATIC_ARRAY =
sig
  type elem
  type array
  val array : int * elem -> array
end

functor DynamicArray (A : STATIC_ARRAY) = struct end

structure ModGraph =
struct
  structure NodesArray = DynamicArray (struct
      type elem = G.node list
      type array = elem Array.array
      val array = Array.array
    end)
end
