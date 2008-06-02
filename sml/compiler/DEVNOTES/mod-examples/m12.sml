(* This program tests repEPs. It should compute two unique formal types 
 * Key1.hash_key and Key2.hash_key as repEPs.
 *)

signature H =
  sig
    type hash_key

    val hashVal : hash_key -> unit 
  end 

functor Hash2TableFn (
    structure Key1 : H
    structure Key2 : H
  ) = struct end
