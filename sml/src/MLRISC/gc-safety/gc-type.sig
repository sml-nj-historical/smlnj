(*
 * Abstract interface for GC types.
 *)
signature GC_TYPE =
sig

   structure O : OBJ_TYPE (* client defined object representation type *)

   type ty = int

   datatype gctype = 
     CONST of int                 (* integer constant *)
   | NONREF of O.objtype ref      (* non-reference value *)
   | REF of O.objtype ref         (* a reference, pointer to a gc object *)
   | ADD of ty * gctype * gctype  (* address arithmetic + *)
   | SUB of ty * gctype * gctype  (* address arithmetic - *)
   | BOT
   | TOP

   type gcmap = gctype Intmap.intmap

   exception GCTYPE

   val toString     : gctype -> string
   val mapToString  : gcmap -> (int -> string)

   (*
    * Annotations.
    *)
   exception GCMAP of gcmap  (* gc-map for a program (per cluster) *)
   exception GCSAFEPOINT     (* marks all gcpoints (per block) *)
  
end
