(* test19.sml *)
(* keywords: structure, let *)

structure A =
let structure B = struct val x = 3 end
 in struct
      structure C = B
      val y = true
    end
end;
