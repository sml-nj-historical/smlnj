(* bug1114.1.sml *)

structure Type = 
struct
  datatype ty = Tyc of string
end;

structure P =
struct

fun pp_type ppstrm (Type.Tyc tyname) = 
   (Compiler.PrettyPrint.begin_block ppstrm Compiler.PrettyPrint.CONSISTENT 0;
    Compiler.PrettyPrint.add_string ppstrm tyname;
    Compiler.PrettyPrint.end_block ppstrm)

end;

val _ = Compiler.PPTable.install_pp ["Type","ty"] P.pp_type;

val test_type = Type.Tyc "bool";
