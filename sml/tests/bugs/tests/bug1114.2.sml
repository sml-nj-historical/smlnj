(* bug1114.2.sml *)

functor TYPE () =
struct
  datatype ty = Tyc of string
end;

structure Type = TYPE();

structure P =
struct

fun pp_type ppstrm (Type.Tyc tyname) = 
   (Compiler.PrettyPrint.begin_block ppstrm Compiler.PrettyPrint.CONSISTENT 0;
    Compiler.PrettyPrint.add_string ppstrm tyname;
    Compiler.PrettyPrint.end_block ppstrm)

end;

val _ = Compiler.PPTable.install_pp ["Type","ty"] P.pp_type;

val test_type = Type.Tyc "bool";
