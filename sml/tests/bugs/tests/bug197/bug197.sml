(* bug.197 *)
(* exportFn image size *)

use "load.sml";
loadAll();
open ParseGen;

fun main (argv, env) =
    let val argc = length argv
        val prog = hd argv
     in if (argc <> 2) then
            outputc std_out ("Usage: " ^ prog ^ " file\n")
        else let val file = hd (tl argv)
              in parseGen file
                  handle Io s =>
                      outputc std_out
                          (prog ^ ": Couldn't open file: "
                           ^ file ^ "\n")
             end;
             outputc std_out "\n")
    end;

exportFn ("smlyacc", main);
