(*
 * Regenerates all the machine description generated files.
 * This works for versions 110.25 and 110.30+
 *)
fun b() = CM.make "Tools/MDL/sources.cm"; 
b(); 
fun c f = MDLGen.gen(f^"/"^f^".mdl");
app c
[ "x86",
  "sparc",
  "alpha",
  "hppa",
  "ppc",
  "mips"
];
