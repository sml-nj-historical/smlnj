(* bug1052.sml *)
(* has to be executed in test directory *)

val ppconsumer = {consumer = Compiler.Control.Print.say, 
		  linewidth = !Compiler.Control.Print.linewidth, 
		  flush = Compiler.Control.Print.flush};
structure Source = Compiler.Source;
val fname = "/dev/null";
val instream = TextIO.openIn fname;
val source = Source.newSource(fname,1,instream,false,ppconsumer);
Source.closeSource source;
