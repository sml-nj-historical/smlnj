(* bug1576.1.sml *)

val x = 1.0/0.0 + ~1.0/0.0;
val _ = print (concat["x = ", 
		      Real.toString x, "\n",
		      "Real.==(x,x) = ", 
		      Bool.toString (Real.==(x,x)), "\n",
		      "Real.!=(x,x) = ", 
		      Bool.toString (Real.!=(x,x)), "\n",
		      "Real.?=(x,x) = ", 
		      Bool.toString (Real.?=(x,x)), "\n",
		      "Real.==(x,1.0) = ", 
		      Bool.toString (Real.==(x,1.0)), "\n",
		      "Real.!=(x,1.0) = ", 
		      Bool.toString (Real.!=(x,1.0)), "\n",
		      "Real.?=(x,1.0) = ", 
		      Bool.toString (Real.?=(x,1.0)), "\n",
		      "Real.==(1.0,1.0) = ", 
		      Bool.toString (Real.==(1.0,1.0)), "\n",
		      "Real.!=(1.0,1.0) = ", 
		      Bool.toString (Real.!=(1.0,1.0)), "\n",
		      "Real.?=(1.0,1.0) = ", 
		      Bool.toString (Real.?=(1.0,1.0)), "\n"]);
