val smlpath = SMLofNJ.getCmdName()
val numprocs = 2

fun startSlaves(i) = 
    let val lastSlave =
	CM.Server.start { name = Int.toString i, pathtrans = NONE, pref = 0,
			  cmd = (smlpath, ["@CMslave"]) }
    in
	if i < numprocs then startSlaves(i + 1) else lastSlave
    end;

startSlaves(0); 

CM.autoload "$smlnj/cmb.cm";
CMB.make();
val _ : int = OS.Process.exit(OS.Process.success);
