(* bug414.sml *)

fun test path =
    let
       val cwd = OS.FileSys.getDir ()
    in
       { cd_in = fn () => (OS.FileSys.chDir path),
	 cd_out = fn () => (OS.FileSys.chDir cwd) }
    end

val {cd_in,cd_out} = test "/tmp";

cd_in ();

cd_out ();
