(* bug1325.sml *)

val x = Posix.FileSys.openf("/etc/passwd", Posix.Filesys.O_RDONLY, 0);
