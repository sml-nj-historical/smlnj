(* bug1042.1.sml *)

signature SIG =
sig
  datatype access_mode = A_READ | A_WRITE | A_EXEC
  sharing type access_mode = Posix.FileSys.access_mode
end;

structure S : SIG =
struct
  structure P : sig datatype access = A_EXEC | A_READ | A_WRITE end =
    Posix.FileSys 
  open P
end;
