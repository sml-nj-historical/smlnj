(* bug1401.1.sml *)

open Posix.Process;
val _ =
   case fork()
     of NONE => (sleep(Time.fromSeconds 5); exit 0w0)
      | SOME pid => (waitpid_nh(W_CHILD pid, []); ());
