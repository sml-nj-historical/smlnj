(* bug217.sml *)
(* Problem: Runbind exception after simultaneous open declaration *)

structure A = struct val x = 3  end;

structure B = struct structure A = A; end;

open A B;
(* following caused Runbind to be raised *)
x;
