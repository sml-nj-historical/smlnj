(* bug1415.1.sml *)

Date.fmt
  (implode(List.tabulate(512, fn _ => #" ")))
  (Date.fromTimeLocal(Time.now()));
