(* bug1408.1.sml *)

IntBinarySet.app (print o Int.toString)
  (IntBinarySet.addList(IntBinarySet.empty,[1,2,3,4]));
