(* bug1444.1.sml *)

val d = Date.date{year=1,month=Date.Jan,day=1,hour=0,minute=0,second=0,offset=NONE};
Date.toString d;
