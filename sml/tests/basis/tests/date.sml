(* test/date.sml
   PS 1995-03-20, 1995-05-12, 1996-07-05
*)

structure Date = struct
  exception NotImplemented
  open Date
  fun fromString _ = raise NotImplemented
end

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

local
    open Time Date 
    fun date h = 
	toString(fromTimeLocal(now() + fromReal (3600.0 * real h))) ^ "\n";
    fun nowdate () = Date.fromTimeLocal(now());
    fun mkdate(y,mo,d,h,mi,s) =
	 Date.date{year=y, month=mo, day=d, hour=h, minute=mi, second=s, 
	  offset=NONE}
    fun cmp(dt1, dt2) = compare(mkdate dt1, mkdate dt2)

    fun fromto dt = 
	toString (Option.valOf(Date.fromString (toString dt))) = toString dt

    fun tofrom s = 
	toString (Option.valOf(Date.fromString s)) = s
in
    
val _ = 
    (print "This is now:                    "; print (date 0);
     print "This is an hour from now:       "; print (date 1);
     print "This is a day from now:         "; print (date 24);
     print "This is a week from now:        "; print (date 168);
     print "This is 120 days from now:      "; print (date (24 * 120));
     print "This is 160 days from now:      "; print (date (24 * 160));
     print "This is 200 days from now:      "; print (date (24 * 200));
     print "This is 240 days from now:      "; print (date (24 * 240));
     print "This is the epoch (UTC):        "; 
     print (toString(fromTimeUniv zeroTime) ^ "\n");   
     print "This is the number of the day:  "; 
     print (fmt "%j" (nowdate()) ^ "\n");
     print "This is today's weekday:        ";
     print (fmt "%A" (nowdate()) ^ "\n");
     print "This is the name of this month: ";
     print (fmt "%B" (nowdate()) ^ "\n"));

val test1 = 
check'(fn _ => 
               cmp((1993,Jul,25,16,12,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1995,May,25,16,12,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,May,26,16,12,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jul,24,16,12,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,24,17,12,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,26,15,12,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,15,13,18), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,25,17,11,18), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,16,11,19), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,25,16,13,17), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,16,12,17), (1994,Jun,25,16,12,18)) = LESS
       andalso cmp((1994,Jun,25,16,12,19), (1994,Jun,25,16,12,18)) = GREATER
       andalso cmp((1994,Jun,25,16,12,18), (1994,Jun,25,16,12,18)) = EQUAL);

val test2 = 
    check'(fn _ => 
	   fmt "%A" (mkdate(1995,May,22,4,0,1)) = "Monday");

val test3 = 
    check'(fn _ => 
	   List.all fromto 
	   [mkdate(1995,Aug,22,4,0,1),
	    mkdate(1996,Apr, 5, 0, 7, 21),
	    mkdate(1996,Mar, 5, 6, 13, 58)]);

val test4 = 
    check'(fn _ => 
	   List.all tofrom 
	   ["Fri Jul  5 14:25:16 1996",
	   "Mon Feb  5 04:25:16 1996",
	   "Sat Jan  6 04:25:16 1996"])
    
end
