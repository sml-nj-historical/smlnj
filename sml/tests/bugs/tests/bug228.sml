(* bug228.sml *)

(* string to real conversion function gives wrong answer *)

exception bad_number;

fun string2real_bug (s : string) = let
    val (fac,li) = case explode s of
        #"-" :: li => (~1.0,li) |
        #"+" :: li => (1.0,li) |
        li => (1.0,li)
    val (res,_) = (List.foldl (fn (c,(a,fac1)) => let
        val n = ord c - ord #"0"
        in
        if fac1 > 0.0
        then
            if n < 0 orelse n > 9
            then raise bad_number
            else (a + fac1 * (real n),fac1/10.0)
        else if c = #"."
        then (a,1.0/10.0)
        else
            if n < 0 orelse n > 9
            then raise bad_number
            else (10.0 * a + (real n),0.0)
        end) (0.0,0.0) li)
    in
    res * fac
    end;

string2real_bug "123.456";
