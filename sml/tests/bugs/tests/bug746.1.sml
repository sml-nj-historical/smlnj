(* bug746.1.sml *)

datatype foo  =  bar of unit | baz of int;

datatype zip  = biz of unit;
    
bar ();
baz 3;
biz ();
