(* bug1043.sml *)

datatype 'a tree = Node of 'a tree * 'a tree | Leaf of 'a;
