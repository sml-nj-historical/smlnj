(* bug1109.4.sml *)

datatype nodep = NNP of node ref | NP
and       node = Node of int * nodep;
NNP(ref(Node(0,NP)));
