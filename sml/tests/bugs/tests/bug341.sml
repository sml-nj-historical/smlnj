datatype 'a A = A of 'a ref;
val f = fn () => ();
val f1 = fn () => ();
f = f1;
ref f = ref f1;
ref 2 = ref 2;
A (ref f) = A (ref f1);
A (ref 2) = A (ref 2);
