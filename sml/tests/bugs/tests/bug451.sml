(* bug451.sml *)

signature A = sig structure Base:sig end end;

signature P = sig end;

functor A (structure P:P) : A =
struct
  structure Base = P;
end;

signature B = sig structure Base:sig end end;

functor B (structure P:P structure A:A sharing P = A.Base ) : B =
struct
  structure Base = P;
end;

functor Q(structure P : P
          structure A : A 
          structure B : B 
          sharing P = A.Base = B.Base) = struct end ;

structure P = struct end ;

structure A = A(structure P = P);
structure B = B(structure P = P structure A = A);

structure Q = Q(structure P = P
                structure A = A
                structure B = B);
