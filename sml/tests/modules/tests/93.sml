signature A = sig type s end

signature B =
  sig
    structure A : sig type t end 
  end

functor F1(
	  structure B : B
          structure D : B 
	  structure A : A
	  sharing D = B 
	  sharing A = B.A) =
struct
end

functor F2(
	  structure A : A
	  structure B : B
          structure D : B 
	  sharing D = B 
	  sharing A = B.A) =
struct
end

functor F2(
          structure D : B 
	  structure B : B
	  structure A : A
	  sharing D = B 
	  sharing A = B.A) =
struct
end

