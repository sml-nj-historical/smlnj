(* 83.sml *)
(* test pervasive boot env. *)

structure S =
struct
  val x = Int.+
end

(* test ability to access global env. during signature parsing *)

signature S1 =
sig
  structure A : sig type int end = Int
end

signature S2 =
sig
  val x : Int.int
end

(* test simple type sharing constraints *)

signature S3 =
sig
  type x = int
end

(* this structure correctly satisfies the sharing constraints *)

structure A : S3 =
struct
  type x=int
end

(* this one doesn't *)

structure B : S3 =
struct
  type x=bool
end

(* test simple structure sharing constraint *)

(* positive case *)

structure C : S1 =
struct
  structure A = Int
end    

(* negative case *)

structure C : S1 =
struct
  structure A = Real
end

(* define a structure and use it in a sharing constraint *)

structure B = struct type t = int end

signature S4 =
sig
  structure A  : sig type t end = B
end

(* hide it *)

structure B = struct type t = bool end

(* test error messages for sharing constraint involving structure that is
   has been hidden *)

(* violate the sharing constraint *)

structure S : S4 =
struct
  structure A = B
end
	    

   
