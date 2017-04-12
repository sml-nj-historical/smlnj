(* json-util.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Utility functions for processing the JSON in-memory representation.
 *)

structure JSONUtil : sig

  (* exceptions for conversion functions *)
    exception NotBool of JSON.value
    exception NotInt of JSON.value
    exception NotNumber of JSON.value
    exception NotString of JSON.value

  (* exception that is raised when trying to process a non-object value as an object *)
    exception NotObject of JSON.value

  (* exception that is raised when the given field is not found in an object *)
    exception FieldNotFound of JSON.value * string

  (* exception that is raised when trying to process a non-array value as an array *)
    exception NotArray of JSON.value

  (* map the above exceptions to a message string; we use General.exnMessage for other
   * exceptions.
   *)
    val exnMessage : exn -> string

  (* conversion functions for atomic values.  These raise the corresponding
   * "NotXXX" exceptions when their argument has the wrong shape.  Also note
   * that asNumber will accept both integers and floats.
   *)
    val asBool : JSON.value -> bool
    val asInt : JSON.value -> IntInf.int
    val asNumber : JSON.value -> Real.real
    val asString : JSON.value -> string

  (* find a field in an object; this function raises the NotObject exception when
   * the supplied value is not an object.
   *)
    val findField : JSON.value -> string -> JSON.value option

  (* lookup a field in an object; this function raises the NotObject exception when
   * the supplied value is not an object and raises FieldNotFound if the value is
   * an object, but does not have the specified field.
   *)
    val lookupField : JSON.value -> string -> JSON.value

  (* convert a JSON array to an SML vector *)
    val asArray : JSON.value -> JSON.value vector

  (* map a conversion function over a JSON array to produce a list; this function
   * raises the NotArray exception if the second argument is not an array.
   *)
    val arrayMap : (JSON.value -> 'a) -> JSON.value -> 'a list

  end = struct

    structure J = JSON

    exception NotBool of JSON.value
    exception NotInt of JSON.value
    exception NotNumber of JSON.value
    exception NotString of JSON.value

    exception NotObject of JSON.value

    exception FieldNotFound of JSON.value * string

    exception NotArray of JSON.value

  (* conversion functions for atomic values *)
    fun asBool (J.BOOL b) = b
      | asBool v = raise NotBool v

    fun asInt (J.INT n) = n
      | asInt v = raise NotInt v

    fun asNumber (J.INT n) = Real.fromLargeInt n
      | asNumber (J.FLOAT f) = f
      | asNumber v = raise NotNumber v

    fun asString (J.STRING s) = s
      | asString v = raise NotString v

    fun findField (J.OBJECT fields) = let
	  fun find lab = (case List.find (fn (l, v) => (l = lab)) fields
		 of NONE => NONE
		  | SOME(_, v) => SOME v
		(* end case *))
	  in
	    find
	  end
      | findField v = raise NotObject v

    fun lookupField (v as J.OBJECT fields) = let
	  fun find lab = (case List.find (fn (l, v) => (l = lab)) fields
		 of NONE => raise FieldNotFound(v, concat["no definition for field \"", lab, "\""])
		  | SOME(_, v) => v
		(* end case *))
	  in
	    find
	  end
      | lookupField v = raise NotObject v

    fun asArray (J.ARRAY vs) = Vector.fromList vs
      | asArray v = raise NotArray v

    fun arrayMap f (J.ARRAY vs) = List.map f vs
      | arrayMap f v = raise NotArray v

  (* map the above exceptions to a message string; we use General.exnMessage for other
   * exceptions.
   *)
    fun exnMessage exn = let
	  fun v2s (J.ARRAY _) = "array"
	    | v2s (J.BOOL false) = "'false'"
	    | v2s (J.BOOL true) = "'true'"
	    | v2s (J.FLOAT _) = "number"
	    | v2s (J.INT _) = "number"
	    | v2s J.NULL = "'null'"
	    | v2s (J.OBJECT _) = "object"
	    | v2s (J.STRING _) = "string"
	  in
	    case exn
	     of NotBool v => String.concat[
		    "expected boolean, but found ", v2s v
		  ]
	      | NotInt(J.FLOAT _) => "expected integer, but found floating-point number"
	      | NotInt v => String.concat[
		    "expected integer, but found ", v2s v
		  ]
	      | NotNumber v => String.concat[
		    "expected number, but found ", v2s v
		  ]
	      | NotString v => String.concat[
		    "expected string, but found ", v2s v
		  ]
	      | NotObject v => String.concat[
		    "expected object, but found ", v2s v
		  ]
	      | FieldNotFound(v, fld) => String.concat[
		    "no definition for field \"", fld, "\" in object"
		  ]
	      | NotArray v => String.concat[
		    "expected array, but found ", v2s v
		  ]
	      | _ => General.exnMessage exn
	    (* end case *)
	  end

  end
