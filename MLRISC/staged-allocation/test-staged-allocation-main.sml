(* test-staged-allocation-main.sml
 * 
 * This module tests C calls for staged allocation.  We generate tests for a variety
 * of calls.  To verify the test, we generate a dummy C function that prints out its 
 * parameters in order.
 *

 #define MAX_SZB 16

 void target (int x, float y)
 {
   printf("%d %f", x, y);
 }

 void glueCode() 
 {
      void* arr0[4096];
      void** arr = arr0;
      /* initialize arguments */
      int x = 23423;
      memcpy(arr, &x, sizeof(int));
      arr += MAX_SZB;
      float y = 1024.013;
      memcpy(arr, &y, sizeof(float));
      arr += MAX_SZB;
      /* call into the MLRISC glue code that will make the C call */
      mlriscGlue(arr0);
 }

 int main () {
     glueCode();
     return 0;
 }

 * 
 *)

structure Main =
  struct

    structure CTy = CTypes
    structure C = TestStagedAllocation.C
    structure T = TestStagedAllocation.T
    structure CFG = TestStagedAllocation.CFG
    structure Test = TestStagedAllocation

    fun li i = T.LI (T.I.fromInt (64, i))

    val maxArgSzB = 16

    datatype c_argument =
	     INT of int
	   | FLOAT of real
	   | POINTER of int

    fun cArgToString (cArg) = (case cArg
        of INT i => Int.toString i
	 | FLOAT f => Real.toString f
	 | POINTER p => Int.toString p
        (* end case *))

    fun tyToFormatString (ty) = (case ty
        of CTy.C_unsigned _ => "%u"
	 | CTy.C_signed _ => "%d"
	 | CTy.C_float => "%f"
	 | CTy.C_double => "%f"
	 | CTy.C_PTR => "%p"
        (* end case *))

    fun cTyToString (ty) = (case ty
        of CTy.C_unsigned _ => "unsigned int"
	 | CTy.C_signed _ => "int"
	 | CTy.C_float => "float"
	 | CTy.C_double => "double"
	 | CTy.C_PTR => "void*"
	 | CTy.C_void => "void"
        (* end case *))

    fun cTyToParam (ty, (i, params, vars)) = let
        val var = " a"^Int.toString i
        in 
           (i+1, (cTyToString(ty)^var) :: params, var :: vars)
        end

    (* construct a format string printing the parameters of a proto *)
    fun protoToFormatString {conv, retTy, paramTys} =
	"\"" ^ String.concatWith " " (List.map tyToFormatString paramTys) ^ "\\n\""

    (* generate a dummy target function that prints its parameters *)
    fun targetFun (targetName, proto as {conv, retTy, paramTys}) = let
        val (_, params, vars) = List.foldl cTyToParam (0, [], []) paramTys
	val (params, vars) = (List.rev params, List.rev vars)
	in 
           cTyToString retTy ^ " " ^ targetName ^ "(" ^ (String.concatWith ", " params) ^ ")" ^
	      "{" ^
	          "printf("^protoToFormatString(proto)^","^(String.concatWith ", " vars)^");" ^
              "}"
        end

    (* generate C code that initializes an argument *) 
    fun genArg ((ty, arg), (i, assignStms)) = 
	(i+1, String.concatWith "\t" [
                "{",
                cTyToString(ty)^" tmp = "^cArgToString(arg)^";",
		"memcpy(arr, &tmp, sizeof("^cTyToString(ty)^"));",
		"arr += MAX_SZB;",
		"}\n"
               ]
	      :: assignStms)

    (* generate C code that calls the MLRISC-generated function *)
    fun genCGlueCode (mlriscGlue, proto as {conv, retTy, paramTys}, args) = let
	val (_, stms) = List.foldl genArg (0, []) (ListPair.zip (paramTys, args))
        in
	   String.concatWith "\n\t" [
             "void glueCode(){",
                (* initialize arguments *)
                "void* arr0[4096];",
                "void** arr = arr0;",
                String.concatWith "\t " (List.rev stms),
	        mlriscGlue^"(arr0);",
	      "}"
            ]
        end

    fun genCCode (target, mlriscGlue, proto, args) = String.concatWith "\n" [
          "#include <stdio.h>",
          "#include <stdlib.h>",
          "#include <string.h>",
  	  "#define MAX_SZB "^Int.toString(maxArgSzB),
	  targetFun(target, proto),
	  genCGlueCode(mlriscGlue, proto, args)
        ]

    fun genCMain () = "int main () { glueCode(); return 0; }"
	
    fun main _ = let
	val tys = [CTy.C_signed CTy.I_int, CTy.C_PTR, CTy.C_PTR]
	val proto = {conv="ccall", retTy=CTy.C_void, paramTys=tys}
	val mlriscGlue = "mlriscGlue"
	val target = "target"

	val args = [CCalls.ARG (li 1024), CCalls.ARG (li 1024), CCalls.ARG (li 1024)]
	val cArgs = [POINTER 0, POINTER 1, POINTER 2]

	(* output C code *)
	val cOutStrm = TextIO.openOut "glue.c"
	val cCode = genCCode(target, mlriscGlue, proto, cArgs)
	val _ = TextIO.output(cOutStrm, cCode)
	val _ = TextIO.closeOut cOutStrm

	(* output main *)
	val cMainOutStrm = TextIO.openOut "main.c"
	val cMain = genCMain()
	val _ = TextIO.output(cMainOutStrm, cMain)
	val _ = TextIO.closeOut cMainOutStrm
		    
	(* output MLRISC code*)
	val asmOutStrm = TextIO.openOut "mlrisc.s"
	fun doit () = Test.dumpOutput(
		      Test.codegen(mlriscGlue, target, proto, args))
	val _ = AsmStream.withStream asmOutStrm doit ()
	val _ = TextIO.closeOut asmOutStrm
	in          
	  ()
        end


  end
