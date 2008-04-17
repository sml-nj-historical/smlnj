(* test-staged-allocation-main.sml
 * 
 * This module tests C calls for staged allocation.  We generate tests for a variety
 * of calls.  To verify the test, we generate a dummy C function that prints out its 
 * parameters in order.
 *
 * The example code below passes arguments x and y to the MLRISC-generated C call. The output
 * of glue.c and sanity.c should be identical.

 /* glue.c */
 #define MAX_SZ 16

 int target (int x, float y)
 {
   printf("%d %f", x, y);
   return 23432;
 }

 void glueCode() 
 {
      void* arr0[4096];
      void** arr = arr0;
      /* initialize arguments */
      int x = 23423;
      memcpy(arr, &x, sizeof(int));
      arr += MAX_SZ;
      float y = 1024.013f;
      memcpy(arr, &y, sizeof(float));
      arr += MAX_SZ;
      /* call into the MLRISC glue code that will make the C call */
      int tmp = mlriscGlue(arr0);
      printf("%d", tmp);
 }

 int main () 
 {
     glueCode();
     return 0;
 }

 /* sanity.c */
 int main ()
 {
     printf("%d %f %d", 23423, 1024.013f, 23432);
     return 0;
 }

 * We also generate the mlriscGlue code in MLRISC.  This code grabs the arguments from
 * arr0 and then passes them to target using Staged Allocation.
 * 
 *)

structure Main =
  struct

    structure CTy = CTypes
    structure C = TestStagedAllocation.C
    structure T = TestStagedAllocation.T
    structure CFG = TestStagedAllocation.CFG
    structure Test = TestStagedAllocation

    (* machine-specific data *)
    val wordTy = 64
    val wordSzB = wordTy div 8
    val param0 = T.REG(wordTy, C.rdi)
    (* maximum argument size in machine words *)
    val maxArgSz = 16
    val maxArgSzB = maxArgSz * wordSzB
    val retValVar = "retVal"

    fun li i = T.LI (T.I.fromInt (wordTy, i))

    datatype c_argument =
	     INT of int
	   | FLOAT of real
	   | DOUBLE of real
	   | POINTER of int

    fun cArgToString (cArg) = (case cArg
        of INT i => Int.toString i
	 | FLOAT f => Real.toString f^"f"
	 | DOUBLE f => Real.toString f
	 | POINTER p => "(void*)0x"^Int.fmt StringCvt.HEX p
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

    fun genPrintf (formatString, args) =
	"printf("^formatString^","^(String.concatWith ", " args)^");"

    fun genFormatString (elts) =
	"\"" ^ String.concatWith " " elts ^ "\\n\""

    (* construct a format string printing the parameters of a proto *)
    fun protoToFormatString {conv, retTy, paramTys} = genFormatString (List.map tyToFormatString paramTys)

    fun protoToPrintf (proto, args) = genPrintf(protoToFormatString(proto), args)	

    (* generate a dummy target function that prints its parameters *)
    fun targetFun (targetName, proto as {conv, retTy, paramTys}, retVal) = let
        val (_, params, vars) = List.foldl cTyToParam (0, [], []) paramTys
	val (params, vars) = (List.rev params, List.rev vars)
	in 
           cTyToString retTy ^ " " ^ targetName ^ "(" ^ (String.concatWith ", " params) ^ ")" ^
	      "{" ^
	          protoToPrintf(proto, vars) ^
	          "return "^(String.concat (List.map cArgToString retVal))^";"^
              "}"
        end

    fun genMLRISCGlueHdr (mlriscGlue, proto as {conv, retTy, paramTys}) = let
        val (_, params, vars) = List.foldl cTyToParam (0, [], []) paramTys
	val (params, vars) = (List.rev params, List.rev vars)
	in 
           cTyToString retTy ^ " " ^ mlriscGlue ^ "(void* arr0);"
        end

    (* generate C code that initializes an argument *) 
    fun genArg ((ty, arg), (i, assignStms)) = 
	(i+1, 
	 String.concatWith "\t" [
         "{",
              cTyToString(ty)^" tmp = "^cArgToString(arg)^";",
	      "memcpy(arr, &tmp, sizeof("^cTyToString(ty)^"));",
	      "arr += MAX_SZ;",
         "}\n"
         ]
	 :: assignStms)

    (* generate C code that calls the MLRISC-generated function *)
    fun genCGlueCode (mlriscGlue, proto as {conv, retTy, paramTys}, args) = let
	val (_, stms) = List.foldl genArg (0, []) (ListPair.zip (paramTys, args))
	val glueCall = if retTy <> CTy.C_void
                          then cTyToString retTy ^ " " ^retValVar^" = " ^ mlriscGlue^"(arr0);\n\t"^
			       genPrintf(genFormatString([tyToFormatString retTy]), [retValVar])
		          else mlriscGlue^"(arr0);"
        in
	   String.concatWith "\n\t" [
             "void glueCode(){",
                (* initialize arguments *)
                "void* arr0[4096];",
                "void** arr = arr0;",
                String.concatWith "\t " (List.rev stms),
	        glueCall,
	      "}"
            ]
        end

    val cIncludes = String.concatWith "\n" [
        "#include <stdio.h>",
        "#include <stdlib.h>",
        "#include <string.h>\n"
    ]

    fun genGlue (target, mlriscGlue, proto, args, retVal) = String.concatWith "\n" [
          cIncludes,
  	  "#define MAX_SZ "^Int.toString(maxArgSz),
	  genMLRISCGlueHdr(mlriscGlue, proto),
	  targetFun(target, proto, retVal),
	  genCGlueCode(mlriscGlue, proto, args)
        ]

    fun genCMain () = "int main () { glueCode(); return 0; }"

    fun genSanityCheck (proto, args, retVal) = let
	val retPrintf = (case retVal
             of [] => ""
	      | [retVal] => genPrintf(genFormatString [tyToFormatString (#retTy proto)], [cArgToString retVal])
            (* end case *))
        in
	    cIncludes^
            "int main () { "^protoToPrintf(proto, args)^retPrintf^" return 0; }"
        end

    fun offset arr0 i = T.ADD(wordTy, arr0, li(i*maxArgSzB))
    fun genGlueArg arr0 (ty, (i, args)) = (i+1, 
	(case ty
          of CTy.C_signed CTy.I_int => CCalls.ARG (T.LOAD(32, offset arr0 i, ()))
	   | CTy.C_unsigned CTy.I_int => CCalls.ARG (T.LOAD(32, offset arr0 i, ()))
	   | CTy.C_PTR => CCalls.ARG (T.LOAD(wordTy, offset arr0 i, ()))
	   | CTy.C_float => CCalls.FARG (T.FLOAD(32, offset arr0 i, ()))
	   | CTy.C_double => CCalls.FARG (T.FLOAD(64, offset arr0 i, ()))
        (* end case *)) :: args)

    val rand = Random.rand (0, 255)
    fun genRandArg (ty) = (case ty
        of CTy.C_float => FLOAT (Random.randReal(rand))
	 | CTy.C_double => DOUBLE(Random.randReal(rand))
	 | CTy.C_unsigned _ => INT (Random.randNat(rand))
	 | CTy.C_signed _ => INT (Random.randNat(rand))
	 | CTy.C_PTR => POINTER(Random.randNat(rand))
        (* end case *))

    fun output (strm, s) = TextIO.output(strm, s^"\n")

    fun main _ = let
	val retTy = CTy.C_double
	val paramTys = [CTy.C_double, CTy.C_unsigned CTy.I_int, CTy.C_PTR, CTy.C_double, 
		   CTy.C_float, CTy.C_PTR, CTy.C_float, CTy.C_PTR, CTy.C_PTR, CTy.C_PTR,
		   CTy.C_signed CTy.I_int, 
		   CTy.C_double, CTy.C_double, CTy.C_double, CTy.C_double, CTy.C_double, 
		   CTy.C_double, CTy.C_double]

	val cArgs = List.map genRandArg paramTys
	val retVal = if retTy <> CTy.C_void then [genRandArg retTy] else []
	val proto = {conv="ccall", retTy=retTy, paramTys=paramTys}
	val mlriscGlue = "mlriscGlue"
	val target = "target"

	(* output C code that glues to the MLRISC code  *)
	val cOutStrm = TextIO.openOut "glue.c"
	val cCode = genGlue(target, mlriscGlue, proto, cArgs, retVal)
	val _ = output(cOutStrm, cCode)
	val _ = TextIO.closeOut cOutStrm

	(* output C code for santity check *)
	val cOutStrm = TextIO.openOut "sanity.c"
	val cCode = genSanityCheck(proto, List.map cArgToString cArgs, retVal)
	val _ = output(cOutStrm, cCode)
	val _ = TextIO.closeOut cOutStrm

	(* output main *)
	val cMainOutStrm = TextIO.openOut "main.c"
	val cMain = genCMain()
	val _ = output(cMainOutStrm, cMain)
	val _ = TextIO.closeOut cMainOutStrm
		    
	(* output MLRISC code*)
	val tmpReg = C.newReg()
	val tmpR = T.REG(wordTy, tmpReg)
	val (_, glueArgs) = List.foldl (genGlueArg tmpR) (0, []) paramTys
	val asmOutStrm = TextIO.openOut "mlrisc.s"
	fun doit () = Test.dumpOutput(Test.codegen(mlriscGlue, target, proto, [T.MV(wordTy, tmpReg, param0)], List.rev glueArgs))
	val _ = AsmStream.withStream asmOutStrm doit ()
	val _ = TextIO.closeOut asmOutStrm
	in          
	  0
        end


  end
