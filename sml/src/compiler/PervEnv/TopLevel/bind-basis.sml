(* bin-basis.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds the OS-independent SML Basis Library signatures and
 * structures in the pervasive top-level environment.
 *
 *)

signature ARRAY = ARRAY
signature ARRAY2 = ARRAY2
signature BIN_IO = BIN_IO
signature BOOL = BOOL
signature BYTE = BYTE
signature CHAR = CHAR
signature COMMAND_LINE = COMMAND_LINE
signature DATE = DATE
signature GENERAL = GENERAL
signature IEEE_REAL = IEEE_REAL
signature IMPERATIVE_IO = IMPERATIVE_IO
signature INTEGER = INTEGER
signature IO = IO
signature LIST = LIST
signature LIST_PAIR = LIST_PAIR
signature MATH = MATH
signature MONO_ARRAY = MONO_ARRAY
signature MONO_ARRAY2 = MONO_ARRAY2
signature MONO_VECTOR = MONO_VECTOR
signature OPTION = OPTION
signature OS = OS
signature OS_FILE_SYS = OS_FILE_SYS
signature OS_IO = OS_IO
signature OS_PATH = OS_PATH
signature OS_PROCESS = OS_PROCESS
signature PACK_WORD = PACK_WORD
signature PRIM_IO = PRIM_IO
signature REAL = REAL
signature SML90 = SML90
signature STREAM_IO = STREAM_IO
signature STRING = STRING
signature STRING_CVT = STRING_CVT
signature SUBSTRING = SUBSTRING
signature TEXT_IO = TEXT_IO
signature TEXT_STREAM_IO = TEXT_STREAM_IO
signature TIME = TIME
signature TIMER = TIMER
signature VECTOR = VECTOR
signature WORD = WORD

structure Array = Array
structure Array2 = Array2
structure BinIO = BinIO
structure BinPrimIO = BinPrimIO
structure Bool = Bool
structure Byte = Byte
structure Char : CHAR = Char
structure CharArray = CharArray
structure CharVector = CharVector
structure CommandLine = CommandLine
structure Date = Date
structure General =
  struct
    open General
    open ExnName
  end
structure IEEEReal = IEEEReal
structure IO = IO
structure Int = Int
structure Int31 = Int31
structure Int32 = Int32
structure LargeInt = LargeInt
structure LargeReal = LargeReal
structure LargeWord = LargeWord
structure List = List
structure ListPair = ListPair
structure Math = Math
structure OS = OS
structure Option = Option
structure Pack16Big = Pack16Big
structure Pack16Little = Pack16Little
structure Pack32Big = Pack32Big
structure Pack32Little = Pack32Little
structure Position = Position
structure Real = Real
structure RealArray = Real64Array
structure RealVector = Real64Vector
structure Real64 = Real64
structure Real64Array = Real64Array
structure Real64Vector = Real64Vector
structure SML90 = SML90
structure String = String
structure StringCvt = StringCvt
structure Substring = Substring
structure SysWord = SysWord
structure TextIO = TextIO
structure TextPrimIO = TextPrimIO
structure Time = Time
structure Timer = Timer
structure Vector = Vector
structure Word = Word
structure Word31 = Word31
structure Word32 = Word32
structure Word8 = Word8
structure Word8Array = Word8Array
structure Word8Vector = Word8Vector

(*
 * $Log$
 *)
