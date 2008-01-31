(* windows.sml
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Structure for the interface to Windows.
 *
 *)

structure Windows : WINDOWS = 
  struct
    structure Key = Windows_KEY
    structure Reg = Windows_REG
    structure Config = Windows_CONFIG
    structure DDE = Windows_DDE
    structure Status = Windows_STATUS
  end 


