(* bug 327 -- large constants should not cause a compiler error but
   rather a parse error.   Transcript should read as follows:

   - 1073751823;
   std_in:2.1-2.10 Error: integer too large
*)
1073751823;
