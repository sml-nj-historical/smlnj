/* win32-process.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * interface to win32 process functions
 */

#include <windows.h>
#include <process.h>
#include <stdlib.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"


/* _ml_win32_PS_system : string -> word32
 *                       command
 *
 */
ml_val_t _ml_win32_PS_system(ml_state_t *msp, ml_val_t arg)
{
  int ret = system(PTR_MLtoC(char,arg));
  ml_val_t res;

  WORD_ALLOC(msp, res, (Word_t)ret);
  return res;
}

/* _ml_win32_PS_exit_process : word32 -> 'a
 *                             exit code
 *
 */
void _ml_win32_PS_exit_process(ml_state_t *msp, ml_val_t arg)
{
  ExitProcess((UINT)WORD_MLtoC(arg));
}

/* _ml_win32_PS_get_environment_variable : string -> string option
 *                                         var
 *
 */
ml_val_t _ml_win32_PS_get_environment_variable(ml_state_t *msp, ml_val_t arg)
{
#define GEV_BUF_SZ 4096
  char buf[GEV_BUF_SZ];
  int ret = GetEnvironmentVariable(PTR_MLtoC(char,arg),buf,GEV_BUF_SZ);
  ml_val_t ml_s,res = OPTION_NONE;

  if (ret > GEV_BUF_SZ) {
    return  RaiseSysError(msp, NIL(char *));
  }
  if (ret > 0) {
    ml_s = ML_CString(msp,buf);
    OPTION_SOME(msp,res,ml_s);
  }
  return res;
#undef GEV_BUF_SZ
}

/* end of win32-process.c */
