/* proc.c
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 */

#include "ml-unixdep.h"
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include "ml-objects.h"
#include "ml-c.h"
#include "sml-basis.h"

/* errorName:
 */
ML_string_t errorName (ml_state_t *msp, int err)
{
    char	buf[32];

    sprintf(buf, "[ERR=%d]", err);
    return ML_CString(msp, buf);

} /* end of errorName */

ML_int_opt_t syserror (ml_state_t *msp, idl_string errName)
{
    return OPTION_NONE;

} /* end of syserror */

/* errorMessage:
 */
ML_string_t errorMessage (ml_state_t *msp, int err)
{
    ml_val_t		s;

    if ((0 <= err) && (err < sys_nerr))
	s = ML_CString (msp, sys_errlist[err]);
    else {
	char		buf[32];
	sprintf(buf, "<unknown error %d>", err);
	s = ML_CString (msp, buf);
    }

    return s;

} /* end of errorMessage */

/* osSystem:
 */
ML_int_t osSystem (ml_state_t *msp, idl_string name)
{
    int		sts;

    sts = system (name);

    if ((sts == 127) || (sts == -1))
	return RAISE_SYSERR(msp, sts);
    else
	return INT_CtoML(sts);

} /* end of osSystem */

/* exitProc:
 */
void exitProc (int sts)
{
    Exit (sts);

    /*NOTREACHED*/

} /* end of exit */

/* getEnv:
 */
ML_string_opt_t getEnv (ml_state_t *msp, idl_string arg)
{
    char     *sts;
    ml_val_t r, s;

    sts = getenv(arg);
    if (sts == NIL(char *))
        r = OPTION_NONE;
    else {
        s = ML_CString(msp,sts);
        OPTION_SOME(msp, r, s)
    }
  
    return r;
} /* end of getEnv */

/* osSleep:
 */
void osSleep (Time_t *t)
{
    int		tim = t->seconds;

    while (tim > 0) {
	tim = sleep(tim);
    }

} /* end of osSleep */

