/* win32-raise-syserr.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 */

#include <windows.h>
#include "ml-base.h"
#include "ml-state.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "ml-c.h"

/* RaiseSysError:
 *
 * Raise the ML exception SysErr, which has the spec:
 *
 *    exception SysErr of (string * syserror option)
 *
 * We use the last win32-api error value as the syserror; eventually that
 * will be represented by an (int * string) pair.  If alt_msg is non-zero,
 * then use it as the error string and use NONE for the syserror.
 */
ml_val_t RaiseSysError (ml_state_t *msp, const char *altMsg)
{
    ml_val_t	    s, syserror, arg, exn;
    const char	    *msg;
    char	    buf[32];
    int             errno = -1;

    if (altMsg != NIL(char *)) {
	msg = altMsg;
	syserror = OPTION_NONE;
    }
    else {
        errno = (int) GetLastError();
	sprintf(buf, "<win32 error code %d>", errno);
	msg = buf;
	OPTION_SOME(msp, syserror, INT_CtoML(errno));
    }

    s = ML_CString (msp, msg);
    REC_ALLOC2 (msp, arg, s, syserror);
    EXN_ALLOC (msp, exn, PTR_CtoML(SysErrId), arg, LIST_nil);

    RaiseMLExn (msp, exn);

    return exn;

} /* end of RaiseSysError */
