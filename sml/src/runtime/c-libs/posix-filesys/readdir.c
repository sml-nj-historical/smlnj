/* readdir.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_readdir : object -> string
 *
 * Return the next filename from the directory stream.
 */
ml_val_t _ml_P_FileSys_readdir (ml_state_t *msp, ml_val_t arg)
{
    struct dirent      *dirent;
    
    errno = 0;
    dirent = readdir(PTR_MLtoC(DIR, arg));
    if (dirent == NIL(struct dirent *)) {
      if (errno != 0)     /* Error occurred */
	return RaiseSysError(msp, NIL(char *));
      else                /* End of stream */
        return ML_string0;
    }
    else
      return ML_CString (msp, dirent->d_name);

} /* end of _ml_P_FileSys_readdir */
