/* dir.c
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies.
 *
 * SML Basis directory support.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include "sml-basis.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

/* openDir:
 */
ML_directory_t openDir (ml_state_t *msp, idl_string path)
{
    DIR		*dir;

    if ((dir = opendir(path)) == NIL(DIR *)) {
	return RAISE_SYSERR(msp, -1);
    }
    else {
	return PTR_CtoML(dir);
    }

}

ML_string_opt_t readDir (ml_state_t *msp, ML_directory_t arg)
{
    DIR			*dir = PTR_MLtoC(DIR, arg);
    struct dirent	*dirent;

    while (TRUE) {
	errno = 0;
	dirent = readdir(dir);
	if (dirent == NIL(struct dirent *)) {
	    if (errno != 0)     /* Error occurred */
		return RAISE_SYSERR(msp, -1);
	    else                /* End of stream */
		return OPTION_NONE;
	}
	else {
	    char	*cp = dirent->d_name;
	    if ((cp[0] == '.')
	    && ((cp[1] == '\0') || ((cp[1] == '.') && (cp[2] == '\0'))))
		continue;
	    else {
	        ml_val_t res;
		res = ML_CString (msp, cp);
		OPTION_SOME(msp, res, res);
		return res;
	    }
	}
    }

}

ML_unit_t rewindDir (ml_state_t *msp, ML_directory_t arg)
{
    DIR		*dir = PTR_MLtoC(DIR, arg);

    rewinddir (dir);
    return ML_unit;

}

ML_unit_t closeDir (ml_state_t *msp, ML_directory_t arg)
{
    DIR		*dir = PTR_MLtoC(DIR, arg);

    closedir (dir);
    return ML_unit;

}

