/* file-sys.c
 *
 * COPYRIGHT (c) 2001 Bell labs, Lucent Technologies.
 *
 * Support for file-system operations in the SML'97 Basis.
 */

#include "ml-unixdep.h"
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#include <stdio.h>
#include <utime.h>
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "sml-basis.h"

/* chDir:
 */
ML_unit_t chDir (ml_state_t *msp,idl_string path)
{
    int			sts;

    sts = chdir(path);

    CHK_RETURN_UNIT(msp, sts)

} /* end of chDir */

/* getDir:
 */
ML_string_t getDir (ml_state_t *msp)
{
    char		path[MAXPATHLEN];
    char		*sts;

    sts = getcwd(path, MAXPATHLEN);

    if (sts != NIL(char *))
	return ML_CString (msp, path);
    else if (errno != ERANGE)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t    p;
	int         buflen;
	char        *buf;

	buflen = 2*MAXPATHLEN;
	buf = MALLOC(buflen);
	if (buf == NIL(char*))
	    return RAISE_ERROR(msp, "no malloc memory");

	while ((sts = getcwd(buf, buflen)) == NIL(char *)) {
            FREE (buf);
            if (errno != ERANGE)
		return RAISE_SYSERR(msp, sts);
            else {
        	buflen = 2*buflen;
        	buf = MALLOC(buflen);
        	if (buf == NIL(char*))
		    return RAISE_ERROR(msp, "no malloc memory");
            }
	}

	p = ML_CString (msp, buf);
	FREE (buf);

	return p;
    }

} /* end of getDir */

/* mkDir:
 */
ML_unit_t mkDir (ml_state_t *msp, idl_string path)
{
    int			sts;

    sts = mkdir (path, 0777);

    CHK_RETURN_UNIT(msp, sts)

} /* end of mkDir */

/* rmDir:
 */
ML_unit_t rmDir (ml_state_t *msp, idl_string path)
{
    int			sts;

    sts = rmdir(path);

    CHK_RETURN_UNIT(msp, sts)

} /* end of rmDir */

/* isReg:
 */
ML_bool_t isReg (ml_state_t *msp, idl_string path)
{
    struct stat		st;
    int			sts;

    if ((sts = stat(path, &st)) < 0)	return RAISE_SYSERR(msp, sts);
    else if (S_ISREG(st.st_mode))	return ML_true;
    else				return ML_false;

} /* end of isReg */

/* isDir:
 */
ML_bool_t isDir (ml_state_t *msp, idl_string path)
{
    struct stat		st;
    int			sts;

    if ((sts = stat(path, &st)) < 0)	return RAISE_SYSERR(msp, sts);
    else if (S_ISDIR(st.st_mode))	return ML_true;
    else				return ML_false;

} /* end of isDir */

/* isLink:
 */
ML_bool_t isLink (ml_state_t *msp, idl_string path)
{
    struct stat		st;
    int			sts;

    if ((sts = stat(path, &st)) < 0)	return RAISE_SYSERR(msp, sts);
#ifdef S_ISLNK
    else if (S_ISLNK(st.st_mode))	return ML_true;
#endif
    else				return ML_false;

} /* end of isLink */

/* readLink:
 *
 * Read the value of a symbolic link.
 *
 * The following implementation assumes that the system readlink
 * fills the given buffer as much as possible, without nul-termination,
 * and returns the number of bytes copied. If the buffer is not large
 * enough, the return value will be at least the buffer size. In that
 * case, we find out how big the link really is, allocate a buffer to
 * hold it, and redo the readlink.
 *
 * Note that the above semantics are not those of POSIX, which requires
 * null-termination on success, and only fills the buffer up to as most 
 * the penultimate byte even on failure.
 */
ML_string_t readLink (ml_state_t *msp, idl_string path)
{
    char	buf[MAXPATHLEN];
    int         len;

    len = readlink(path, buf, MAXPATHLEN);

    if (len < 0)
	return RAISE_SYSERR(msp, len);
    else if (len < MAXPATHLEN) {
	buf[len] = '\0';
	return ML_CString (msp, buf);
    }
    else {  /* buffer not big enough */
	char         *nbuf;
	ml_val_t     obj;
	struct stat  sbuf;
	int          res;
	int          nlen;

      /* Determine how big the link text is and allocate a buffer */
	res = lstat (path, &sbuf);
	if (res < 0)
	    return RAISE_SYSERR(msp, res);
	nlen = sbuf.st_size + 1;
	nbuf = MALLOC(nlen);
	if (nbuf == 0)
	    return RAISE_ERROR(msp, "out of malloc memory");

        /* Try the readlink again. Give up on error or if len is still bigger
         * than the buffer size.
         */
	len = readlink(path, buf, len);
	if (len < 0)
	    return RAISE_SYSERR(msp, len);
	else if (len >= nlen)
	    return RAISE_ERROR(msp, "readlink failure");

	nbuf[len] = '\0';
	obj = ML_CString (msp, nbuf);
	FREE (nbuf);
	return obj;
    }

} /* end of readLink */

/* fileSize:
 */
ML_int32_t fileSize (ml_state_t *msp, idl_string path)
{
    int		sts;
    struct stat	st;
    ml_val_t	res;

    if ((sts = stat(path, &st)) < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	INT32_ALLOC (msp, res, (int)st.st_size);
	return res;
    }

} /* end of fileSize */

/* modTime:
 */
ML_int32_t modTime (ml_state_t *msp, idl_string path)
{
    struct stat		st;
    int			sts;

    if ((sts = stat(path, &st)) < 0)	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	t;
	INT32_ALLOC(msp, t, st.st_mtime);
	return t;
    }

} /* end of modTime */

/* setTime:
 */
ML_unit_t setTime (ml_state_t *msp, idl_string path, Time_t *t)
{
    int		sts;

    if (t == NIL(Time_t *))
      /* set access and modification times to current time */
	sts = utime (path, NIL(struct utimbuf *));
    else {
	struct utimbuf	tb;

	tb.actime = t->seconds;
	tb.modtime = t->seconds;
	sts = utime(path, &tb);
    }

    CHK_RETURN_UNIT(msp, sts);

} /* end of setTime */

/* remove:
 */
ML_unit_t removeFile (ml_state_t *msp, idl_string path)
{
    int		sts;

    sts = unlink (path);

    CHK_RETURN_UNIT(msp, sts);

} /* end of remove */

/* rename:
 */
ML_unit_t renameFile (ml_state_t *msp, idl_string old, idl_string new)
{
    int		sts;

    sts = rename (old, new);

    CHK_RETURN_UNIT (msp, sts);

} /* end of rename */

/* fileAccess:
 */
ML_bool_t fileAccess (ml_state_t *msp, idl_string path, int mode)
{
    int		m = F_OK;

    if (mode & A_READ) m |= R_OK;
    if (mode & A_WRITE) m |= W_OK;
    if (mode & A_EXEC) m |= X_OK;

    if (access (path, m) == 0)
	return ML_true;
    else if ((errno == EACCES) || (errno == ENOENT) || (errno == ENOTDIR) || (errno == EROFS))
	return ML_false;
    else
	return RAISE_SYSERR(msp, -1);

} /* end of fileAccess */

/* tmpName:
 */
ML_string_t tmpName (ml_state_t *msp)
{
    char        buf[L_tmpnam];

    tmpnam (buf);

    return ML_CString (msp, buf);

} /* end of tmpName */

/* fileId:
 */
ML_word8vec_t fileId (ml_state_t *msp, idl_string path)
{
    struct stat		st;
    struct fid {
	ino_t	ino;
	dev_t	dev;
    }			fid;
    int			sts;
    ml_val_t		data, res;

    if ((sts = stat(path, &st)) < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	fid.ino = st.st_ino;
	fid.dev = st.st_dev;
	data = ML_CData (msp, &fid, sizeof(struct fid));
	SEQHDR_ALLOC (msp, res, DESC_word8vec, data, sizeof(struct fid));
	return res;
    }

} /* end of fileId */

/* ioDescKind:
 */
ML_int_t ioDescKind (ml_state_t *msp, ML_iodesc_t iod)
{
    int			fd = INT_MLtoC(iod);
    struct stat		st;
    int			sts;

    sts = fstat (fd, &st);

    if (sts < 0)			return RAISE_SYSERR(msp, sts);
    else if (S_ISREG(st.st_mode))	return INT_CtoML(IOD_KIND_FILE);
    else if (S_ISDIR(st.st_mode))	return INT_CtoML(IOD_KIND_DIR);
    else if (S_ISCHR(st.st_mode))	return INT_CtoML(IOD_KIND_TTY);
    else if (S_ISBLK(st.st_mode))	return INT_CtoML(IOD_KIND_DEVICE);
    else if (S_ISFIFO(st.st_mode))	return INT_CtoML(IOD_KIND_PIPE);
#ifdef S_ISLNK
    else if (S_ISLNK(st.st_mode))	return INT_CtoML(IOD_KIND_SYMLINK);
#endif
#ifdef S_ISSOCK
    else if (S_ISSOCK(st.st_mode))	return INT_CtoML(IOD_KIND_SOCKET);
#endif
    else return RAISE_ERROR(msp, "ioDescKind: unknown file type");

} /* end of ioDescKind */
