/* file-io.c
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies.
 *
 * File I/O support for the SML'97 Basis on Unix.
 */

#include "ml-base.h"
#include "ml-unixdep.h"
#include <unistd.h>
#include <fcntl.h>
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "sml-basis.h"


/* openFile:
 */
ML_iodesc_t openFile (ml_state_t *msp, ML_string_t s, int flgs)
{
    int		flags;
    int		fd;

  /* get flags */
    switch (flgs & 0x3) {
      case OPEN_RD:	flags = O_RDONLY; break;
      case OPEN_WR:	flags = O_WRONLY; break;
      case OPEN_RDWR:	flags = O_RDWR; break;
      default:		return RAISE_ERROR(msp, "openFile: bogus flags");
    }
    if (flgs & OPEN_CREATE) flags |= O_CREAT;
    if (flgs & OPEN_TRUNC) flags |= O_TRUNC;
    if (flgs & OPEN_APPEND) flags |= O_APPEND;

    fd = open (STR_MLtoC(s), flags, 0666);
    /* SayDebug("openFile(%s) = %d; flgs = %d\n", STR_MLtoC(s), fd, flgs); */

    CHK_RETURN(msp, fd)

} /* end of openFile */

/* closeFile:
 */
void closeFile (ML_iodesc_t iod)
{
    close(INT_MLtoC(iod));

} /* end of closeFile */

/* cmpIODesc:
 */
int cmpIODesc (ML_iodesc_t iod1,ML_iodesc_t iod2)
{
    return (iod1 - iod2);

} /* end of cmpIODesc */

/* readTextVec:
 */
ML_charvec_opt_t readTextVec (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t iod,
    int		nbytes)
{
    int			fd = INT_MLtoC(iod);
    ml_val_t		vec, hdr, res;
    int			n;

    if (nbytes == 0){
	OPTION_SOME (msp, res, ML_string0);
	return res;
    }

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS(nbytes));
    n = read (fd, PTR_MLtoC(char, vec), nbytes);
    if (n < 0)
	return RAISE_SYSERR(msp, n);
    else if (n == 0) {
	OPTION_SOME (msp, res, ML_string0);
	return res;
    }

    if (n < nbytes) {
      /* we need to shrink the vector */
	ML_ShrinkRaw32 (msp, vec, BYTES_TO_WORDS(n));
    }

    SEQHDR_ALLOC (msp, hdr, DESC_string, vec, n);
    OPTION_SOME (msp, res, hdr);

    return res;

} /* end of readTextVec */

/* readTextArr:
 */
ML_int_t readTextArr (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t	iod,
    ML_chararr_t arr,
    int		nbytes,
    int		offset)
{
    int		fd = INT_MLtoC(iod);
    char	*start = STR_MLtoC(arr) + offset;
    int		n;

    n = read (fd, start, nbytes);

    CHK_RETURN (msp, n)

} /* end of readTextArr */

/* writeTextVec:
 */
ML_int_t writeTextVec (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t iod,
    ML_charvec_t buf,
    int		offset,
    int		nbytes)
{
    int		fd = INT_MLtoC(iod);
    char	*start = STR_MLtoC(buf) + offset;
    ssize_t    	n;

    n = write (fd, start, nbytes);

    CHK_RETURN (msp, n)

} /* end of writeTextVec */

/* writeTextArr:
 */
ML_int_t writeTextArr (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t	iod,
    ML_chararr_t buf,
    int		offset,
    int		nbytes)
{
    int		fd = INT_MLtoC(iod);
    char	*start = STR_MLtoC(buf) + offset;
    ssize_t    	n;

    n = write (fd, start, nbytes);

    CHK_RETURN (msp, n)

} /* end of writeTextArr */

/* readBinVec:
 */
ML_word8vec_t readBinVec (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t	iod,
    int		nbytes)
{
    int		fd = INT_MLtoC(iod);
    ml_val_t	    vec, hdr, res;
    int		    n;

    /* SayDebug("readBinVec: iod = %d, nbytes = %d\n", fd, nbytes); */
    if (nbytes == 0){
	OPTION_SOME (msp, res, ML_string0);
	return res;
    }

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS(nbytes));
    /* SayDebug("  vec = %p\n", PTR_MLtoC(void, vec)); */
    n = read (fd, PTR_MLtoC(void, vec), (size_t)nbytes);
    /* SayDebug("  %d bytes read\n", n); */
    if (n < 0)
	return RAISE_SYSERR(msp, n);
    else if (n == 0) {
	OPTION_SOME (msp, res, ML_string0);
	return res;
    }

    if (n < nbytes) {
      /* we need to shrink the vector */
	ML_ShrinkRaw32 (msp, vec, BYTES_TO_WORDS(n));
    }

    SEQHDR_ALLOC (msp, hdr, DESC_word8vec, vec, n);
    OPTION_SOME (msp, res, hdr);

    return res;

} /* end of readBinVec */

/* readBinArr:
 */
ML_int_t readBinArr (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t	iod,
    ML_word8arr_t arr,
    int		nbytes,
    int		offset)
{
    int		fd = INT_MLtoC(iod);
    char	*start = STR_MLtoC(arr) + offset;
    int		n;

    n = read (fd, start, nbytes);

    CHK_RETURN (msp, n)

} /* end of readBinArr */

/* writeBinVec:
 */
ML_int_t writeBinVec (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t	iod,
    ML_word8vec_t buf,
    int		offset,
    int		nbytes)
{
    int		fd = INT_MLtoC(iod);
    ssize_t    	n;

    n = write (fd, STR_MLtoC(buf), nbytes);

    CHK_RETURN (msp, n)

} /* end of writeBinVec */

/* writeBinArr:
 */
ML_int_t writeBinArr (
    ml_state_t	*msp,
    ML_bool_t	noblock,
    ML_iodesc_t	iod,
    ML_word8arr_t buf,
    int		offset,
    int		nbytes)
{
    int		fd = INT_MLtoC(iod);
    char	*start = STR_MLtoC(buf) + offset;
    ssize_t    	n;

    n = write (fd, start, nbytes);

    CHK_RETURN (msp, n)
  
} /* end of writeBinArr */

/* getPos:
 */
ML_int32_t getPos (ml_state_t *msp, ML_iodesc_t iod)
{
    int		fd = INT_MLtoC(iod);
    off_t	sts;

    sts = lseek (fd, 0, SEEK_CUR);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else {
	ml_val_t	pos;
	INT32_ALLOC(msp, pos, (int)sts);
	return pos;
    }

} /* end of getPos */

/* setPos:
 */
ML_unit_t setPos (ml_state_t *msp, ML_iodesc_t iod, ML_int32_t offset, int whence)
{
    int		fd = INT_MLtoC(iod);
    off_t	sts;

    switch (whence) {
      case SET_POS_BEGIN:	whence = SEEK_SET; break;
      case SET_POS_CUR:		whence = SEEK_CUR; break;
      case SET_POS_END:		whence = SEEK_END; break;
      default: Die("bogus whence");
    }

    /* SayDebug("setPos: iod=%d, offset=%d\n", fd, INT32_MLtoC(offset)); */
    sts = lseek (fd, INT32_MLtoC(offset), whence);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else
	return ML_unit;

} /* end of setPos */

/* getStdIn:
 */
ML_iodesc_t getStdIn ()
{
    return INT_CtoML(0);

} /* end of getStdIn */

/* getStdOut:
 */
ML_iodesc_t getStdOut ()
{
    return INT_CtoML(1);

} /* end of getStdOut */

/* getStdErr:
 */
ML_iodesc_t getStdErr ()
{
    return INT_CtoML(2);

} /* end of getStdErr */
