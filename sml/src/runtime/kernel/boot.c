/* boot.c
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.
 *
 * This is the bootstrap loader for booting from .bin files.
 */

#include "ml-osdep.h"
#include <stdio.h>
#include "ml-base.h"
#include "ml-limits.h"
#include "cache-flush.h"
#include "bin-file.h"
#include "ml-objects.h"
#include "gc.h"
#include "ml-globals.h"

#ifndef SEEK_SET
#  define SEEK_SET	0
#endif

/** The names of the boot and binary file lists **/
PVT char	*FileLists[] = {
	"BOOTLIST", "PERVLIST", "BINLIST"
    };
#define NUM_FILE_LISTS	(sizeof(FileLists) / sizeof(char *))


pers_id_t	RunTimePerID = {RUNTIME_PERID};


/* The persistent ID list is stored in the PervStruct refcell.  It is a
 * list of (PerID, ML-object) pairs.
 */
#define PerIDList	(*PTR_MLtoC(ml_val_t, PervStruct))

PVT ml_val_t	BinFileList = LIST_nil;	/* A list of bin files to load */


/* local routines */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *binDir);
PVT FILE *OpenBinFile (const char *binDir, const char *fname, bool_t isBinary);
PVT void ReadBinFile (
    FILE *file, void *buf, int nbytes,
    const char *binDir, const char *fname
);
PVT void LoadBinFile (ml_state_t *msp, const char *binDir, const char *fname);
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj);
PVT ml_val_t LookupPerID (pers_id_t *perID);
PVT void ShowPerID (char *buf, pers_id_t *perID);


/* BootML:
 *
 * Boot the system using the .bin files from binDir.
 */
void BootML (const char *binDir, heap_params_t *heapParams)
{
    ml_state_t	*msp;
    char	*fname;

    msp = AllocMLState (TRUE, heapParams);

#ifdef HEAP_MONITOR
    if (HeapMon_Init(CmdLineArgs, msp->ml_heap) == FAILURE)
	Die("unable to start heap monitor");
#endif

    InitFaultHandlers ();
    AllocGlobals (msp);

  /* Enter the runtime system binding */
    EnterPerID (msp, &RunTimePerID, RunTimeCompUnit);

  /* construct the list of files to be loaded */
    BinFileList = BuildFileList (msp, binDir);

  /* boot the system */
    while (BinFileList != LIST_nil) {
	fname = PTR_MLtoC(char, LIST_hd(BinFileList));
	Say ("[Loading %s]\n", fname);
	BinFileList = LIST_tl(BinFileList);
	LoadBinFile (msp, binDir, fname);
    }

} /* end of BootML */


/* BuildFileList:
 *
 * Given the directory path, build a list of the .bin files in the
 * heap.
 */
PVT ml_val_t BuildFileList (ml_state_t *msp, const char *binDir)
{
    FILE	*listF;
    ml_val_t	fileNames[MAX_NUM_BOOT_FILES];
    int		i, j, numFiles;
    char	nameBuf[MAX_BOOT_PATH_LEN];
    ml_val_t	fileList;

    for (numFiles = 0, i = 0;  i < NUM_FILE_LISTS;  i++) {
	listF = OpenBinFile (binDir, FileLists[i], FALSE);
	if (listF == NULL)
	    continue;
      /* read in the file names, converting them to ML strings. */
	while (fgets (nameBuf, MAX_BOOT_PATH_LEN, listF) != NIL(char *)) {
	    j = strlen(nameBuf)-1;
	    if (nameBuf[j] == '\n') nameBuf[j] = '\0';	/* remove "\n" */
	    if (numFiles < MAX_NUM_BOOT_FILES)
		fileNames[numFiles++] = ML_CString(msp, nameBuf);
	    else
		Die ("too many files\n");
	}
	fclose (listF);
    }

  /* create the in-heap list */
    for (fileList = LIST_nil, i = numFiles;  --i >= 0; ) {
	LIST_cons(msp, fileList, fileNames[i], fileList);
    }

    return fileList;

} /* end of BuildFileList */


/* OpenBinFile:
 *
 * Open a file in the bin file directory.
 */
PVT FILE *OpenBinFile (const char *binDir, const char *fname, bool_t isBinary)
{
    char	path[MAX_BOOT_PATH_LEN];
    FILE	*file;

    sprintf(path, "%s%c%s", binDir, PATH_ARC_SEP, fname);

    if ((file = fopen(path, isBinary ? "rb" : "r")) == NULL)
	Error ("unable to open \"%s\"\n", path);

    return file;

} /* end of OpenBinFile */

/*
 * BINFILE FORMAT description:
 *
*************** The following really belongs in the header file ****************
 *  Every 4-byte integer field is stored in big-endian format.
 *
 *     Start Size Purpose
 * ----BEGIN OF HEADER----
 *          0 16  magic string
 *         16  4  number of import values (importCnt)
 *         20  4  number of exports (exportCnt = currently always 0 or 1)
 *         24  4  size of CM-specific info in bytes (cmInfoSzB)
 *         28  4  size of pickled lambda-expression in bytes (lambdaSzB)
 *         32  4  size of reserved area 1 in bytes (reserved1)
 *         36  4  size of reserved area 2 in bytes (reserved2)
 *         40  4  size of code area in bytes (codeSzB)
 *         44  4  size of pickled environment in bytes (envSzB)
 *         48  i  import trees [This area contains pickled import trees --
 *                  see below.  The total number of leaves in these trees is
 *                  importCnt.  The size impSzB of this area depends on the
 *                  shape of the trees.]
 *       i+48 ex  export pids [Each export pid occupies 16 bytes. Thus, the
 *                  size ex of this area is 16*exportCnt (0 or 16).]
 *    ex+i+48 cm  CM info [Currently a list of pid-pairs.] (cm = cmInfoSzB)
 * ----END OF HEADER----
 *          0  h  HEADER (h = 48+cm+ex+i)
 *          h  l  pickle of exported lambda-expr. (l = lambdaSzB)
 *        l+h  r  reserved areas (r = reserved1+reserved2)
 *      r+l+h  c  code area (c = codeSzB) [Structured into several
 *                  segments -- see below.]
 *    c+r+l+h  e  pickle of static environment (e = envSzB)
 *  e+c+r+l+h  -  END OF BINFILE
 *
 * IMPORT TREE FORMAT description:
 *
 *  The import tree area contains a list of (pid * tree) pairs.
 *  The pids are stored directly as 16-byte strings.
 *  Trees are constructed according to the following ML-datatype:
 *    datatype tree = NODE of (int * tree) list
 *  Leaves in this tree have the form (NODE []).
 *  Trees are written recursively -- (NODE l) is represented by n (= the
 *  length of l) followed by n (int * node) subcomponents.  Each component
 *  consists of the integer selector followed by the corresponding tree.
 *
 *  The size of the import tree area is only given implicitly. When reading
 *  this area, the reader must count the number of leaves and compare it
 *  with importCnt.
 *
 *  Integer values in the import tree area (lengths and selectors) are
 *  written in "packed" integer format. In particular, this means that
 *  Values in the range 0..127 are represented by only 1 byte.
 *  Conceptually, the following pickling routine is used:
 *
 *    void recur_write_ul (unsigned long l, FILE *file)
 *    {
 *        if (l != 0) {
 *            recur_write_ul (l >> 7, file);
 *            putc ((l & 0x7f) | 0x80, file);
 *        }
 *    }
 *
 *    void write_ul (unsigned long l, FILE *file)
 *    {
 *        recur_write_ul (l >> 7, file);
 *        putc (l & 0x7f, file);
 *    }
 *
 * CODE AREA FORMAT description:
 *
 *  The code area contains multiple code segements.  There will be at least
 *  two.  The very first segment is the "data" segment -- responsible for
 *  creating literal constants on the heap.  The idea is that code in the
 *  data segment will be executed only once at link-time. Thus, it can
 *  then be garbage-collected immediatly. (In the future it is possible that
 *  the data segment will not contain executable code at all but some form
 *  of bytecode that is to be interpreted separately.)
 *
 *  In the binfile, each code segment is represented by its size s (in
 *  bytes -- written as a 4-byte big-endian integer) followed by s bytes of
 *  machine- (or byte-) code. The total length of all code segments
 *  (including the bytes spent on representing individual sizes) is codeSzB.
 *
 * LINKING CONVENTIONS:
 *
 *  Linking is achieved by executing all code segments in sequential order.
 *
 *  The first code segment (i.e., the "data" segment) receives unit as
 *  its single argument.
 *
 *  The second code segment receives a record as its single argument.
 *  This record has (importCnt+1) components.  The first importCnt
 *  components correspond to the leaves of the import trees.  The final
 *  component is the result from executing the data segment.
 *
 *  All other code segments receive a single argument which is the result
 *  of the preceding segment.
 *
 *  The result of the last segment represents the exports of the compilation
 *  unit.  It is to be paired up with the export pid and stored in the
 *  dynamic environment.  If there is no export pid, then the final result
 *  will be thrown away.
 *
 *  The import trees are used for constructing the argument record for the
 *  second code segment.  The pid at the root of each tree is the key for
 *  looking up a value in the existing dynamic environment.  In general,
 *  that value will be a record.  The selector fields of the import tree
 *  associated with the pid are used to recursively fetch components of that
 *  record.
 */

/* ReadBinFile:
 */
PVT void ReadBinFile (
    FILE *file, void *buf, int nbytes, const char *binDir, const char *fname
)
{
    if (fread(buf, nbytes, 1, file) == -1)
	Die ("cannot read file \"%s%c%s\"", binDir, PATH_ARC_SEP, fname);

} /* end of ReadBinFile */

/* ReadPackedInt32:
 *
 * Read an integer in "packed" format.  (Small numbers only require 1 byte.)
 */
PVT Int32_t ReadPackedInt32 (FILE *file, const char *binDir, const char *fname)
{
    Unsigned32_t	n;
    Byte_t		c;

    n = 0;
    do {
	ReadBinFile (file, &c, sizeof(c), binDir, fname);
	n = (n << 7) | (c & 0x7f);
    } while ((c & 0x80) != 0);

    return ((Int32_t)n);

} /* end of ReadPackedInt32 */

/* ImportSelection:
 *
 * Select out the interesting bits from the imported object.
 */
PVT void ImportSelection (
    ml_state_t	*msp,
    FILE	*file,
    const char	*binDir,
    const char	*fname,
    int		*importVecPos,
    ml_val_t	tree)
{
    Int32_t cnt = ReadPackedInt32 (file, binDir, fname);
    if (cnt == 0) {
	ML_AllocWrite (msp, *importVecPos, tree);
	(*importVecPos)++;
    }
    else {
	while (cnt-- > 0) {
	    Int32_t selector = ReadPackedInt32 (file, binDir, fname);
	    ImportSelection (
		msp, file, binDir, fname, importVecPos,
		REC_SEL(tree, selector));
	}
    }

} /* end of ImportSelection */

/* LoadBinFile:
 */
PVT void LoadBinFile (ml_state_t *msp, const char *binDir, const char *fname)
{
    FILE	    *file;
    int		    i, exportSzB, remainingCode;
    bool_t	    isDataSeg;
    ml_val_t	    codeObj, importRec, closure, exportVal, val;
    binfile_hdr_t   hdr;
    pers_id_t	    exportPerID;
    Int32_t         thisSzB;

  /* open the file */
    file = OpenBinFile (binDir, fname, TRUE);
    if (file == NULL)
	Exit (1);

  /* get the header */
    ReadBinFile (file, &hdr, sizeof(binfile_hdr_t), binDir, fname);

  /* get header byte order right */
    hdr.importCnt	= BIGENDIAN_TO_HOST(hdr.importCnt);
    hdr.exportCnt	= BIGENDIAN_TO_HOST(hdr.exportCnt);
    hdr.importSzB	= BIGENDIAN_TO_HOST(hdr.importSzB);
    hdr.cmInfoSzB	= BIGENDIAN_TO_HOST(hdr.cmInfoSzB);
    hdr.lambdaSzB	= BIGENDIAN_TO_HOST(hdr.lambdaSzB);
    hdr.reserved1	= BIGENDIAN_TO_HOST(hdr.reserved1);
    hdr.reserved2	= BIGENDIAN_TO_HOST(hdr.reserved2);
    hdr.codeSzB		= BIGENDIAN_TO_HOST(hdr.codeSzB);
    hdr.envSzB		= BIGENDIAN_TO_HOST(hdr.envSzB);

  /* read the import PerIDs, and create the import vector */
    {
	int	importVecPos;

	if (NeedGC (msp, REC_SZB(hdr.importCnt + 1)))
	    InvokeGCWithRoots (msp, 0, &BinFileList, &exportVal, NIL(ml_val_t *));

	ML_AllocWrite (msp, 0, MAKE_DESC(hdr.importCnt + 1, DTAG_record));
	for (importVecPos = 1; importVecPos <= hdr.importCnt; ) {
	    pers_id_t	importPid;
	    ReadBinFile (file, &importPid, sizeof(pers_id_t), binDir, fname);
	    ImportSelection (
		msp, file, binDir, fname, &importVecPos,
		LookupPerID(&importPid));
	}
	ML_AllocWrite(msp, hdr.importCnt + 1, ML_nil);
	importRec = ML_Alloc(msp, hdr.importCnt + 1);
    }

  /* read the export PerID */
    if (hdr.exportCnt == 1) {
	exportSzB = sizeof(pers_id_t);
	ReadBinFile (file, &exportPerID, exportSzB, binDir, fname);
    }
    else if (hdr.exportCnt != 0)
	Die ("# of export pids is %d (should be 0 or 1)", (int)hdr.exportCnt);
    else
	exportSzB = 0;

  /* seek to code section */
    {
	long	    off = sizeof(binfile_hdr_t)
			+ hdr.importSzB
	                + exportSzB
	                + hdr.cmInfoSzB
			+ hdr.lambdaSzB
			+ hdr.reserved1 + hdr.reserved2;

	if (fseek(file, off, SEEK_SET) == -1)
	    Die ("cannot seek on bin file \"%s%c%s\"", binDir, PATH_ARC_SEP, fname);
    }

  /* Read code objects and run them.  We add a comment string to each code
   * object to mark which bin file it came from.  This code should be the
   * same as that in ../c-libs/smlnj-runtime/mkcode.c.
   */
    isDataSeg = TRUE;
    val = ML_nil;
    remainingCode = hdr.codeSzB;
    while (remainingCode > 0) {
	int		strLen = strlen(fname);
	int		padLen, extraLen;

      /* read the size for this code object */
	ReadBinFile (file, &thisSzB, sizeof(Int32_t), binDir, fname);
	thisSzB = BIGENDIAN_TO_HOST(thisSzB);

      /* We use one byte for the length, so the longest string is 255
       * characters.  We need padding so that the code + string +
       * length byte is WORD_SZB bytes.  The padding is inserted between
       * the code and the string.
       */
	if (strLen > 255)
	    strLen = 255;
	extraLen = strLen+1;  /* include byte for length */
	padLen = ROUNDUP(thisSzB+extraLen, WORD_SZB) - (thisSzB+extraLen);
	extraLen += padLen;

      /* how much more? */
	remainingCode -= thisSzB + sizeof(Int32_t);
	if (remainingCode < 0)
	    Die ("format error (code size mismatch) in bin file \"%s%c%s\"",
		binDir, PATH_ARC_SEP, fname);

      /* allocate space and read code object */
	codeObj = ML_AllocCode (msp, thisSzB+extraLen);
	ReadBinFile (file, PTR_MLtoC(void, codeObj), thisSzB, binDir, fname);

      /* tack on the bin-file name as a comment string. */
	memcpy (PTR_MLtoC(char, codeObj)+thisSzB+padLen, fname, strLen);
	*(PTR_MLtoC(Byte_t, codeObj)+thisSzB+extraLen-1) = (Byte_t)strLen;
	
	FlushICache (PTR_MLtoC(void, codeObj), thisSzB);
      
      /* create closure */
	REC_ALLOC1 (msp, closure, PTR_CtoML(PTR_MLtoC(ml_val_t, codeObj) + 1));

      /* apply the closure to the import PerID vector */
	if (isDataSeg) {
	    SaveCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
	    val = ApplyMLFn (msp, closure, val, TRUE);
	    RestoreCState (msp, &BinFileList, &importRec, NIL(ml_val_t *));
	    REC_SEL(importRec,hdr.importCnt) = val;
	    val = importRec;
	    isDataSeg = FALSE;
	}
	else {
	    SaveCState (msp, &BinFileList, NIL(ml_val_t *));
	    val = ApplyMLFn (msp, closure, val, TRUE);
	    RestoreCState (msp, &BinFileList, NIL(ml_val_t *));
	}

      /* do a GC, if necessary */
	if (NeedGC (msp, PERID_LEN+REC_SZB(5)))
	    InvokeGCWithRoots (msp, 0, &BinFileList, &val, NIL(ml_val_t *));
    }

  /* we are done: val -> exportVal */
    exportVal = val;

  /* record the resulting exported PerID */
    if (exportSzB != 0)
      EnterPerID (msp, &exportPerID, exportVal);

    fclose (file);

} /* end of LoadBinFile */

/* EnterPerID:
 *
 * Enter a PerID/object binding in the heap allocated list of PerIDs.
 */
PVT void EnterPerID (ml_state_t *msp, pers_id_t *perID, ml_val_t obj)
{
    ml_val_t	    mlPerID;

  /* Allocate a string for the PerID */
    mlPerID = ML_AllocString (msp, PERID_LEN);
    memcpy (PTR_MLtoC(char, mlPerID), (char *)perID, PERID_LEN);

  /* Allocate the list element */
    REC_ALLOC3(msp, PerIDList, mlPerID, obj, PerIDList);

}

/* LookupPerID:
 */
PVT ml_val_t LookupPerID (pers_id_t *perID)
{
    ml_val_t        p;

    for (p = PerIDList;  p != ML_unit;  p = REC_SEL(p, 2)) {
	if (memcmp((char *)perID, REC_SELPTR(char, p, 0), PERID_LEN) == 0)
	    return (REC_SEL(p, 1));
    }

    {
	char	buf[64];
	ShowPerID (buf, perID);
	Die ("unable to find PerID %s", buf);
    }

} /* end of LookupPerID */


/* ShowPerID:
 */
PVT void ShowPerID (char *buf, pers_id_t *perID)
{
    char	*cp = buf;
    int		i;

    *cp++ = '[';
    for (i = 0;  i < PERID_LEN;  i++) {
	sprintf (cp, "%02x", perID->bytes[i]);
	cp += 2;
    }
    *cp++ = ']';
    *cp++ = '\0';

} /* end of ShowPerID */

