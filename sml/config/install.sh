#!/bin/sh
#
# Copyright (c) 1994 AT&T Bell Laboratories.
#
# Installation script for SML/NJ and related tools; this is a temporary
# placeholder until the configuration tool is finished.
#
# Significant changes to accommodate (and take advantage of) the new CM
# by M.Blume (2/2000).
#

this=$0

#
# get the target list
#
if [ -r config/targets.customized ] ; then
    . config/targets.customized
elif [ ! -r config/targets ]; then
    echo "$this: !!! File config/targets is missing."
    exit 1
else
    . config/targets
fi

#
# create the preloads.standard file
#
if [ ! -r config/preloads ]; then
    echo "$this: !!! File config/preloads is missing."
    exit 1
fi
cp config/preloads preloads.standard

#
# Some OSs have make in strange places, but most of the time it is
# simply on the PATH:
#
MAKE=make

#
# Make sure we don't have any unpleasant surprises due to the installing
# user's process environment:
#
unset CM_PATHCONFIG

SHELL=/bin/sh
echo $this: Using shell $SHELL.

#
# set the SML root directory
#
REAL_PWD=`pwd`
ROOT=${PWD:-$REAL_PWD}
echo $this: SML root is $ROOT.
echo $this: Installation directory is ${INSTALLDIR:=$ROOT}.

#
# set the various directory and file pathname variables
#
BINDIR=$INSTALLDIR/bin		# main dir for binary stuff
CONFIGDIR=$ROOT/config
HEAPDIR=$BINDIR/.heap		# where heap images live
RUNDIR=$BINDIR/.run		# where executables (i.e., the RTS) live
SRCDIR=$ROOT/src		# where the source tree is rooted
LIBDIR=$INSTALLDIR/lib		# where libraries live
LIBLIST=$ROOT/liblist		# list of commands to stabilize libraries
LATESTANDALONES=$ROOT/latestandalones # standalone programs to be built late
LIBMOVESCRIPT=$ROOT/libmove	# a temporary script
LOCALPATHCONFIG=$INSTALLDIR/pathconfig # a temporary pathconfig file

URLGETTER=unknown

#
# The path to the dir where ml-yacc, ml-burg, ml-lex, ml-build, and
# ml-makedepend live.  This path will be interpreted relative to $LIBDIR.
#
TOOLDIR=../bin

#
# A temporary file for post-editing the pathconfig file...
#
PCEDITTMP=$INSTALLDIR/pcedittmp.$$

#
# files to be deleted after we are done...
#
tmpfiles=""
tmpfiles="$tmpfiles $ROOT/preloads.standard"
tmpfiles="$tmpfiles $LIBLIST"
tmpfiles="$tmpfiles $LATESTANDALONES"
tmpfiles="$tmpfiles $LOCALPATHCONFIG"
tmpfiles="$tmpfiles $LIBMOVESCRIPT"
tmpfiles="$tmpfiles $PCEDITTMP"
#
# make sure we always clean up after ourselves...
#
trap 'rm -f $tmpfiles' 0 1 2 3 15


#
# set the CM configuration variables (these are environment variables
# that will be queried by the bootstrap code)
# Especially important is CM_PATHCONFIG_DEFAULT.
#
CM_PATHCONFIG_DEFAULT=$LIBDIR/pathconfig
export CM_PATHCONFIG_DEFAULT

#
# the release version that we are installing
#
VERSION=`cat $CONFIGDIR/version`
echo $this: Installing version $VERSION.

#
# the URL for the (usually remote) source archive
#
SRCARCHIVEURL=`cat $CONFIGDIR/srcarchiveurl`
echo $this: URL of source archive is $SRCARCHIVEURL.

#
# Function to make a directory including its ancestors.
#
makedir() {
    if [ x$1 = x ] ; then
	:
    elif [ -d $1 ] ; then
	:
    else
	makedir `dirname $1`
	echo "$this: Making directory $1"
	if mkdir $1 ; then
	    :
	else
	    echo "$this: !!! Unable to make directory $1!"
	    exit 1
	fi
    fi
}

#
# Function for asking user to fetch source archive.
#   $1 - descriptive name
#   $2 - base name without extension, without version, and without dir
#   $3 - remote directory
#
askurl() {
    echo "$this: Please, fetch $1 archive"
    echo ' ('$2.'*' or $VERSION-$2.'*)'
    echo " from $3"
    echo " and then re-run this script!"
    exit 1
}

#
# Function for fetching source archives automatically using wget or lynx.
#   $1 - command to actually get the stuff
#   $2 - descriptive name
#   $3 - base name without extension and without dir
#   $4 - remote directory
#
fetchurl() {
    getter=$1 ; shift
    echo $this: Fetching $1 from $3. Please stand by...
    fetched=no
    for base in $2 $VERSION-$2 ; do
	for ext in tar.gz tgz tar.Z tz tar tar.bz2 ; do
	    try=$base.$ext
	    echo $this: Trying $try ...
	    if $getter $3 $try $ROOT/$try ; then
		fetched=yes
		echo $this: Fetching $try was a success.
		break 2		# get out of both for-loops
	    else
		rm -f $ROOT/$try
	    fi
	done
    done
    if [ $fetched = no ] ; then
	echo $this: Fetching $try was no success.
	echo '  ' You should try to do it manually now.
	askurl "$1" "$2" "$3"
    fi
}

usewget() {
    wget -nv -O $3 $1/$2
}

uselynx() {
    lynx -source $1/$2 >$3
}

usecurl() {
    curl -s $1/$2 >$3
}

testurlgetter() {
    (exec >/dev/null 2>&1 ; exec $*)
}

#
# Function to check whether wget or lynx is available.
# Set URLGETTER accordingly.
#
urlgetter() {
    if [ "$URLGETTER" = unknown ] ; then
	if testurlgetter wget --help ; then
	    URLGETTER="fetchurl usewget"
	elif testurlgetter curl --help ; then
	    URLGETTER="fetchurl usecurl"
	elif testurlgetter lynx -help ; then
	    URLGETTER="fetchurl uselynx"
	else
	    URLGETTER="askurl"
	fi
    fi
}

un_tar() {
    echo "$this: Un-TAR-ing $1 archive."
    tar -xf $2
}

un_tar_Z() {
    echo "$this: Un-COMPRESS-ing and un-TAR-ing $1 archive."
    zcat $2 | tar -xf -
}

un_tar_gz() {
    echo "$this: Un-GZIP-ing and un-TAR-ing $1 archive."
    gunzip -c $2 | tar -xf -
}

un_tar_bz2() {
    echo "$this: Un-BZIP2-ing and un-TAR-ing $1 archive."
    bunzip2 -c $2 | tar -xf -
}

unarchive() {
    # $1: descriptive string, $2: archive, $3: unpacker
    if [ -r $ROOT/$2 ] ; then
	$3 "$1" $ROOT/$2
    elif [ -r $ROOT/$VERSION-$2 ]; then
	$3 "$1" $ROOT/$VERSION-$2
    else
	return 1
    fi
}

#
# Function to unpack a source archive.
#
# $1: descriptive name of the sources to be unpacked
# $2: the directory into which to unpack the sources
# $3: the sub-directory of $2 that is going to be created by unpacking
# $4: the basename of the source archive (the script will check several
#     different suffixes to determine what kind of de-compression is to
#     be used)
#
# fetch_n_unpack is the helper function that does the real work.  If
# no archive is found locally, it invokes $URLGETTER and tries again.
# The variable $tryfetch is used to make sure this happens only once.
fetch_n_unpack() {
    cd $2
    if unarchive "$1" $4.tar.gz un_tar_gz ||
       unarchive "$1" $4.tgz un_tar_gz ||
       unarchive "$1" $4.tar.Z un_tar_Z ||
       unarchive "$1" $4.tar un_tar ||
       unarchive "$1" $4.tar.bz1 un_tar_bz2 ||
       unarchive "$1" $4.tz un_tar_Z
    then
	: we are done
    elif [ $tryfetch = yes ] ; then
	urlgetter
	$URLGETTER "$1" $4 $SRCARCHIVEURL
	tryfetch=no
	fetch_n_unpack "$1" "$2" "$3" "$4"
    fi
}

#
# The main "unpack" driver function that invokes the above helper.
#
unpack() {
    tryfetch=yes
    if [ -d $2/$3 ]; then
	echo "$this: The $1 tree already exists."
    else
	fetch_n_unpack "$1" "$2" "$3" "$4"
    fi
    if [ ! -d $2/$3 ]; then
	echo "$this: !!! Unable to unpack $1 archive."
	exit 1
    fi
}

# A function to move all stable library files to a parallel directory
# hierarchy.
# The first argument must be a simple path (no / inside), and
# the second argument must be an absolute path.
move() {
    if [ -d $1 ] ; then
	if [ ! -d $2 ] ; then
	    if [ -f $2 ] ; then
		echo $this: $2 exists as a non-directory.
		exit 1
	    fi
	    mkdir $2
	fi
	cd $1
	for i in * ; do
	    move $i $2/$i
	done
	cd ..
    elif [ -f $1 ] ; then
	rm -f $2
	mv $1 $2
    fi
}

# move stable library file from $1 to $2
movelib()
{
    SOURCE=$1
    TARGET=$2
    TARGETDIR=`dirname ${TARGET}`
    if [ ! -d ${TARGETDIR} ] ; then
	makedir ${TARGETDIR}
    fi
    mv ${SOURCE} ${TARGET}
}

# A shell function that registers a library for being built.
# This function takes 3 arguments:
#   $1 = controlling anchor
#   $2 = name relative to controlling anchor
#   $3 = dir (relative to ${SRCDIR}) corresponding to $1
#
# This works by adding ML code to file $LIBLIST.  The code in this file
# will be executed near the end of this script.  If $MOVE_LIBRARIES is
# set to true, then reglib will also register a "movelib" to be executed at
# the end by putting a "movelib" line into $LIBMOVESCRIPT.
reglib() {
    ANCHOR=$1
    RELNAME=$2
    LIBNAME='$'${ANCHOR}/${RELNAME}
    ADIR=${SRCDIR}/$3
    RELDIR=`dirname $RELNAME`
    RELBASE=`basename $RELNAME`
    if [ x$RELDIR = x. ] ; then
	RELDIR=
    else
	RELDIR=/$RELDIR
    fi
    RELLOC=${RELDIR}/CM/${ARCH}-unix/${RELBASE}
    SRCFINALLOC=${ADIR}${RELLOC}
    if [ x$MOVE_LIBRARIES = xtrue ] ; then
	FINALLOC=${LIBDIR}/${ANCHOR}${RELLOC}
	FINALCONFIGPATH=${ANCHOR}
    else
	FINALLOC=${SRCFINALLOC}
	FINALCONFIGPATH=${ADIR}
    fi
    
    if [ -f ${FINALLOC} ] ; then
	echo "$this: Library ${LIBNAME} already exists in ${FINALLOC}"
    else
	echo "$this: Scheduling library ${LIBNAME} to be built as ${FINALLOC}."
	echo "  andalso CM.stabilize false \"${LIBNAME}\"" >>${LIBLIST}
	echo ${ANCHOR} ${ADIR} >>${LOCALPATHCONFIG}
	if [ x$MOVE_LIBRARIES = xtrue ] ; then
	    echo movelib ${SRCFINALLOC} ${FINALLOC} >>${LIBMOVESCRIPT}
	fi
    fi
    echo ${ANCHOR} ${FINALCONFIGPATH} >>${CM_PATHCONFIG_DEFAULT}
}

#
# Function to build a standalone program such as ml-yacc.  The function takes
# 2 or 3 arguments.  First the name of the program which at the same time
# is the directory name under $SRCDIR where the sources reside.  The second
# argument is a descriptive name for the program (passed on to "unpack").
# The optional third argument specifies the path relative to $SRCDIR/$1
# of the directory where the program's heap image is to be found.
#

standalone() {
    TARGET=$1.$HEAP_SUFFIX
    if [ $# = 3 ] ; then
	TARGETLOC=$3/$TARGET
    else
	TARGETLOC=$TARGET
    fi
    if [ -r $HEAPDIR/$TARGET ] ; then
	echo $this: Target $TARGET already exists.
    else
	echo $this: Building $TARGET.
	unpack $2 $SRCDIR $1 $1
	cd $SRCDIR/$1
	# build it, but make sure we don't pick up some (unrelated)
	# local path configuration...
	CM_LOCAL_PATHCONFIG=/dev/null ./build
	if [ -r $TARGETLOC ] ; then
	    mv $TARGETLOC $HEAPDIR/$TARGET
	    if [ ! -f $BINDIR/$1 ] ; then
		cd $BINDIR
		ln -s .run-sml $1
	    fi
	else
	    echo "$this: !!! Build of $TARGET failed."
	fi
    fi
}

#
# create the various sub directories
#
for dir in $BINDIR $HEAPDIR $RUNDIR $LIBDIR $SRCDIR ; do
    makedir $dir
done

#
# install the script that tests the architecture, and make sure that it works
#
if [ -x $BINDIR/.arch-n-opsys ]; then
    echo $this: Script $BINDIR/.arch-n-opsys already exists.
else
    cat $CONFIGDIR/_arch-n-opsys \
    | sed -e "s,@SHELL@,$SHELL,g" > $BINDIR/.arch-n-opsys
    chmod 555 $BINDIR/.arch-n-opsys
    if [ ! -x $BINDIR/.arch-n-opsys ]; then
	echo "$this: !!! Installation of $BINDIR/.arch-n-opsys failed."
	exit 1
    fi
fi

ARCH_N_OPSYS=`$BINDIR/.arch-n-opsys`
if [ "$?" != "0" ]; then
    echo "$this: !!! Script $BINDIR/.arch-n-opsys fails on this machine."
    echo "$this: !!! You must patch this by hand and repeat the installation."
    exit 2
else
    echo $this: Script $BINDIR/.arch-n-opsys reports $ARCH_N_OPSYS.
fi
eval $ARCH_N_OPSYS

#
# Function to install a "driver" script...
#   This takes care of patching the source of the script with the SHELL,
#   BINDIR, and VERSION variables to use.
#
installdriver() {
    dsrc=$1
    ddst=$2
# We install the driver unconditionally. (It would be better to test
# for an outdated driver script, but not all "test" commands understand
# the -nt comparison operator....)
#   if [ -x $BINDIR/$ddst ]; then
#	echo $this: Script $BINDIR/$ddst already exists.
#   else
	rm -f $BINDIR/$ddst
	cat $CONFIGDIR/$dsrc | \
	sed -e "s,@SHELL@,$SHELL,g" \
	    -e "s,@BINDIR@,$BINDIR," \
	    -e "s,@VERSION@,$VERSION," \
	    > $BINDIR/$ddst
	chmod 555 $BINDIR/$ddst
	if [ ! -x $BINDIR/$ddst ]; then
	    echo "$this: !!! Installation of $BINDIR/${ddst} failed."
	    exit 1
	fi
#   fi
}

installdriver _run-sml .run-sml
installdriver _link-sml .link-sml
installdriver _ml-build ml-build
installdriver _ml-makedepend ml-makedepend

#
# set some architecture dependent run-time system flags
#
case $ARCH in
    mips*)
	ALLOC=1M
	;;
    x86)
	# The following is the _wrong_ value for many popular x86 chips
	# (i.e., Celerons).  However, the optimal value for those is 32k,
	# and such a small value is not enough for the runtime system's boot
	# code.  Therefore, we use 256k here and re-set it to the proper
	# value in .run-sml.
	ALLOC=256k
	;;
    alpha32)
	ALLOC=512k
	;;
    *)
	ALLOC=512k
	;;
esac

case $OPSYS in
    solaris)
	MAKE=/usr/ccs/bin/make
	;;
    linux)
	EXTRA_DEFS=`$CONFIGDIR/chk-global-names.sh`
	if [ "$?" != "0" ]; then
	    echo "$this: !!! Problems checking for underscores in asm names."
	    exit 1
	fi
	EXTRA_DEFS="XDEFS=$EXTRA_DEFS"
	;;
esac

#
# the name of the bin files directory
#
BOOT_ARCHIVE=boot.$ARCH-unix
BOOT_FILES=sml.$BOOT_ARCHIVE

#
# build the run-time system
#
unpack "run-time" $SRCDIR runtime runtime
if [ -x $RUNDIR/run.$ARCH-$OPSYS ]; then
    echo $this: Run-time system already exists.
else
    cd $SRCDIR/runtime/objs
    echo $this: Compiling the run-time system.
    $MAKE -f mk.$ARCH-$OPSYS $EXTRA_DEFS
    if [ -x run.$ARCH-$OPSYS ]; then
	mv run.$ARCH-$OPSYS $RUNDIR
	$MAKE MAKE=$MAKE clean
    else
	echo "$this: !!! Run-time system build failed for some reason."
	exit 1
    fi
fi
cd $SRCDIR

#
# boot the base SML system
#
if [ -r $HEAPDIR/sml.$HEAP_SUFFIX ]; then
    echo $this: Heap image $HEAPDIR/sml.$HEAP_SUFFIX already exists.
else
    unpack bin $ROOT $BOOT_FILES $BOOT_ARCHIVE
    cd $ROOT/$BOOT_FILES
    if $BINDIR/.link-sml @SMLheap=$ROOT/sml @SMLboot=BOOTLIST @SMLalloc=$ALLOC
    then
	cd $ROOT
	if [ -r sml.$HEAP_SUFFIX ]; then
	    mv sml.$HEAP_SUFFIX $HEAPDIR
	    cd $BINDIR
	    ln -s .run-sml sml
	    #
	    # Now move all stable libraries to #LIBDIR and generate
	    # the pathconfig file.
	    #
	    cd $ROOT/$BOOT_FILES
	    for anchor in * ; do
		if [ -d $anchor ] ; then
		    echo $anchor $anchor >>$CM_PATHCONFIG_DEFAULT
		    move $anchor $LIBDIR/$anchor
		fi
	    done
	    cd $ROOT
	    # $BOOT_FILES is now only an empty skeleton, let's get rid of it.
	    rm -rf $BOOT_FILES

	else
	    echo "$this !!! No heap image generated (sml.$HEAP_SUFFIX)."
	    exit 1
	fi
    else
	echo "$this !!! Boot code failed, no heap image (sml.$HEAP_SUFFIX)."
	exit 1
    fi
fi

#
# Initialize $LIBLIST
#
cd $ROOT
rm -f $LOCALPATHCONFIG $LIBLIST
echo 'ignore (OS.Process.exit (if true' >$LIBLIST

#
# now build (or prepare to build) the individual targets
#
cd $SRCDIR
echo $this: Installing other targets.
for i in $TARGETS ; do
    case $i in
      src-smlnj)
	for src in compiler cm MLRISC smlnj-lib ml-yacc system
	do
	    unpack $src $ROOT/src $src $src
        done
	;;
      ml-yacc)
	standalone ml-yacc ML-Yacc src
	echo ml-yacc $TOOLDIR >>$CM_PATHCONFIG_DEFAULT
	;;
      ml-lex)
	standalone ml-lex ML-Lex
	echo ml-lex $TOOLDIR >>$CM_PATHCONFIG_DEFAULT
	;;
      ml-burg)
	standalone ml-burg ML-Burg
	echo ml-burg $TOOLDIR >>$CM_PATHCONFIG_DEFAULT
	;;
      ml-nlffigen)
        echo standalone ml-nlffigen ML-NLFFI-Gen >>$LATESTANDALONES
	echo ml-nlffigen $TOOLDIR >>$CM_PATHCONFIG_DEFAULT
	;;
      smlnj-lib)
        unpack "SML/NJ Library" $SRCDIR smlnj-lib smlnj-lib

	# Don't make the Util library -- it came pre-made and has been
	# installed when making the base system.  In other words, don't do...
	    #reglib smlnj-lib.cm smlnj-lib.cm smlnj-lib/Util
	# ... and don't make the HTML library ...
	    #reglib html-lib.cm html-lib.cm smlnj-lib/HTML
	# ... and don't make the PP library ...
	    #reglib pp-lib.cm pp-lib.cm smlnj-lib/PP
	# make the Unix library
	    reglib unix-lib.cm unix-lib.cm smlnj-lib/Unix
	# make the INet library
	    reglib inet-lib.cm inet-lib.cm smlnj-lib/INet
	# make the RegExp library
	    reglib regexp-lib.cm regexp-lib.cm smlnj-lib/RegExp
	# make the Reactive library
	    reglib reactive-lib.cm reactive-lib.cm smlnj-lib/Reactive
	;;
      cml)
        unpack CML $SRCDIR cml cml
	reglib cml core-cml.cm cml/src
	reglib cml cml-internal.cm cml/src
	reglib cml cml.cm cml/src
	reglib cml basis.cm cml/src
	;;
      cml-lib)
        unpack CML $SRCDIR cml cml
	reglib cml-lib trace-cml.cm cml/cml-lib/cm
	reglib cml-lib smlnj-lib.cm cml/cml-lib/cm
	;;
      eXene)
        unpack EXene $SRCDIR eXene eXene
	reglib eXene.cm eXene.cm eXene
	;;
      ckit)
        unpack "C-Kit" $ROOT ckit ckit
	reglib ckit-lib.cm ckit-lib.cm ../ckit/src
	;;
      ml-nlffi-lib)
        unpack "NLFFI Library" $SRCDIR ml-nlffi-lib ml-nlffi-lib
	reglib memory.cm memory.cm ml-nlffi-lib/memory
	reglib c-int.cm c-int.cm ml-nlffi-lib/internals
	reglib c.cm c.cm ml-nlffi-lib
	;;
      pgraph-util)
	unpack "CM source code" $SRCDIR cm cm
	reglib pgraph-util.cm pgraph-util.cm cm/pgraph
	;;
      doc)
	unpack Doc $ROOT doc doc
        cd $ROOT/doc
	build $ROOT
	;;
      *)
        echo "$this: !!! Unknown target $i."
	;;
    esac
done

#
# Now go and stabilize all registered libraries...
# This is done with library sources in their original locations inside
# $SRCDIR, so we must consult $LOCALPATHCONFIG.
#

echo $this: Compiling library code.
echo 'then OS.Process.success else OS.Process.failure));' >>$LIBLIST
if CM_LOCAL_PATHCONFIG=$LOCALPATHCONFIG $BINDIR/sml <$LIBLIST ; then
    echo $this: Libraries compiled successfully.
else
    echo "$this: !!! Something went wrong when compiling the libraries."
    exit 1
fi

#
# Move the libraries to their final locations...
#

if [ -r $LIBMOVESCRIPT ] ; then
    echo $this: Moving libraries to $LIBDIR.
    . $LIBMOVESCRIPT
fi

#
# Build "late" standalone programs (i.e., those that must be built
# after libraries are already in place):
#

if [ -r $LATESTANDALONES ] ; then
    echo $this: Building late standalone programs.
    . $LATESTANDALONES
fi

#
# Finally, remove duplicate entries from pathconfig file...
#
if [ -f $CM_PATHCONFIG_DEFAULT ] ; then
    cp $CM_PATHCONFIG_DEFAULT $PCEDITTMP
    rm -f $CM_PATHCONFIG_DEFAULT
    awk <$PCEDITTMP 'NF == 2 { mapping[$1] = $2 }
NF != 2 { print $0 }
END { for (i in mapping) print i, mapping[i] }' \
      | sort >$CM_PATHCONFIG_DEFAULT
fi

exit 0
