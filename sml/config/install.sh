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

if [ x${INSTALL_QUIETLY} = xtrue ] ; then
    export CM_VERBOSE=false
fi

vsay() {
    if [ x${INSTALL_DEBUG} = xtrue ] ; then
	echo "$@"
    elif [ x${INSTALL_QUIETLY} = xtrue ] ; then
	:
    else
	echo "$@"
    fi
}

dsay() {
    if [ x${INSTALL_DEBUG} = xtrue ] ; then
	echo "$@"
    fi
}

complain() {
    echo "$@"
    exit 1
}

# The following variable holds the name of all possible targets.
# The names must occur in the order in which targets are to be built.
ALLTARGETS=\
"src-smlnj \
 ml-yacc \
 ml-lex \
 ml-burg \
 ml-nlffigen \
 smlnj-lib \
 cml \
 cml-lib \
 eXene \
 ckit \
 ml-nlffi-lib \
 pgraph-util \
 mlrisc-tools \
 nowhere \
 doc"

this=$0

# By default, move libraries to lib directory:
MOVE_LIBRARIES=true

# function to be used in config/targets:
dont_move_libraries() { MOVE_LIBRARIES=false ; }

# Initialize target list.
TARGETS=""

# function to be used in config/targets:
request() { TARGETS="$TARGETS $1" ; }

#
# get the target list
#
if [ -r config/targets.customized ] ; then
    . config/targets.customized
elif [ ! -r config/targets ]; then
    complain "$this: !!! File config/targets is missing."
else
    . config/targets
fi

#
# resolve dependencies
#
isnotin() {
    tested_x=$1
    shift
    for set_y in "$@" ; do
	if [ ${tested_x} = ${set_y} ] ; then
	    return 1
	fi
    done
    return 0
}

require() {
    require_who=$1
    shift
    if isnotin ${require_who} ${TARGETS} ; then
	:
    else
	for required_x in "$@" ; do
	    if isnotin ${required_x} ${TARGETS} ; then
		echo "Including ${required_x} (needed by ${require_who})."
		request ${required_x}
		CHANGED=true
	    fi
	done
    fi
}

onepass() {
    while read depline ; do
	require $depline
    done
}

if [ -r config/dependencies ] ; then
    CHANGED=true
    while [ $CHANGED = true ]; do
	CHANGED=false
	onepass <config/dependencies
    done
fi

#
# Rebuild target list using the order defined by ALLTARGETS.
#
NEWTARGETS=""
for t in ${ALLTARGETS} ; do
    if isnotin $t ${TARGETS} ; then
	:
    else
	NEWTARGETS="$NEWTARGETS $t"
    fi
done
TARGETS=$NEWTARGETS

#
# create the preloads.standard file
#
if [ ! -r config/preloads ]; then
    complain "$this: !!! File config/preloads is missing."
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
vsay $this: Using shell $SHELL.

#
# set the SML root directory
#
REAL_PWD=`pwd`
ROOT=${PWD:-$REAL_PWD}
vsay $this: SML root is $ROOT.
vsay $this: Installation directory is ${INSTALLDIR:=$ROOT}.

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
vsay $this: Installing version $VERSION.

#
# the URL for the (usually remote) source archive
#
SRCARCHIVEURL=`cat $CONFIGDIR/srcarchiveurl`
vsay $this: URL of source archive is $SRCARCHIVEURL.

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
	if [ x${INSTALL_VERBOSE} = xtrue ] ; then
	    vsay "$this: Making directory $1"
	fi
	if mkdir $1 ; then
	    :
	else
	    complain "$this: !!! Unable to make directory $1!"
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
    vsay $this: Fetching $1 from $3. Please stand by...
    fetched=no
    for base in $2 $VERSION-$2 ; do
	for ext in tar.gz tgz tar.Z tz tar tar.bz2 ; do
	    try=$base.$ext
	    vsay $this: Trying $try ...
	    if $getter $3/$try $ROOT/$try ; then
		fetched=yes
		vsay $this: Fetching $try was a success.
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

# wrapper for wget
usewget() {
    wget -nv -O $2 $1
}

# wrapper for lynx
uselynx() {
    lynx -source $1 >$2
}

# wrapper for curl
usecurl() {
    curl -s $1 >$2
}

testurlgetter() {
    (exec >/dev/null 2>&1 ; exec $*)
}

#
# Function to check whether wget or lynx is available.
# Set URLGETTER accordingly.  URLGETTER can be set externally
# to either 'wget' or 'curl' or 'lynx' -- in which case the
# corresponding command will be used (properly wrapped).  Any
# other external setting will be passed directly to fetchurl (without
# wrapping -- meaning it must take precisely two argumets: source and
# destination, in that order).
#
urlgetter() {
    case ${URLGETTER:-unknown} in
	fetchurl*)
	    ;;
	unknown)
	    # automatically figure out which wrapper to use
	    if testurlgetter wget --help ; then
		URLGETTER="fetchurl usewget"
	    elif testurlgetter curl -s -O file:///dev/null -o /dev/null ; then
		URLGETTER="fetchurl usecurl"
	    elif testurlgetter lynx -help ; then
		URLGETTER="fetchurl uselynx"
	    else
		URLGETTER="askurl"
	    fi
	    ;;
	wget|curl|lynx)
	    # special getters we know how to wrap
	    URLGETTER="fetchurl use${URLGETTER}"
	    ;;
	*)
	    # other -- must be able to work without wrapper
	    URLGETTER="fetchurl ${URLGETTER}"
	    ;;
    esac
}

# wrapper for tar
un_tar() {
    vsay "$this: Un-TAR-ing $1 archive."
    tar -xf $2
}

# wrapper for zcat followed by tar
un_tar_Z() {
    vsay "$this: Un-COMPRESS-ing and un-TAR-ing $1 archive."
    zcat $2 | tar -xf -
}

# wrapper for gunzip followed by tar
un_tar_gz() {
    vsay "$this: Un-GZIP-ing and un-TAR-ing $1 archive."
    gunzip -c $2 | tar -xf -
}

# wrapper for bunzip2 followed by tar
un_tar_bz2() {
    vsay "$this: Un-BZIP2-ing and un-TAR-ing $1 archive."
    bunzip2 -c $2 | tar -xf -
}

# unarchive archive without and with version number attached
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
       unarchive "$1" $4.tar.bz2 un_tar_bz2 ||
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
	vsay "$this: The $1 tree already exists."
    else
	fetch_n_unpack "$1" "$2" "$3" "$4"
    fi
    if [ ! -d $2/$3 ]; then
	complain "$this: !!! Unable to unpack $1 archive."
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
		complain $this: $2 exists as a non-directory.
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
	vsay "$this: Library ${LIBNAME} already exists in ${FINALLOC}"
    else
	vsay "$this: Scheduling library ${LIBNAME} to be built as ${FINALLOC}."
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
# 2 or 3 or 4 arguments.  First the name of the program which at the same time
# is the directory name under $SRCDIR/$4 where the sources reside.  The second
# argument is a descriptive name for the program (passed on to "unpack").
# The optional third argument specifies the path relative to $SRCDIR/$4/$1
# of the directory where the program's heap image is to be found.
# The fourth argument, if missing, defaults to "."
#

standalone() {
    TARGET=$1.$HEAP_SUFFIX
    if [ $# = 3 ] ; then
	TARGETLOC=$3/$TARGET
    else
	TARGETLOC=$TARGET
    fi
    if [ $# = 4 ] ; then
         MYSRCDIR=$SRCDIR/$4
    else
         MYSRCDIR=$SRCDIR
    fi
    if [ -r $HEAPDIR/$TARGET ] ; then
	vsay $this: Target $TARGET already exists.
    else
	echo $this: Building $TARGET.
	unpack $2 $MYSRCDIR $1 $1
	cd $MYSRCDIR/$1
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

######################################################################

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
    vsay $this: Script $BINDIR/.arch-n-opsys already exists.
else
    cat $CONFIGDIR/_arch-n-opsys \
    | sed -e "s,@SHELL@,$SHELL,g" > $BINDIR/.arch-n-opsys
    chmod 555 $BINDIR/.arch-n-opsys
    if [ ! -x $BINDIR/.arch-n-opsys ]; then
	complain "$this: !!! Installation of $BINDIR/.arch-n-opsys failed."
    fi
fi

ARCH_N_OPSYS=`$BINDIR/.arch-n-opsys`
if [ "$?" != "0" ]; then
    echo "$this: !!! Script $BINDIR/.arch-n-opsys fails on this machine."
    echo "$this: !!! You must patch this by hand and repeat the installation."
    exit 2
else
    vsay $this: Script $BINDIR/.arch-n-opsys reports $ARCH_N_OPSYS.
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
	    complain "$this: !!! Installation of $BINDIR/${ddst} failed."
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
	    complain "$this: !!! Problems checking for underscores in asm names."
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
    vsay $this: Run-time system already exists.
else
    cd $SRCDIR/runtime/objs
    echo $this: Compiling the run-time system.
    $MAKE -f mk.$ARCH-$OPSYS $EXTRA_DEFS
    if [ -x run.$ARCH-$OPSYS ]; then
	mv run.$ARCH-$OPSYS $RUNDIR
	$MAKE MAKE=$MAKE clean
    else
	complain "$this: !!! Run-time system build failed for some reason."
    fi
fi
cd $SRCDIR

#
# boot the base SML system
#
if [ -r $HEAPDIR/sml.$HEAP_SUFFIX ]; then
    vsay $this: Heap image $HEAPDIR/sml.$HEAP_SUFFIX already exists.
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
	    complain "$this !!! No heap image generated (sml.$HEAP_SUFFIX)."
	fi
    else
	complain "$this !!! Boot code failed, no heap image (sml.$HEAP_SUFFIX)."
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
vsay $this: Installing other targets.
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
	# ... and don't make the controls library ...
	    #reglib controls-lib.cm controls-lib.cm smlnj-lib/Controls
	# make the Unix library
	    reglib unix-lib.cm unix-lib.cm smlnj-lib/Unix
	# make the INet library
	    reglib inet-lib.cm inet-lib.cm smlnj-lib/INet
	# make the RegExp library
	    reglib regexp-lib.cm regexp-lib.cm smlnj-lib/RegExp
	# make the Reactive library
	    reglib reactive-lib.cm reactive-lib.cm smlnj-lib/Reactive
        # make the HashCons library
	    reglib hash-cons-lib.cm hash-cons-lib.cm smlnj-lib/HashCons
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
	reglib c memory/memory.cm ml-nlffi-lib
	reglib c internals/c-int.cm ml-nlffi-lib
	reglib c c.cm ml-nlffi-lib
	;;
      pgraph-util)
	unpack "CM source code" $SRCDIR cm cm
	reglib pgraph-util.cm pgraph-util.cm cm/pgraph
	;;
      mlrisc-tools)
	unpack "MLRISC Tools Library" $SRCDIR MLRISC MLRISC
        reglib mlrisc-tools pp.cm MLRISC/Tools
        reglib mlrisc-tools source-map.cm MLRISC/Tools
        reglib mlrisc-tools sml-ast.cm MLRISC/Tools
        reglib mlrisc-tools prec-parser.cm MLRISC/Tools
        reglib mlrisc-tools parser.cm MLRISC/Tools
        reglib mlrisc-tools match-compiler.cm MLRISC/Tools
	;;
      nowhere)
	unpack "MLRISC Tools Library" $SRCDIR MLRISC MLRISC
        echo standalone nowhere NoWhere . MLRISC/Tools >>$LATESTANDALONES
	echo nowhere $TOOLDIR >>$CM_PATHCONFIG_DEFAULT
	;;
      doc)
	echo Package doc is currently unavailable.
	# unpack Doc $ROOT doc doc
        # cd $ROOT/doc
	# build $ROOT
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
    vsay $this: Libraries compiled successfully.
else
    complain "$this: !!! Something went wrong when compiling the libraries."
fi

#
# Move the libraries to their final locations...
#

if [ -r $LIBMOVESCRIPT ] ; then
    vsay $this: Moving libraries to $LIBDIR.
    . $LIBMOVESCRIPT
fi

#
# Build "late" standalone programs (i.e., those that must be built
# after libraries are already in place):
#

if [ -r $LATESTANDALONES ] ; then
    vsay $this: Building late standalone programs.
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
