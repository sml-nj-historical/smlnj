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
if [ ! -r config/targets ]; then
    echo "$this: !!! File config/targets is missing."
    exit 1
fi
. config/targets

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
LIBMOVESCRIPT=$ROOT/libmove	# a temporary script
LOCALPATHCONFIG=$INSTALLDIR/pathconfig # a temporary pathconfig file

URLGETTER=unknown

#
# the path to the dir where ml-yacc, ml-burg, ml-lex, and ml-build live
#
TOOLDIR=$BINDIR

#
# files to be deleted after we are done...
#
tmpfiles=""
tmpfiles="$tmpfiles $ROOT/preloads.standard"
tmpfiles="$tmpfiles $LIBLIST"
tmpfiles="$tmpfiles $LOCALPATHCONFIG"
tmpfiles="$tmpfiles $LIBMOVESCRIPT"
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
# Function to make a directory (and advertise such action).
#
makedir() {
    if [ ! -d $1 ] ; then
	echo $this: Making directory $1
	if mkdir $1 ; then
	    : everything is fine
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
    echo $this: Please, fetch $1 archive '('$VERSION-$2.'*)' from
    echo '  ' $3
    echo "and then re-run this script!"
    exit 1
}

#
# Function for fetching source archives automatically using wget or lynx.
#   $1 - command to actually get the stuff
#   $2 - descriptive name
#   $3 - base name without extension, without version, and without dir
#   $4 - remote directory
#
fetchurl() {
    getter=$1 ; shift
    echo $this: Fetching $1 from $2. Please stand by...
    fetched=no
    for ext in tgz tar.gz tar.Z tz tar tar.bz2 ; do
	try=$VERSION-$2.$ext
	echo $this: Trying $try ...
	if $getter $3 $try $ROOT/$try ; then
	    fetched=yes
	    echo $this: Fetching $try was a success.
	    break
	else
	    rm -f $ROOT/$try
	fi
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
	elif testurlgetter lynx -help ; then
	    URLGETTER="fetchurl uselynx"
	else
	    URLGETTER="askurl"
	fi
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
# on archive is found locally, it invokes $URLGETTER and tries again.
# The variable $tryfetch is used to make sure this happens only once.
fetch_n_unpack() {
    larc=$ROOT/$VERSION-$4
    cd $2
    if [ -r $larc.tar.Z ] ; then
	echo "$this: Un-compress-ing and un-tar-ing $1 archive."
	zcat $larc.tar.Z | tar -xf -
    elif [ -r $larc.tar ] ; then
	echo "$this: Un-tar-ing $1 archive."
	tar -xf $larc.tar
    elif [ -r $larc.tar.gz ] ; then
	echo "$this: Un-gzip-ing and un-tar-ing $1 archive."
	gunzip -c $larc.tar.gz | tar -xf -
    elif [ -r $larc.tar.bz2 ] ; then
	echo "$this: Un-bzip2-ing and un-tar-ing $1 archive."
	bunzip2 -c $larc.tar.bz2 | tar -xf -
    elif [ -r $larc.tgz ] ; then
	echo "$this: Un-gzip-ing and un-tar-ing $1 archive."
	gunzip -c $larc.tgz | tar -xf -
    elif [ -r $larc.tz ] ; then
	echo "$this: Un-compress-ing and un-tar-ing $1 archive."
	zcat $larc.tz | tar -xf -
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

#
# Move the stable archive of a library whose description file was $1/$2 to
# $LIBDIR/$2/CM/$ARCH-unix/$2 so that it appears as if the description file
# had been at $LIBDIR/$2/$2
#
# (This script will also move all other libraries that show up in
#  $1/CM/$ARCH-unix because in the case of the boot directory this indicates
#  that some library did not have its own path anchor but was specified
#  relative to $1/$2. Still, don't rely on this to be robust -- rather make
#  separate anchors for every library!)
#
movelibs() {
    for lib in `/bin/ls $1/CM/$ARCH-unix` ; do
	case $lib in
	*.cm | *.cmi)
	    if [ $lib != $2 ] ; then
		echo "$this: Warning:" $lib specified relative to $2
	    fi
	    echo $this: Moving library $lib to $LIBDIR
	    makedir $LIBDIR/$2
	    makedir $LIBDIR/$2/CM
	    makedir $LIBDIR/$2/CM/$ARCH-unix
	    mv $1/CM/$ARCH-unix/$lib $LIBDIR/$2/CM/$ARCH-unix/$lib
	    ;;
	*)
	    ;;
	esac
     done
}

# A shell function that registers a library for being built.
# This function takes two arguments: 1. a name under which the library
# is to be known later (something.cm) and 2. the path relative to $SRCDIR
# that leads to the library's .cm file.  The library's .cm file must be the
# same as $1.
#
# This works by adding ML code to file $LIBLIST.  The code in this file
# will be executed near the end of this script.  If $MOVE_LIBRARIES is
# set to true, then reglib will also register a "movelibs" to be executed at
# the end by putting a "movelibs" line into $LIBMOVESCRIPT.

reglib() {
    if [ x$MOVE_LIBRARIES = xtrue ] ; then
	FINALLOCATION=$LIBDIR/$1
    else
	FINALLOCATION=$SRCDIR/$2
    fi
    if [ -d $FINALLOCATION/CM/$ARCH-unix ] ; then
	echo "$this: Library $1 already exists in $FINALLOCATION."
    else
        echo "$this: Scheduling library $1 to be built in $FINALLOCATION."
        echo "  andalso CM.stabilize false \"$1\"" >>$LIBLIST
        echo $1 $SRCDIR/$2 >>$LOCALPATHCONFIG
        if [ x$MOVE_LIBRARIES = xtrue ] ; then
	    echo movelibs $SRCDIR/$2 $1 >>$LIBMOVESCRIPT
        fi
	echo $1 $FINALLOCATION >>$CM_PATHCONFIG_DEFAULT
    fi
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
	./build
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
for dir in $BINDIR $HEAPDIR $RUNDIR $LIBDIR $SRCDIR
do
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

#
# set some architecture dependent run-time system flags
#
case $ARCH in
    mips*)
	ALLOC=1M
	;;
    x86)
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
BOOT_FILES=sml.boot.$ARCH-unix

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
	# $MAKE MAKE=$MAKE clean
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
    unpack bin $ROOT $BOOT_FILES $BOOT_FILES
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
echo 'OS.Process.exit (if true' >$LIBLIST

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
      smlnj-lib)
        unpack "SML/NJ Library" $SRCDIR smlnj-lib smlnj-lib

	# Don't make the Util library -- it came pre-made and has been
	# installed when making the base system.  In other words, don't do...
	    #reglib smlnj-lib.cm smlnj-lib/Util
	# ... and don't make the HTML library ...
	    #reglib html-lib.cm smlnj-lib/HTML
	# ... and don't make the PP library ...
	    #reglib pp-lib.cm smlnj-lib/PP
	# make the Unix library
	    reglib unix-lib.cm smlnj-lib/Unix
	# make the INet library
	    reglib inet-lib.cm smlnj-lib/INet
	# make the RegExp library
	    reglib regexp-lib.cm smlnj-lib/RegExp
	# make the Reactive library
	    reglib reactive-lib.cm smlnj-lib/Reactive
	;;
      cml)
        unpack CML $SRCDIR cml cml
	reglib core-cml.cm cml/src/core-cml
	reglib cml.cm cml/src
	reglib cml-basis.cm cml
	;;
      cml-lib)
        unpack CML $SRCDIR cml cml
	reglib cml-lib.cm cml/cml-lib
	;;
      eXene)
        unpack EXene $SRCDIR eXene eXene
	reglib eXene.cm eXene
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
echo 'then OS.Process.success else OS.Process.failure);' >>$LIBLIST
if CM_LOCAL_PATHCONFIG=$LOCALPATHCONFIG $BINDIR/sml <$LIBLIST ; then
    echo $this: Libraries compiled successfully.
else
    echo "$this: !!! Something went wrong when compiling the libraries."
    exit 1
fi

#
# Finally, move the libraries to their final locations...
#

if [ -r $LIBMOVESCRIPT ] ; then
    echo $this: Moving libraries to $LIBDIR.
    . $LIBMOVESCRIPT
fi

exit 0
