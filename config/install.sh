#!/bin/sh
#
# Copyright (c) 1994 AT&T Bell Laboratories.
# Copyright (c) 2014-2018 The Fellowship of SML/NJ
#
# Installation script for SML/NJ and related tools.
#
# Significant changes to take advantage of a new portable installer
# script for everything after booting the interactive system.
#
# Author: Matthias Blume (blume@tti-c.org)
#

if [ x$1 = xnolib ] ; then
    nolib=true
else
    nolib=false
fi

if [ x${INSTALL_QUIETLY} = xtrue ] ; then
    export CM_VERBOSE
    CM_VERBOSE=false
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

complain() {
    echo "$@"
    exit 1
}

this=$0


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

SHELL=/bin/sh
vsay $this: Using shell $SHELL.

#
# set the SML root directory
#
REAL_PWD=`pwd`
ROOT=${PWD:-$REAL_PWD}
vsay $this: SML root is $ROOT.

cd "${INSTALLDIR:=$ROOT}"
INSTALLDIR=`pwd`
cd "$ROOT"
vsay $this: Installation directory is ${INSTALLDIR}.

#
# set the various directory and file pathname variables
#
BINDIR=$INSTALLDIR/bin		# main dir for binary stuff
CONFIGDIR=$ROOT/config
HEAPDIR=$BINDIR/.heap		# where heap images live
RUNDIR=$BINDIR/.run		# where executables (i.e., the RTS) live
BASEDIR=$ROOT/base		# where the base source tree is rooted
LIBDIR=$INSTALLDIR/lib		# where libraries live

#
# files to be deleted after we are done...
#
tmpfiles=""
tmpfiles="$tmpfiles preloads.standard"
#
# make sure we always clean up after ourselves...
#
trap 'cd "$ROOT"; rm -f $tmpfiles' 0 1 2 3 15


#
# set the CM configuration variables (these are environment variables
# that will be queried by the bootstrap code)
# Especially important is CM_PATHCONFIG.
#
export CM_PATHCONFIG
CM_PATHCONFIG=$LIBDIR/pathconfig
#
# the release version that we are installing
#
VERSION=`cat "$CONFIGDIR/version"`
vsay $this: Installing version $VERSION.

#
# the URL for the (usually remote) source archive
#
. "$CONFIGDIR"/srcarchiveurl
vsay $this: URL of source archive is $SRCARCHIVEURL.

#
# Function to make a directory including its ancestors.
#
makedir() {
    if [ x"$1" = x ] ; then
	:
    elif [ -d "$1" ] ; then
	:
    else
	makedirtmp=`dirname "$1"`
	makedir "$makedirtmp"
	if [ x${INSTALL_VERBOSE} = xtrue ] ; then
	    vsay "$this: Making directory $1"
	fi
	if mkdir "$1" ; then
	    :
	else
	    complain "$this: !!! Unable to make directory $1!"
	fi
    fi
}

#
# Fish out the CM metadata directory name from library files
# and store it in ORIG_CM_DIR_ARC.
# The single argument is the name of the directory containing
# a single subdirectory which is a CM metadata directory:
#
fish() {
    cd "$1"
    ORIG_CM_DIR_ARC=unknown
    for i in * .[a-zA-Z0-9]* ; do
	if [ -d $i ] ; then
	    ORIG_CM_DIR_ARC=$i
	    break
	fi
    done
    if [ $ORIG_CM_DIR_ARC = unknown ] ; then
	complain "$this: could not determine CM metadata directory name"
    else
	echo "$this: CM metadata directory name is \"${ORIG_CM_DIR_ARC}\""
    fi
}


# A function to move all stable library files to a parallel directory
# hierarchy.
# The first argument must be a simple path (no / inside), and
# the second argument must be an absolute path.
move() {
    if [ -L "$1" ] ; then
	rm -f "$1"	     # remove symbolic link made by diracs (see below)
    elif [ -d "$1" ] ; then
	if [ ! -d "$2" ] ; then
	    if [ -f "$2" ] ; then
		complain $this: $2 exists as a non-directory.
	    fi
	    mkdir "$2"
	fi
	cd "$1"
	for i in * .[a-zA-Z0-9]* ; do
	    move "$i" "$2"/"$i"
	done
	cd ..
    elif [ -f "$1" ] ; then
	rm -f "$2"
	mv "$1" "$2"
    fi
}

#
# Traverse the directory tree rooted at $3 (must be single arc!).
# Find all directories named $1, rename them into $2 and make
# and establish $1 as a symbolic link to $2:
#
dirarcs() {
    if [ -d "$3" ] ; then
	if [ "$3" = "$1" ] ; then
	    mv "$1" "$2"
	    ln -s "$2" "$1"
	else
	    cd "$3"
	    for d in * .[a-zA-Z0-9]* ; do
		dirarcs "$1" "$2" "$d"
	    done
	    cd ..
	fi
    fi
}


######################################################################

#
# create the various sub directories
#
for dir in "$BINDIR" "$HEAPDIR" "$RUNDIR" "$LIBDIR" "$BASEDIR" ; do
    makedir "$dir"
done

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
	rm -f "$BINDIR"/"$ddst"
	cat "$CONFIGDIR"/"$dsrc" | \
	sed -e "s,@SHELL@,$SHELL,g" \
	    -e "s,@BINDIR@,$BINDIR," \
	    -e "s,@LIBDIR@,$LIBDIR," \
	    -e "s,@VERSION@,$VERSION," \
	    -e "s,@CMDIRARC@,${CM_DIR_ARC:-dummy}," \
	    > "$BINDIR"/"$ddst"
	chmod 555 "$BINDIR"/"$ddst"
	if [ ! -x "$BINDIR"/"$ddst" ]; then
	    complain "$this: !!! Installation of $BINDIR/${ddst} failed."
	fi
#   fi
}

#
# install the script that tests architecture and os...
#
installdriver _arch-n-opsys .arch-n-opsys

#
# run it to figure out what architecture and os we are using, define
# corresponding variables...
#
ARCH_N_OPSYS=`"$BINDIR"/.arch-n-opsys`
if [ "$?" != "0" ]; then
    case `uname -s` in
      CYGWIN*)
        echo "$this: !!! SML/NJ does not support 64-bit cygwin"
        echo :$this: !!! Please use the 32-bit version"
        ;;
      *)
        echo "$this: !!! Script $BINDIR/.arch-n-opsys fails on this machine."
        echo "$this: !!! You must patch this by hand and repeat the installation."
        ;;
    esac
    exit 2
else
    vsay $this: Script $BINDIR/.arch-n-opsys reports $ARCH_N_OPSYS.
fi
eval $ARCH_N_OPSYS

#
# now install most of the other driver scripts
#  (except ml-build, since we don't know $CM_DIR_ARC yet)
#
installdriver _run-sml .run-sml
installdriver _link-sml .link-sml
installdriver _ml-makedepend ml-makedepend

installdriver _heap2exec heap2exec

#
# set some architecture dependent run-time system flags
#
case $ARCH in
    mips*)
	ALLOC=1M
	;;
    x86)
	# The following is the _wrong_ value for some x86 chips
	# (i.e., Celerons).  We use 512k here and re-set it to the proper
	# value in .run-sml.
	ALLOC=512k
	;;
    alpha32)
	ALLOC=512k
	;;
    *)
	ALLOC=512k
	;;
esac

# OS-specific things for building the runtime system
#
RT_MAKEFILE=mk.$ARCH-$OPSYS
case $OPSYS in
    darwin)
	if [ "$ARCH" = "x86" ] ; then
            # the /usr/bin/as command does _not_ accept the -mmacosx-version-min
	    # command-line option prior to MacOS X 10.10 (Yosemite)
	    case `sw_vers -productVersion` in
	      10.6*) AS_ACCEPTS_SDK=no ;;
	      10.7*) AS_ACCEPTS_SDK=no ;;
	      10.8*) AS_ACCEPTS_SDK=no ;;
	      10.9*) AS_ACCEPTS_SDK=no ;;
	      10.14*) # Mojave needs a special makefile
		AS_ACCEPTS_SDK=yes
		RT_MAKEFILE=mk.x86-darwin18
		;;
	      *) AS_ACCEPTS_SDK=yes ;;
	    esac
	EXTRA_DEFS="AS_ACCEPTS_SDK=$AS_ACCEPTS_SDK"
	fi
	;;
    linux)
	EXTRA_DEFS=`"$CONFIGDIR/chk-global-names.sh"`
	if [ "$?" != "0" ]; then
	    complain "$this: !!! Problems checking for underscores in asm names."
	fi
	EXTRA_DEFS="XDEFS=$EXTRA_DEFS"
	;;
    solaris)
	MAKE=/usr/ccs/bin/make
	;;
esac

#
# on 64-bit linux systems, we need to check to see if the 32-bit emulation
# support is installed
#
if [ x"$ARCH" = "xx86" -a x"$OPSYS" = "xlinux" ] ; then
  case `uname -m` in
    x86_64)
      tmpFile=smlnj-test$$
      echo "int main () { return 0; }" >> /tmp/$tmpFile.c
      gcc -m32 -o /tmp/$tmpFile /tmp/$tmpFile.c 2> /dev/null 1>> /dev/null
      if [ "$?" != "0" ] ; then
	rm -f /tmp/$tmpFile /tmp/$tmpFile.c
        echo "$this: !!! SML/NJ requires support for 32-bit executables."
        echo "  Please see http://www.smlnj.org/dist/working/$VERSION/INSTALL for more details."
	exit 1
      else
	rm -f /tmp/$tmpFile /tmp/$tmpFile.c
      fi
    ;;
    *) ;;
  esac
fi

#
# the name of the bin files directory
#
BOOT_ARCHIVE=boot.$ARCH-unix
BOOT_FILES=sml.$BOOT_ARCHIVE

#
# build the run-time system
#
if [ -x "$RUNDIR"/run.$ARCH-$OPSYS ]; then
    vsay $this: Run-time system already exists.
else
    "$CONFIGDIR"/unpack "$ROOT" runtime
    cd "$BASEDIR"/runtime/objs
    echo $this: Compiling the run-time system.
    $MAKE -f $RT_MAKEFILE $EXTRA_DEFS
    if [ -x run.$ARCH-$OPSYS ]; then
	mv run.$ARCH-$OPSYS "$RUNDIR"
	if [ -f runx.$ARCH-$OPSYS ]; then
	    mv runx.$ARCH-$OPSYS "$RUNDIR"
	fi
	if [ -f run.$ARCH-$OPSYS.so ]; then
	    mv run.$ARCH-$OPSYS.so "$RUNDIR"
	fi
	if [ -f run.$ARCH-$OPSYS.a ]; then
	    mv run.$ARCH-$OPSYS.a "$RUNDIR"
	fi
	$MAKE MAKE=$MAKE clean
    else
	complain "$this: !!! Run-time system build failed for some reason."
    fi
fi
cd "$BASEDIR"

#
# boot the base SML system
#
if [ -r "$HEAPDIR"/sml.$HEAP_SUFFIX ]; then
    vsay $this: Heap image $HEAPDIR/sml.$HEAP_SUFFIX already exists.
    fish "$LIBDIR"/smlnj/basis
    # ignore requested arc name since we have to live with what is there:
    export CM_DIR_ARC
    CM_DIR_ARC=$ORIG_CM_DIR_ARC
    # now re-dump the heap image:
    vsay "$this: Re-creating a (customized) heap image..."
    "$BINDIR"/sml @CMredump "$ROOT"/sml
    cd "$ROOT"
    if [ -r sml.$HEAP_SUFFIX ]; then
	mv sml.$HEAP_SUFFIX "$HEAPDIR"
    else
	complain "$this !!! Unable to re-create heap image (sml.$HEAP_SUFFIX)."
    fi
else
    "$CONFIGDIR"/unpack "$ROOT" "$BOOT_ARCHIVE"

    fish "$ROOT"/"$BOOT_FILES"/smlnj/basis

    cd "$ROOT"

    # Target arc:
    export CM_DIR_ARC
    CM_DIR_ARC=${CM_DIR_ARC:-".cm"}

    if [ $CM_DIR_ARC = $ORIG_CM_DIR_ARC ] ; then
	: we are happy
    else
	# now we have to make a symbolic link for each occurrence of
	# $ORIG_CM_DIR_ARC to $CM_DIR_ARC
	dirarcs "$ORIG_CM_DIR_ARC" "$CM_DIR_ARC" "$BOOT_FILES"
    fi

    cd "$ROOT"/"$BOOT_FILES"

    # now link (boot) the system and let it initialize itself...
    if "$BINDIR"/.link-sml @SMLheap="$ROOT"/sml @SMLboot=BOOTLIST @SMLalloc=$ALLOC
    then
	cd "$ROOT"
	if [ -r sml.$HEAP_SUFFIX ]; then
	    mv sml.$HEAP_SUFFIX "$HEAPDIR"
	    cd "$BINDIR"
	    ln -s .run-sml sml
	    #
	    # Now move all stable libraries to #LIBDIR and generate
	    # the pathconfig file.
	    #
	    cd "$ROOT"/"$BOOT_FILES"
	    for anchor in * ; do
		if [ -d $anchor ] ; then
		    echo $anchor $anchor >>$CM_PATHCONFIG
		    move $anchor "$LIBDIR"/$anchor
		fi
	    done
	    cd "$ROOT"
	    # $BOOT_FILES is now only an empty skeleton, let's get rid of it.
	    rm -rf "$BOOT_FILES"

	else
	    complain "$this !!! No heap image generated (sml.$HEAP_SUFFIX)."
	fi
    else
	complain "$this !!! Boot code failed, no heap image (sml.$HEAP_SUFFIX)."
    fi
fi

#
# now that we know CM_DIR_ARC we can install the ml-build driver...
#
installdriver _ml-build ml-build

cd "$ROOT"

#
# Now do all the rest using the precompiled installer
# (see base/system/smlnj/installer for details)
#
if [ $nolib = false ] ; then
    echo $this: Installing other libraries and programs:
    export ROOT INSTALLDIR CONFIGDIR BINDIR
    CM_TOLERATE_TOOL_FAILURES=true
    export CM_TOLERATE_TOOL_FAILURES
    if "$BINDIR"/sml -m \$smlnj/installer.cm
    then
	vsay $this: Installation complete.
    else
	complain "$this: !!! Installation of libraries and programs failed."
    fi
fi

exit 0
