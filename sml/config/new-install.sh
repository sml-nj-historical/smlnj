#!/bin/sh
#
# Copyright (c) 1994 AT&T Bell Laboratories.
# Copyright (c) 2003 The Fellowship of SML/NJ
#
# Installation script for SML/NJ and related tools; this is a temporary
# placeholder until the configuration tool is finished.
#
# Significant changes to take advantage of a new portable installer
# script for everything after booting the interactive system.
#
# Author: Matthias Blume (blume@tti-c.org)
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

#
# files to be deleted after we are done...
#
tmpfiles=""
tmpfiles="$tmpfiles $ROOT/preloads.standard"
#
# make sure we always clean up after ourselves...
#
trap 'rm -f $tmpfiles' 0 1 2 3 15


#
# set the CM configuration variables (these are environment variables
# that will be queried by the bootstrap code)
# Especially important is CM_PATHCONFIG.
#
CM_PATHCONFIG=$LIBDIR/pathconfig
export CM_PATHCONFIG

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

######################################################################

#
# create the various sub directories
#
for dir in $BINDIR $HEAPDIR $RUNDIR $LIBDIR $SRCDIR ; do
    makedir $dir
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

#
# install the script that tests architecture and os...
#
installdriver _arch-n-opsys .arch-n-opsys

#
# run it to figure out what architecture and os we are using, define
# corresponding variables...
#
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
# now install all the other driver scripts...
#
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
$CONFIGDIR/unpack $ROOT runtime
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
    $CONFIGDIR/unpack $ROOT $BOOT_ARCHIVE
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
		    echo $anchor $anchor >>$CM_PATHCONFIG
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
# Now do all the rest using the libinstall.sml script:
#
echo $this: Compiling library code.
if $BINDIR/sml $CONFIGDIR/libinstall.sml <<EOF
LibInstall.proc { smlnjroot = "${ROOT}",
                  targetsfiles = ["config/targets.customized",
                                  "config/targets"],
                  buildcmd = "CM_LOCAL_PATHCONFIG=/dev/null ./build",
                  unpackcmd = SOME "$CONFIGDIR/unpack",
                  instcmd = fn target => let
                              val new = "$BINDIR/" ^ target
                            in
                               if OS.FileSys.access (new, []) then ()
                               else
                                 Posix.FileSys.symlink
                                    { old = "$BINDIR/.run-sml",
                                      new = new }
                             end }
EOF
then
    vsay $this: Libraries compiled successfully.
else
    complain "$this: !!! Something went wrong when compiling the libraries."
fi

exit 0
