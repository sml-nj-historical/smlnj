#!/bin/sh
#
# Copyright (c) 1994 AT&T Bell Laboratories.
#
# installation script for SML/NJ and related tools; this is a temporary
# placeholder until the configuration tool is finished.
#

#set -x

#
# get the target list
#
if [ ! -r config/targets ]; then
  echo "!!! File config/targets is missing."
  exit 1;
fi
. config/targets

#
# some OSs have make in strange places
#
MAKE=make

#
# check for ksh
#
# ksh causes some people problems so we will always use /bin/sh
#
#echo "checking for ksh"
#if [ -x /bin/ksh ]; then
#  SHELL=/bin/ksh
#elif [ -x /usr/local/bin/ksh ]; then
#  SHELL=/usr/local/bin/ksh
#else
  SHELL=/bin/sh
#fi
echo Using shell $SHELL.

#
# set the SML root directory
#
REAL_PWD=`pwd`
ROOT=${PWD:-$REAL_PWD}
echo SML root is $ROOT.

#
# set the various directory and file pathname variables
#
BINDIR=$ROOT/bin		# main dir for binary stuff
CONFIGDIR=$ROOT/config
HEAPDIR=$BINDIR/.heap		# where heap images live
RUNDIR=$BINDIR/.run		# where executables (i.e., the RTS) live
SRCDIR=$ROOT/src		# where the source tree is rooted
LIBDIR=$ROOT/lib		# where libraries live
LIBLIST=$ROOT/liblist		# list of commands to stabilize libraries
LIBMOVESCRIPT=$ROOT/libmove	# a temporary script
LOCALPATHCONFIG=$ROOT/pathconfig # a temporary pathconfig file

#
# the paths to ml-yacc, ml-burg, and ml-lex; needed to configure CM
#
YACCPATH=$BINDIR/ml-yacc
LEXPATH=$BINDIR/ml-lex
BURGPATH=$BINDIR/ml-burg

#
# set the CM configuration variables (these are environment variables
# that will be queried by the bootstrap code)
# Especially important is CM_PATHCONFIG_DEFAULT.
#
CM_YACC_DEFAULT=$YACCPATH
CM_LEX_DEFAULT=$LEXPATH
CM_BURG_DEFAULT=$BURGPATH
CM_PATHCONFIG_DEFAULT=$LIBDIR/pathconfig
export CM_YACC_DEFAULT CM_LEX_DEFAULT CM_BURG_DEFAULT CM_PATHCONFIG_DEFAULT

#
# the release version that we are installing
#
VERSION=`cat $CONFIGDIR/version`
echo Installing version $VERSION.

#
# Function to make a directory (and advertise such action).
#
makedir() {
    if [ ! -d $1 ] ; then
	echo Making directory $1
	if mkdir $1 ; then
	    : everything is fine
	else
	    echo "!!! Unable to make directory $1!"
	    exit 1
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
unpack() {
    if [ ! -d $2/$3 ]; then
	echo Unpacking $1 source files.
	cd $2
	if [ -r $4.tar.Z ] ; then
	    zcat $4.tar.Z | tar -xf -
	elif [ -r $4.tar ] ; then
	    tar -xf $4.tar
	elif [ -r $4.tar.gz ] ; then
	    gunzip -c $4.tar.gz | tar -xf -
	elif [ -r $4.tar.bz2 ] ; then
	    bunzip2 -c $4.tar.bz2 | tar -xf -
	elif [ -r $4.tgz ] ; then
	    gunzip -c $4.tgz | tar -xf -
	elif [ -r $4.tz ] ; then
	    zcat $4.tz | tar -xf -
	else
	    echo "!!! The $1 source files are missing."
	    exit 1
	fi
	if [ ! -d $2/$3 ]; then
	    echo "!!! Unable to unpack $1 source files."
	    exit 1
	fi
    fi
}

#
# Move the stable archive of a library whose description file was $1/$2 to
# $LIBDIR/$2/CM/$ARCH-unix/$2 so that it appears as if the description file
# had been at $LIBDIR/$2/$2
#
movelib() {
    if [ -f $1/CM/$ARCH-unix/$2 ] ; then
	echo Moving library $2 to $LIBDIR.
	makedir $LIBDIR/$2
	makedir $LIBDIR/$2/CM
	makedir $LIBDIR/$2/CM/$ARCH-unix
	mv $1/CM/$ARCH-unix/$2 $LIBDIR/$2/CM/$ARCH-unix/$2
    fi
}

# A shell function that registers a library for being built.
# This function takes two arguments: 1. a name under which the library
# is to be known later (something.cm) and 2. the path relative to $SRCDIR
# that leads to the library's .cm file.  The library's .cm file must be the
# same as $1.
#
# This works by adding ML code to file $LIBLIST.  The code in this file
# will be executed near the end of this script.  If $MOVE_LIBRARIES is
# set to true, then reglib will also register a "movelib" to be executed at
# the end by putting a "movelib" line into $LIBMOVESCRIPT.

reglib() {
    if [ x$MOVE_LIBRARIES = xtrue ] ; then
	FINALLOCATION=$LIBDIR/$1
    else
	FINALLOCATION=$SRCDIR/$2
    fi
    if [ -d $FINALLOCATION ] ; then
	echo "Library $1 already exists in $FINALLOCATION."
    else
        echo "Scheduling library $1 to be built in $FINALLOCATION."
        echo "andalso CM.stabilize false \"$1\"" >>$LIBLIST
        echo $1 $SRCDIR/$2 >>$LOCALPATHCONFIG
        if [ x$MOVE_LIBRARIES = xtrue ] ; then
	    echo movelib $SRCDIR/$2 $1 >>$LIBMOVESCRIPT
        fi
	echo $1 $FINALLOCATION >>$CM_PATHCONFIG_DEFAULT
    fi
}

#
# Function to build a standalone program such as ml-yacc.  The function takes
# 2 or 3 arguments.  First the name of the program with at the same time
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
	echo Target $TARGET already exists.
    else
	echo Building $TARGET.
	unpack $2 $SRCDIR $1 $ROOT/$VERSION-$1
	cd $SRCDIR/$1
	./build
	if [ -r $TARGETLOC ] ; then
	    mv $TARGETLOC $HEAPDIR/$TARGET
	    if [ ! -f $BINDIR/$1 ] ; then
		cd $BINDIR
		ln -s .run-sml $1
	    fi
	else
	    echo "!!! Build of $TARGET failed."
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
  echo Script $BINDIR/.arch-n-opsys already exists.
else
  cat $CONFIGDIR/_arch-n-opsys | sed -e "s,@SHELL@,$SHELL,g" > $BINDIR/.arch-n-opsys
  chmod 555 $BINDIR/.arch-n-opsys
  if [ ! -x $BINDIR/.arch-n-opsys ]; then
    echo "!!! Installation of $BINDIR/.arch-n-opsys failed for some reason."
    exit 1
  fi
fi
ARCH_N_OPSYS=`$BINDIR/.arch-n-opsys`
if [ "$?" != "0" ]; then
  echo "!!! Script $BINDIR/.arch-n-opsys fails on this machine."
  echo "!!! You must patch this by hand and repeat the installation."
  exit 2
else
  echo Script $BINDIR/.arch-n-opsys reports $ARCH_N_OPSYS.
fi
eval $ARCH_N_OPSYS

if [ -x $BINDIR/.run-sml ]; then
  echo Script $BINDIR/.run-sml already exists.
else
  cat $CONFIGDIR/_run-sml | \
    sed -e "s,@SHELL@,$SHELL,g" -e "s,@BINDIR@,$BINDIR," -e "s,@VERSION@,$VERSION," \
    > $BINDIR/.run-sml
  chmod 555 $BINDIR/.run-sml
  if [ ! -x $BINDIR/.run-sml ]; then
    echo "!!! Installation of $BINDIR/.run-sml failed for some reason."
    exit 1
  fi
fi

#
# set some architecture dependent run-time system flags
#
case $ARCH in
  mips*) ALLOC=1M ;;
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
      echo "!!! Problems checking for underscores in global names."
      exit 1
    fi
    EXTRA_DEFS="XDEFS=$EXTRA_DEFS"
  ;;
esac

#
# the name of the bin files directory
#
BOOT_FILES=comp.boot.$ARCH-unix

#
# build the run-time system
#
unpack "run-time" $SRCDIR runtime $ROOT/$VERSION-runtime
if [ ! -x $RUNDIR/run.$ARCH-$OPSYS ]; then
  cd $SRCDIR/runtime/objs
  echo Compiling the run-time system.
  $MAKE -f mk.$ARCH-$OPSYS $EXTRA_DEFS
  if [ -x run.$ARCH-$OPSYS ]; then
    mv run.$ARCH-$OPSYS $RUNDIR
    $MAKE MAKE=$MAKE clean
  else
    echo "!!! Run-time system build failed for some reason."
    exit 1
  fi
fi
cd $SRCDIR

#
# boot the base SML system
#
if [ -r $HEAPDIR/sml.$HEAP_SUFFIX ]; then
  echo Heap image $HEAPDIR/sml.$HEAP_SUFFIX already exists.
else
  unpack bin $ROOT $BOOT_FILES $ROOT/$VERSION-$BOOT_FILES
  cd $ROOT
  if $RUNDIR/run.$ARCH-$OPSYS @SMLheap=sml \
	@SMLboot=$ROOT/$BOOT_FILES @SMLrtpid=`cat $BOOT_FILES/RTPID` \
	@SMLalloc=$ALLOC
  then
    if [ -r sml.$HEAP_SUFFIX ]; then
	mv sml.$HEAP_SUFFIX $HEAPDIR
	cd $BINDIR
	ln -s .run-sml sml

	#
	# Now move all stable libraries to #LIBDIR and generate
	# the pathconfig file.
	#

	cd $ROOT/$BOOT_FILES
	for lib in *.cm ; do
	    echo $lib $LIBDIR/$lib >>$CM_PATHCONFIG_DEFAULT
	    movelib $ROOT/$BOOT_FILES/$lib $lib
	done
	cd $ROOT
	rm -rf $BOOT_FILES

    else
	echo "!!! Boot code did not produce heap image (sml.$HEAP_SUFFIX)."
	exit 1
    fi
  else
    echo "!!! Boot code failed, no heap image built (sml.$HEAP_SUFFIX)."
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
echo Installing other targets.
for i in $TARGETS ; do
    case $i in
      src-smlnj)
	for src in compiler comp-lib cm MLRISC smlnj-lib ml-yacc system
	do
	    unpack $src $ROOT/src $src $ROOT/$VERSION-$src
        done
	;;
      ml-yacc)
	standalone ml-yacc ML-Yacc src
	;;
      ml-lex)
	standalone ml-lex ML-Lex
	;;
      ml-burg)
	standalone ml-burg ML-Burg
	;;
      smlnj-lib)
        unpack "SML/NJ Library" $SRCDIR smlnj-lib $ROOT/$VERSION-smlnj-lib

	# Don't make the Util library -- it came pre-made and has been
	# installed when making the base system.  In other words, don't do...
	    #reglib smlnj-lib.cm smlnj-lib/Util
	# make the Unix library
	    reglib unix-lib.cm smlnj-lib/Unix
	# make the INet library
	    reglib inet-lib.cm smlnj-lib/INet
	# make the HTML library
	    reglib html-lib.cm smlnj-lib/HTML
	# make the PP library
	    reglib pp-lib.cm smlnj-lib/PP
	# make the RegExp library
	    reglib regexp-lib.cm smlnj-lib/RegExp
	# make the Reactive library
	    reglib reactive-lib.cm smlnj-lib/Reactive
	;;
      cml)
        unpack CML $SRCDIR cml $ROOT/$VERSION-cml
	reglib core-cml.cm cml/src/core-cml
	reglib cml.cm cml/src
	reglib cml-basis.cm cml
	;;
      cml-lib)
        unpack CML $SRCDIR cml $ROOT/$VERSION-cml
	reglib cml-lib.cm cml/cml-lib
	;;
      eXene)
        unpack EXene $SRCDIR eXene $ROOT/$VERSION-eXene
	reglib eXene.cm eXene
	;;
      doc)
	unpack Doc $ROOT doc $ROOT/$VERSION-doc
        cd $ROOT/doc
	build $ROOT
	;;
      *)
        echo "!!! Unknown target $i."
	;;
    esac
done

#
# Now go and stabilize all registered libraries...
# This is done with library sources in their original locations inside
# $SRCDIR, so we must consult $LOCALPATHCONFIG.
#

echo Compiling library code.
echo 'then OS.Process.success else OS.Process.failure);' >>$LIBLIST
if CM_LOCAL_PATHCONFIG=$LOCALPATHCONFIG $BINDIR/sml <$LIBLIST ; then
    echo Libraries compiled successfully.
else
    echo "!!! Something went wrong when compiling the libraries."
    exit 1
fi
rm -f $LIBLIST $LOCALPATHCONFIG

#
# Finally, move the libraries to their final locations...
#

if [ -r $LIBMOVESCRIPT ] ; then
    echo Moving libraries to $LIBDIR.
    . $LIBMOVESCRIPT
    rm -f $LIBMOVESCRIPT
fi
