#!/bin/sh
#
# unpack object src-path obj-src tar-file
#

OBJECT=$1
SRCDIR=$2
SRCPATH=$SRCDIR/$3
TARFILE=$4

if [ ! -d $SRCPATH ]; then
  if [ -r $TARFILE.Z ]; then
    cd $SRCDIR
    echo "unpacking $OBJECT source files"
    zcat $TARFILE.Z | tar -xf -
  elif [ -r $TARFILE ]; then
    cd $SRCDIR
    echo "unpacking $OBJECT source files"
    tar -xf $TARFILE
  elif [ -r $TARFILE.gz ]; then
    cd $SRCDIR
    echo "unpacking $OBJECT source files"
    gunzip -c $TARFILE.gz | tar -xf -
  else
    echo "!!! the $OBJECT source files are missing"
    exit 1
  fi
  if [ ! -d $SRCPATH ]; then
    echo "!!! unable to unpack $OBJECT source files"
    exit 1
  fi
fi
exit 0
