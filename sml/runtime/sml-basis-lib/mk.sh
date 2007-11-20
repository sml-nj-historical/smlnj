#!/bin/sh
#
# script to run make on the basis library
#

DEFS="-DHOST_X86 -DTARGET_X86 -DOPSYS_UNIX -DOPSYS_LINUX -D_POSIX_SOURCE -D_BSD_SOURCE"

echo make VERSION=v-x86-linux CC="gcc -ansi" CFLAGS="-O2 -g" DEFS="$DEFS"
make VERSION=v-x86-linux CC="gcc -ansi" CFLAGS="-O2 -g" DEFS="$DEFS"

