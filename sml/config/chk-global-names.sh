#!/bin/sh
#
# COPYRIGHT (c) 1995 AT&T Bell Laboratories.
#
# Check to see if "_" is prepended to global names in the symbol table.
#

CC=${CC:-cc}

TMP_FILE=/tmp/smlConfig-$$
TMP_FILE_C=$TMP_FILE.c

cat > $TMP_FILE_C <<XXXX
main () {}
XXXX

$CC -c -o $TMP_FILE $TMP_FILE_C
if [ "$?" != "0" ]; then
    rm -f $TMP_FILE $TMP_FILE_C
    exit 1
fi

if `nm $TMP_FILE | grep -q "_main"`
  then echo "-DGLOBALS_HAVE_UNDERSCORE"
fi             

rm -f $TMP_FILE $TMP_FILE_C

exit 0
