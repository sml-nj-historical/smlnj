#!/bin/ksh
#

SML="/usr/local/sml/109.10/bin/sml @SMLdebug=/dev/null"

INPUT=mldoc/input

MLDOC_DIR="/home/jhr/SGML/ML-Doc"
BIN_DIR="$MLDOC_DIR/bin"
INDEX_DIR="INDEX"
HTML_DIR="HTML"

INDEX_EXTRACT="$BIN_DIR/index-extract"
HTML_EXTRACT="$BIN_DIR/html-extract"
HTML_INDEX="$BIN_DIR/html-index"

rm -f "$INDEX_DIR/INDEX"
for i in $(cat $INPUT)
do
  BASE=$(basename $i ".mldoc")
  rm -rf "$HTML_DIR/$i.html" "$INDEX_DIR/$i.index"
done

echo "creating index files"
for i in $(cat $INPUT)
do
  echo "index for $i"
#  $SML @SMLload=$INDEX_EXTRACT -o $INDEX_DIR $i
  $SML @SMLload=$INDEX_EXTRACT -merge $INDEX_DIR/INDEX $i
done

echo "creating HTML files"
for i in $(cat $INPUT)
do
  BASE=$(basename $i ".mldoc")
  echo "$i --> $HTML_DIR/$BASE.html"
  $SML @SMLload=$HTML_EXTRACT -dir $HTML_DIR -index $INDEX_DIR/INDEX -head $HTML_DIR/HEAD.template -foot $HTML_DIR/FOOT.template $i
done

echo "creating HTML index"
$SML @SMLload=$HTML_INDEX -i $INDEX_DIR/INDEX -dir $HTML_DIR -all

