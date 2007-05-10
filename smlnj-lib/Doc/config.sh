#!/bin/sh
#
# This script creates the Makefile for building the documentation.  You
# will need to have installed the ML-Doc tools and have mk-mldoc-makefile
# program in your path.

find ML-Doc -name "*.mldoc" -print | mk-mldoc-makefile

