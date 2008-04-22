# Makefile
#
# COPYRIGHT (c) 2007 The SML/NJ Fellowship
#
# Temporary makefile for generating glue code from the IDL specs.
# Eventually, this should be generated from a Makefile.in as
# part of system configuration
#

# assume that we are in an SML/NJ tree and that the ml-idl tool is
# installed in the bin directory
#
ML_IDL=../../bin/ml-idl
ML_IDL_FLAGS=

# roots of the various installation directories
#
BASIS_IMP_DIR=../system/Basis/Implementation
OLD_RUNTIME_DIR=../runtime
NEW_RUNTIME_DIR=.

IDL_FILES=	sml-basis.idl \
		smlnj-runtime.idl

help:
	@echo "This makefile supports the following targets:"
	@echo "    help    -- print this message"
	@echo "    classic -- generate C glue code for the classic runtime"
	@echo "    new-rt  -- generate C++ glue code for the new runtime"
	@echo "    sml     -- generate the SML glue code for either runtime"
	@echo "    install -- copy generated files to the appropriate locations"
	@echo "    clean   -- remove any generated files"

classic:
	for f in $(IDL_FILES) ; do \
	  $(ML_IDL) -t classic $(ML_IDL_FLAGS) $$f; \
	done

new-rt:
	for f in $(IDL_FILES) ; do \
	  $(ML_IDL) -t new-rt $(ML_IDL_FLAGS) $$f; \
	done

sml:
	for f in $(IDL_FILES) ; do \
	  $(ML_IDL) -t smlnj $(ML_IDL_FLAGS) $$f; \
	done

install:

clean:
	rm -rf *.h *.c *.hxx *.cxx *.sml *.cm

