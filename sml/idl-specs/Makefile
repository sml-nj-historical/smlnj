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
NEW_RUNTIME_DIR=../../rt

IDL_FILES=		sml-basis.idl \
			smlnj-runtime.idl

BASIS_C_FILES =		cfun-list.h \
			cfun-proto-list.h \
			sml-basis-lib.c \
			sml-basis.c \
			sml-basis.h

RT_C_FILES =		cfun-list.h \
			cfun-proto-list.h \
			smlnj-runtime-lib.c \
			smlnj-runtime.c \
			smlnj-runtime.h

BASIS_CXX_FILES =	sml-basis-lib.cxx \
			sml-basis-lib.hxx \
			sml-basis.cxx \
			sml-basis.hxx

RT_CXX_FILES =		smlnj-runtime-lib.cxx \
			smlnj-runtime-lib.hxx \
			smlnj-runtime.hxx \
			smlnj-runtime.cxx

SML_FILES =	sml-basis-sig.sml \
		sml-basis.sml \
		smlnj-runtime-sig.sml \
		smlnj-runtime.sml

help:
	@echo "This makefile supports the following targets:"
	@echo "    help           -- print this message"
	@echo "    install        -- generate and install all files to the appropriate locations"
	@echo "    install-new-rt -- generate and install C++ glue code for the new runtime and basis"
	@echo "    new-rt         -- generate C++ glue code for the new runtime and basis"
	@echo "    install-sml    -- generate and install the SML glue code for the new runtime and basis"
	@echo "    sml            -- generate the SML glue code for the new runtime and basis"
	@echo "    classic-basis  -- generate and install C glue code for the classic basis"
	@echo "    classic-rt     -- generate and install C glue code for the classic runtime"
	@echo "    clean          -- remove any generated files"

# generate and install the class version of the basis glue
#
classic-basis:
	$(ML_IDL) -t classic $(ML_IDL_FLAGS) sml-basis.idl
	cp -p $(BASIS_C_FILES) $(OLD_RUNTIME_DIR)/smlnj-rt-lib

# generate and install the class version of the smlnj-runtime glue
#
classic-rt:
	$(ML_IDL) -t classic $(ML_IDL_FLAGS) smlnj-runtime.idl
	cp -p $(RT_C_FILES) $(OLD_RUNTIME_DIR)/sml-basis-lib

new-rt:
	for f in $(IDL_FILES) ; do \
	  $(ML_IDL) -t new-rt $(ML_IDL_FLAGS) $$f; \
	done

sml:
	for f in $(IDL_FILES) ; do \
	  $(ML_IDL) -t smlnj $(ML_IDL_FLAGS) $$f; \
	done

# generate and install generated files for new runtime system and compiler.
#
install:	install-new-rt install-sml

install-new-rt:	new-rt
	cp -p $(BASIS_CXX_FILES) $(NEW_RUNTIME_DIR)/src/smlnj-rt/basis/glue
	cp -p $(RT_CXX_FILES) $(NEW_RUNTIME_DIR)/src/smlnj-rt/runtime/glue

install-sml:
	if [ ! -d $(BASIS_IMP_DIR)/RuntimeIDL ] ; then \
	  mkdir $(BASIS_IMP_DIR)/RuntimeIDL; \
	fi
	cp -p $(SML_FILES) $(BASIS_IMP_DIR)/RuntimeIDL
	
clean:
	rm -rf *.h *.c *.hxx *.cxx *.sml *.cm

