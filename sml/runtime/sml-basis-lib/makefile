#
# the makefile for the basis support library
#

SHELL =		/bin/sh
CC =		cc
CFLAGS =	-O
MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	libsml-basis.a
VERSION =	v-dummy

# generated files
GEN_HEADERS =	cfun-list.h cfun-proto-list.h sml-basis.h
GEN_SRCS =	sml-basis-lib.c sml-basis.c
GEN_OBJS =	sml-basis-lib.o sml-basis.o

SRCS =		$(GEN_SRCS) date.c dir.c file-io.c file-sys.c fp.c poll.c proc.c time.c cmd.c
OBJS =		$(GEN_OBJS) date.o dir.o file-io.o file-sys.o fp.o poll.o proc.o time.o cmd.o

# include directories for this level
#
OBJS_DIR =	../objs
INC_DIR =	../include
INCLUDES =	-I$(OBJS_DIR) -I$(INC_DIR)

$(LIBRARY): $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION):
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:		$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
		$(INC_DIR)/ml-c.h $(GEN_HEADERS) sml-basis.h
	$(CC) $(CFLAGS) $(DEFS) $(INCLUDES) -c $<

clean:
	rm -f v-* *.o $(LIBRARY)

devclean:	clean
	rm -f $(GEN_HEADERS) $(GEN_SRCS)

