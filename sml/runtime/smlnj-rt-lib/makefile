#
# the makefile for the C runtime library
#

SHELL =		/bin/sh
CC =		cc
CFLAGS =	-O
MAKE =		make
AR =		ar
ARFLAGS =	rcv
RANLIB =	ranlib

LIBRARY =	smlnj-runtime.a
VERSION =	v-dummy

# generated files
GEN_HEADERS =	cfun-list.h cfun-proto-list.h smlnj-runtime.h
GEN_SRCS =	smlnj-runtime-lib.c smlnj-runtime.c
GEN_OBJS =	smlnj-runtime-lib.o smlnj-runtime.o

SRCS =		$(GEN_SRCS) alloc-code.c set-timer.c get-quantum.c set-time-array.c \
		interval-tick.c set-int-timer.c export-heap.c list-signals.c get-sig-state.c \
		set-sig-state.c get-sig-mask.c set-sig-mask.c pause.c raw-argv.c shift-argv.c \
		sys-info.c gc-control.c debug.c dummy.c record-concat.c record1.c \
		blast-in.c blast-out.c mk-exec.c mk-literals.c
OBJS =		$(GEN_OBJS) alloc-code.o set-timer.o get-quantum.o set-time-array.o \
		interval-tick.o set-int-timer.o export-heap.o list-signals.o get-sig-state.o \
		set-sig-state.o get-sig-mask.o set-sig-mask.o pause.o raw-argv.o shift-argv.o \
		sys-info.o gc-control.o debug.o dummy.o record-concat.o record1.o \
		blast-in.o blast-out.o mk-exec.o mk-literals.o

# include directories for this level
#
OBJS_DIR =	../objs
INC_DIR =	../include
INCLUDES =	-I$(OBJS_DIR) -I$(INC_DIR)
GC_INCLUDES =	$(INCLUDES) -I../gc

$(LIBRARY): $(VERSION) $(OBJS)
	rm -rf $(LIBRARY)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS)
	$(RANLIB) $(LIBRARY)

$(VERSION):
	($(MAKE) MAKE="$(MAKE)" clean)
	echo "$(VERSION)" > $(VERSION)

.c.o:		$(INC_DIR)/ml-unixdep.h $(INC_DIR)/ml-base.h $(INC_DIR)/ml-values.h \
		$(INC_DIR)/ml-c.h $(GEN_HEADERS) smlnj-runtime.h
	$(CC) $(CFLAGS) $(DEFS) $(GC_INCLUDES) -c $<

clean:
	rm -f v-* *.o $(LIBRARY)

devclean:	clean
	rm -f $(GEN_HEADERS) $(GEN_SRCS)

