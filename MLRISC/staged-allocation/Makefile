ML_BUILD_FLAGS =	-Ctdp.instrument=true \$$smlnj-tdp/back-trace.cm

build-amd64:
	ml-build $(ML_BUILD_FLAGS) ../amd64/staged-allocation/sources.cm Main.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc -g -m64 -c mlrisc.s
	gcc -g  -m64 -c glue.c
	gcc -g  -m64 -c sanity.c
	gcc -g  -m64 mlrisc.o glue.o sanity.o -o sanity
	gcc -g  -m64 -c main.c
	gcc -g  -m64 mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

build-x86:
	ml-build $(ML_BUILD_FLAGS) ../x86/staged-allocation/sources.cm Main.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc -g  -c mlrisc.s
	gcc -g   -c glue.c
	gcc -g   -c sanity.c
	gcc -g   mlrisc.o glue.o sanity.o -o sanity
	gcc -g   -c main.c
	gcc -g   mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

clean:
	rm -rf *.s *.c *.o main sanity *~ .cm *.out test-main*