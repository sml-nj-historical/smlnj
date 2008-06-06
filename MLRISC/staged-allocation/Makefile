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

X86_FLAGS=-m32 -march=i686 -g

build-x86:
	ml-build $(ML_BUILD_FLAGS) ../x86/staged-allocation/sources.cm Main.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc $(X86_FLAGS)  -c mlrisc.s
	gcc $(X86_FLAGS)   -c glue.c
	gcc $(X86_FLAGS)   -c sanity.c
	gcc $(X86_FLAGS) -S glue.c
	gcc $(X86_FLAGS)   mlrisc.o glue.o sanity.o -o sanity
	gcc $(X86_FLAGS)   -c main.c
	gcc $(X86_FLAGS)   mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

clean:
	rm -rf *.s *.c *.o main sanity *~ .cm *.out test-main*