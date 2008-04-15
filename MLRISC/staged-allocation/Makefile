build-amd64:
	ml-build sources.cm Main.main test-main
	sml @SMLcmdname=test-main @SMLload=test-main
	gcc -g -m64 -c mlrisc.s
	gcc -g  -m64 -c glue.c
	gcc -g  -m64 sanity.c -o sanity
	gcc -g  -m64 -c main.c
	gcc -g  -m64 mlrisc.o glue.o main.o -o main
	./main > main.out
	./sanity > sanity.out
	diff -Naur main.out sanity.out

clean:
	rm -rf *.s *.c *.o main sanity *~ .cm *.out test-main*