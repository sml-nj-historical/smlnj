build-amd64:
	gcc -g -m64 -c mlrisc.s
	gcc -g  -m64 -c glue.c
	gcc -g  -m64 -c main.c
	gcc -g  -m64 mlrisc.o glue.o main.o -o main

clean:
	rm -rf *.s *.c *.o main *~ .cm