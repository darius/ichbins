CFLAGS = -g2 -Wall -W

all: ichbins ichbins2

clean:
	rm -f ichbins ichbins2* ichbins3* *.o tests/?.c tests/? tests/?.out

ichbins: ichbins.o

ichbins2: ichbins2.o

ichbins2.c: ichbins ichbins.scm 
	./ichbins <ichbins.scm >ichbins2.c
