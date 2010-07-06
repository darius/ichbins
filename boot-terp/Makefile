CFLAGS = -g2 -Wall -W

all: lump

lump: lump.o

test: all
	awk -f driver.awk t

clean:
	rm -f tmp.c *.o lump input output
