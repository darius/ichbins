# Some optional sanity checking:
#CC = clang
#SANITIZE = -fsanitize=address
#SANITIZE = -fsanitize=undefined
SANITIZE = 

CFLAGS = -g2 -Wall -W $(SANITIZE)
LDFLAGS = $(SANITIZE)

all: lump

lump: lump.o

test: all
	awk -f driver.awk t

clean:
	rm -f tmp.c *.o lump input output
