# Nowadays this segfaults if you compile with gcc. I guess there must be some
# undefined behavior, because I failed to easily track down the problem in gdb.
# As a stopgap here we compile with Clang. Sadly the sanitizer switch doesn't
# turn up what I did wrong either.

CC = clang
SANITIZE = -fsanitize=undefined
CFLAGS = -g2 -Wall -W $(SANITIZE)
LDFLAGS = $(SANITIZE)

all: lump

lump: lump.o

test: all
	awk -f driver.awk t

clean:
	rm -f tmp.c *.o lump input output
