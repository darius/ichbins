SRC = $(wildcard *.scm)

C=$(SRC:.scm=.c)
BIN=$(C:.c=)

all:    clean $(BIN)

%.c: %.scm
	if ../../ichbins <$^ >$@; then echo "ok!"; else ../../ichbins <$^; fi

clean:
	rm -rf *.c $(BIN)
