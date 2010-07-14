
run: list_of_trees.o

list_of_trees.cpp: generate_code list_of_trees.scm
	./generate_code <list_of_trees.scm >list_of_trees.cpp

%.c: %.scm
	if ../../ichbins <$^ >$@; then echo "ok!"; else ../../ichbins <$^; fi

clean:
	rm -f generate_code generate_code.o list_of_trees.cpp list_of_trees.o
