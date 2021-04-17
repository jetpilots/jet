
CCFLAGS=-std=c99 -I../engine -ferror-limit=999
CCLIBS=-lc -lm
CC=gcc


install: cjet
	cp cjet /usr/local/bin

cjet: *.[ch] tokenkind.h
	$(CC)  $(CCFLAGS) main.c $(CCLIBS) -o $@

tokenkind.h: makeTokens.sh
	./makeTokens.sh

all: cjet cjet-fast cjet-dbg cjet-cov

# Each build mode (except the default) should define a specific macro.

cjet-fast: *.[ch] tokenkind.h
	$(CC) -Os -DFAST $(CCFLAGS) main.c $(CCLIBS) -o $@

cjet-dbg: *.[ch] tokenkind.h
	$(CC) -g -O0 -DDEBUG $(CCFLAGS) main.c $(CCLIBS) -o $@

cjet-cov: *.[ch] tokenkind.h
	$(CC) -g -DCOVERAGE -fprofile-arcs -ftest-coverage -O3 $(CCFLAGS) main.c $(CCLIBS) -o $@

# test:  #cjet-cov
# 	./coverage.sh

clean:
	rm -r cjet cjet-fast cjet-dbg cjet-cov \
	> /dev/null 2>&1; true