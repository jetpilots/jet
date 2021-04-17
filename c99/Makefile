
CCFLAGS=-std=c99 -I../engine
CCLIBS=-lc -lm
CC=gcc


install: cjet
	cp cjet /usr/local/bin

cjet: *.[ch]
	$(CC)  $(CCFLAGS) $< $(CCLIBS) -o $@

TokenKind.h: makeTokens.sh
	cd c99 && ./makeTokens.sh

all: cjet cjet-fast cjet-dbg cjet-cov

# Each build mode (except the default) should define a specific macro.

cjet-fast: *.[ch]
	$(CC) -Os -DFAST $(CCFLAGS) main.c $(CCLIBS) -o $@

cjet-dbg: *.[ch]
	$(CC) -g -O0 -DDEBUG $(CCFLAGS) main.c $(CCLIBS) -o $@

cjet-cov: *.[ch]
	$(CC) -g -DCOVERAGE -fprofile-arcs -ftest-coverage -O3 $(CCFLAGS) main.c $(CCLIBS) -o $@

# test:  #cjet-cov
# 	./coverage.sh

clean:
	rm -r cjet cjet-fast cjet-dbg cjet-cov \
	> /dev/null 2>&1; true