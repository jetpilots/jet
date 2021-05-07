
CCFLAGS=-std=c99 -I../engine -O3 -ferror-limit=999
CCLIBS=-lc -lm
CC=gcc


install: cjet cjetd tokcnt
	cp cjet cjetd /usr/local/bin

cjet: main.c *.h TokenKind.h
	$(CC)  $(CCFLAGS) main.c $(CCLIBS) -o $@

cjetd: serv.c *.h TokenKind.h
	$(CC)  $(CCFLAGS) serv.c $(CCLIBS) -o $@

tokcnt: tokcnt.c *.h TokenKind.h
	$(CC)  $(CCFLAGS) tokcnt.c $(CCLIBS) -o $@

TokenKind.h: makeTokens.sh
	./makeTokens.sh

all: cjet cjet-fast cjet-dbg cjet-cov

# Each build mode (except the default) should define a specific macro.

cjet-fast: *.[ch] TokenKind.h
	$(CC) -Os -DFAST $(CCFLAGS) main.c $(CCLIBS) -o $@

cjet-dbg: *.[ch] TokenKind.h
	$(CC) -g -O0 -DDEBUG $(CCFLAGS) main.c $(CCLIBS) -o $@

cjet-cov: *.[ch] TokenKind.h
	$(CC) -g -DCOVERAGE -fprofile-arcs -ftest-coverage -O0 $(CCFLAGS) main.c $(CCLIBS) -o $@

# test:  #cjet-cov
# 	./coverage.sh

clean:
	rm -r cjet cjet-fast cjet-dbg cjet-cov \
	> /dev/null 2>&1; true