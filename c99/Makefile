
CCFLAGS=-std=c99 -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -O3 -Dmonostatic=static -I../engine #-D_POSIX_C_SOURCE=199309L
CCLIBS=-lc -lm
CC=gcc
COMMITHASHXX=`git log -1 --format=%h`
COMMITHASH=`git rev-parse HEAD`
COMMITDATE=`git log -1 --format=%cd --date=format:'%Y.%m.%d'`
PREFIX=/usr/local/bin

COMMITINFO="-DCOMMITHASH=\"$(COMMITHASH)\"" "-DCOMMITDATE=\"$(COMMITDATE)\""

install: cjet tokcnt

# mv cjet $(PREFIX) 2> /dev/null || sudo mv cjet $(PREFIX)

cjet: main.c *.h TokenKind.h
	$(CC)  $(CCFLAGS) $(COMMITINFO) main.c $(CCLIBS) -o $@

cjetd: serv.c *.h TokenKind.h
	$(CC)  $(CCFLAGS) $(COMMITINFO) serv.c $(CCLIBS) -o $@

tokcnt: tokcnt.c *.h TokenKind.h
	$(CC)  $(CCFLAGS) $(COMMITINFO) tokcnt.c $(CCLIBS) -o $@

TokenKind.h: makeTokens.sh
	./makeTokens.sh

all: cjet cjet-fast cjet-dbg cjet-cov

# Each build mode (except the default) should define a specific macro.

cjet-fast: *.[ch] TokenKind.h
	$(CC) $(CCFLAGS) $(COMMITINFO) -Os -DFAST main.c $(CCLIBS) -o $@

cjet-dbg: *.[ch] TokenKind.h
	$(CC) $(CCFLAGS) $(COMMITINFO) -g -O0 -DDEBUG main.c $(CCLIBS) -o $@

cjet-cov: *.[ch] TokenKind.h
	$(CC) $(CCFLAGS) $(COMMITINFO) -g -DCOVERAGE -fprofile-arcs -ftest-coverage -O0  main.c $(CCLIBS) -o $@

# test:  #cjet-cov
# 	./coverage.sh

clean:
	rm -r cjet cjet-fast cjet-dbg cjet-cov \
	> /dev/null 2>&1; true
