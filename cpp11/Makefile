
CCFLAGS=-std=c++11 -I../engine -ferror-limit=999
CCLIBS=-lc -lm
CC=g++


install: ccjet
	cp ccjet /usr/local/bin

ccjet: *.[ch]pp
	$(CC)  $(CCFLAGS) main.cpp $(CCLIBS) -o $@

tokenkind.h: makeTokens.sh
	./makeTokens.sh

all: ccjet ccjet-fast ccjet-dbg ccjet-cov

# Each build mode (except the default) should define a specific macro.

ccjet-fast: *.[ch]pp tokenkind.h
	$(CC) -Os -DFAST $(CCFLAGS) main.cpp $(CCLIBS) -o $@

ccjet-dbg: *.[ch]pp tokenkind.h
	$(CC) -g -O0 -DDEBUG $(CCFLAGS) main.cpp $(CCLIBS) -o $@

ccjet-cov: *.[ch]pp tokenkind.h
	$(CC) -g -DCOVERAGE -fprofile-arcs -ftest-coverage -O3 $(CCFLAGS) main.cpp $(CCLIBS) -o $@

# test:  #ccjet-cov
# 	./coverage.sh

clean:
	rm -r ccjet ccjet-fast ccjet-dbg ccjet-cov \
	> /dev/null 2>&1; true