
CCFLAGS=-std=c99 -Icore -Imodules
CCLIBS=-lc -lm
CC=gcc


jetc: programs/main.c modules/*.h
	$(CC)  $(CCFLAGS) $< $(CCLIBS) -o $@

modules/TokenKindDefs.h: modules/makeTokens.sh
	cd modules && ./makeTokens.sh

all: jetc jetc-fast jetc-dbg jetc-cov

# Each build mode (except the default) should define a specific macro.

jetc-fast: programs/main.c modules/*.h
	$(CC) -Os -DFAST $(CCFLAGS) $< $(CCLIBS) -o $@

jetc-dbg: programs/main.c modules/*.h
	$(CC) -g -O0 -DDEBUG $(CCFLAGS) $< $(CCLIBS) -o $@

jetc-cov: programs/main.c modules/*.h
	$(CC) -g -DCOVERAGE -fprofile-arcs -ftest-coverage -O3 $(CCFLAGS) $< $(CCLIBS) -o $@

test:  #jetc-cov
	./coverage.sh

clean:
	@rm -r jetc jetc-fast jetc-dbg jetc-cov \
	*.gcda *.gcno *.gcov \
	tests/*.gcda tests/*.gcno tests/*.gcov \
	programs/*.gcda programs/*.gcno programs/*.gcov \
	*.dSYM \
	> /dev/null 2>&1; true
