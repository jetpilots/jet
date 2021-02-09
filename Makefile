
CCFLAGS=-std=c99  -Imodules
CCLIBS=-lc -lm
CC=gcc


jetc: programs/main.c modules/*.h
	$(CC)  $(CCFLAGS) $< $(CCLIBS) -o $@

modules/TokenKindDefs.h: modules/makeTokens.sh
	cd modules && ./makeTokens.sh

all: jetc jetc-fast jetc-dbg jetc-cov

jetc-fast: programs/main.c modules/*.h
	$(CC) -Os $(CCFLAGS) $< $(CCLIBS) -o $@

jetc-dbg: programs/main.c modules/*.h
	$(CC) -g -O0 $(CCFLAGS) $< $(CCLIBS) -o $@

jetc-cov: programs/main.c modules/*.h
	$(CC) -g -fprofile-arcs -ftest-coverage -O3 $(CCFLAGS) $< $(CCLIBS) -o $@

test:  #jetc-cov
	./coverage.sh

clean:
	@rm -r jetc jetc-fast jetc-dbg jetc-cov \
	*.gcda *.gcno *.gcov \
	tests/*.gcda tests/*.gcno tests/*.gcov \
	*.dSYM \
	> /dev/null 2>&1; true
