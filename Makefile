all: jetc

CCFLAGS=-std=c99  -Imodules -ferror-limit=999
CC=gcc

modules/TokenKindDefs.h: modules/makeTokens.sh
	cd modules && ./makeTokens.sh

jetc: programs/main.c modules/*.h
	$(CC)  $(CCFLAGS) $< -lc -o $@

jetc-fast: programs/main.c modules/*.h
	$(CC) -Os $(CCFLAGS) $< -lc -o $@

jetc-dbg: programs/main.c modules/*.h
	$(CC) -g -O0 $(CCFLAGS) $< -lc -o $@

jetc-cov: programs/main.c modules/*.h
	$(CC) -g -fprofile-arcs -ftest-coverage -O3 $(CCFLAGS) $< -lc -o $@

clean:
	@rm -r jetc jetc-fast jetc-dbg jetc-cov *.gcda *.gcno *.gcov > /dev/null 2>&1; true
