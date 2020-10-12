all: jetc

CCFLAGS=-std=c99 -Werror -Imodules
CC=gcc

modules/TokenKindDefs.h: modules/makeTokens.sh
	cd modules && ./makeTokens.sh

jetc: programs/main.c modules/*.h
	$(CC) -Os $(CCFLAGS) $< -lc -o $@

jetc-fast: programs/main.c modules/*.h
	$(CC) -Os $(CCFLAGS) $< -lc -o $@

jetc-dbg: programs/main.c modules/*.h
	$(CC) -g -O0 $(CCFLAGS) $< -lc -o $@

jetc-cov: programs/main.c modules/*.h
	$(CC) -g -fprofile-arcs -ftest-coverage -O0 $(CCFLAGS) $< -lc -o $@

clean:
	@rm -r jetc jetc-fast jetc-dbg jetc-cov > /dev/null 2>&1; true
