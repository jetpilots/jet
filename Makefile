all:
	make -C c99

test:
	./coverage.sh

clean:
	rm -r *.gcda *.gcno *.gcov \
	*.jetr .*.jet.[dfrocx] \
	tests/*.gcda tests/*.gcno tests/*.gcov \
	c99/*.gcda c99/*.gcno c99/*.gcov \
	*.dSYM \
	> /dev/null 2>&1; true