all:
	make -C c99
	make -C cpp11
	make -C self

test:
	./coverage.sh

clean:
	rm -r *.gcda *.gcno *.gcov \
	*.jetr .*.jet.[dfrocx] \
	tests/*.gcda tests/*.gcno tests/*.gcov \
	*.dSYM \
	> /dev/null 2>&1; true