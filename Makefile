all:
	make -C c99

test:
	./coverage.sh

clean:
	@find . -name '*.o' -delete
	@find . -name '*.so' -delete
	@find . -name '*.gcda' -delete
	@find . -name '*.gcno' -delete
	@find . -name '*.gcov' -delete
	@find . -name 'a.out' -delete
	@find . -name '*.jetr' -delete
	@find . -name '.*.jet.[dfrocx]' -delete
	@find . -name '.*.jet.x.c' -delete
