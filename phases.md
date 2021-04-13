##### (#1)  Lint (`-l`)

`parse -> resolve -> analyse -> lint? -> genc? -> buildobj?`

- Prints errors to `stderr`. Checks all kinds of errors including strict spacing.

- Prints prettified output (walking AST) to `stdout`, or if **`-i`** is specified, to the original file, overwriting it.
- If there are errors leading to an invalid AST, pretty-prints using a token-based formatter instead.

- *(todo)* Writes a binary file with the AST for use next time if there were no errors.



>**Cannot proceed beyond this phase unless the AST is error-free.**



##### (#2)  Generate `.c` (`-c`)

`parse -> resolve -> analyse -> genc? -> buildobj?`

- Happens automatically after linting, if there were no errors. Always writes to file `<filename>.jc`. and `<filename>.jh`.

- If this phase is called by itself (using **`-c`**) it stops on the first error generated. 

- In build scripts this is the recommended phase to invoke, since failures will be fast. Errors will be checked anyway and the overhead of dumping reformatted code will be avoided.

- `<filename>.o` is built (debug mode), allowing a fast build for **#4a/4b**. `-w` skips this. 



##### (#3)  Build executable (`-b`)

- Doesn't happen automatically. Only invokes `make`, it will in turn invoke `jetc` if needed.
- `-br` and `-bf` build release or fast mode targets
- If no target is specified, build all of them (in `programs`).



##### (#4a)  Run target

- Starts from **#3** and runs the built target (in debug mode; use `-r` or `-f` for release or fast instead).

- This is the default mode for `jetc` when no options are provided.

  

##### (#4b) Test (`-t`)

- Runs **#3** to prepare executables (and ensure `.o` files are up to date)
- Builds test wrappers and test targets.
- Runs test targets and collects test output.
- Runs test reporter to proide summary and reporting.





```
usage: jetc [options] <filename>

With no options, builds the executable for the given filename,
(also building all dependencies), and runs it in debug mode.
    
Options:

    -r, -z
    	Set release or fast mode for building or running.

    -b 
    	Just build the target(s) but don't run them.
    	
    -f
    	Force building all files, even if they are up to date.

	-l
        Lint, reformat and compile the file, reporting errors.
        The reformatted source is printed to standard output, 
        and errors are printed to the standard error stream.
	-i  
		(with -l) Reformat the source file in-place.
		
	-c  Like -l, but performs no reformatting, and stops on the
    	first error. This is the recommended way to invoke jetc
    	from within build scripts, so that failure can be fast.
    
    -w
    	(with -c or -l) Skip generating object files (they will
        then be generated during the next build). You may find
        this useful when repeatedly linting (e.g. from an IDE).
        
    -n <N>
    	Run the executable with N processes (over MPI). If N is
    	not given or is zero, the number of processes is taken
    	from the cluster environment. If this fails, the number
    	of physical CPUs on the launching host is used instead.
    
    -m <M>
    	Set a maximum number of M threads for the process. If
    	M is not given or is zero, the number of physical CPUs
    	is used instead.
    	
    -x
    	Disable the reference-counting based garbage collector.
    	
    -v
    	Generate coverage instrumentation.
    	
    -p
    	Generate profile information when the run finishes. The
    	information is placed in <filename>.jetp.
```



How about `jetcomp`, `jetlint`, `jetfmt`, `jettest`, `jetrun`, `jetmake`, ?

Nope endless reparsing