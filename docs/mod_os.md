# Jet Engine Manual

## Module `os`

This module contains ...

### `os.pid`
(read-only) process ID of the current program.

### `os.tid`
(read-only) thread ID of the current thread in the program. Even if you don't use multithreading, the single thread running in your program still has its own ID.

### `os.spawn(path as String)`
Launches the program at the given path. If `path` starts with a `/`, it is treated as the absolute path of the program to launch. This may be faster than simply passing a command as the `path`, in which case it will still work but will incur some overhead trying to search the user's `$PATH` environment variable.
```
os.spawn("echo HEY")
os.spawn("$os.cmdir/utility")
```

### `os.cmdir`
(read-only) The path at which the current executable exists. This is not the same as `os.pwd`.

### `os.pwd()`
Returns the current working directory.

### `os.argv[]`
(read-only) A list of arguments with which the current program was launched. `os.argv[1]` is the executable name, and `os.argv[2:end]` are the arguments.

