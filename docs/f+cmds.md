## `jet` command line

This document introduces the `jet` command and its various options.

### Run a program

Each program is represented by one file within the `programs` subfolder of your project. The names of these files, without the `.fp` extension, are the names of programs within your projects. You can run one of these programs by typing:
```sh
jet myprog
```
where `myprog` is an illustrative program name (replace it with your own). For convenience, and to work with your shell's default tab completion, there are minor variations that will work as intended:
```sh
jet myprog
jet myprog.fp
jet programs/myprog.fp
```
They all mean the same thing.

By default, your program runs in debug mode (don't worry -- jet *debug* mode is faster than most languages' *release* mode). But here are your alternatives:
```sh
jet -r myprog # release mode
jet -f myprog # fast mode
```

**Debug** mode adds jet internal debug information, such as function call site strings, extended backtraces, extended error information, rich assertions, etc. Safety features for stack overflow checking, array bounds checking and null pointer checking are enabled. The backend C compiler flags are `-g -O3`.

**Release** mode leaves out this debug information, but keeps the safety features. The backend C compiler flags are `-O3`.

**Fast** mode leaves out both the debug information and the safety features. The backend C compiler flags are `-O3 -ffast-math -march-native` (that's `-fast -xHost` for `icc`).

### Build an executable

The options `-br`, `-bf`, and `-bd` will build an executable in the release, fast and debug configurations respectively. The executable is self-contained, and except on macOS, is a statically linked executable. That means not only does it not have any shared library dependencies, but it also **cannot possibly have** any shared library dependencies.

You can run the executable like any other, distribute it in binary packages for the system it was built for, and so on.

What `jet myprog` does is simply (re-)build an executable, if the relevant source files have changed, and run this executable. The overhead should be small, and is made up primarily of `make`'s scan over the source files. This is practical when you are developing and need to repeatedly run or test your code.

But when you need to run your program in a loop, or when you otherwise know the source hasn't changed, save yourself the trouble and run the executable directly. Even the debug executable is fully self-contained, rich assertions, extended debug info and all. Everything is **inside** the executable. `jet` does no magic.

Of course, you can load the executable into your favourite debugger and step through it like a regular C program. At present, there is no attempt to demangle the generated function names, or link them to their original jet definitions, but they are simple and readable anyway.

### Build the entire project (all programs)
```sh
jet build
```

In this mode, if the compiler finds a module that is not used by any program, it reports an 'unused module' warning for it. Try not to have these, since they represent a pointless cognitive load for users trying to read your source code. Instead, move them into `scratch` if you must really keep them around.

### Publish a package

With one single command, you can publish your package to all configured destinations. This can include source code repositories (by default it runs `git push` for all remotes), but also binary repositories for linux (Most distributions based on `apt`, `yum`, `apk`, and other popular package managers) and macOS (`Homebrew`, `Fink`, `MacPorts`). The entire project is (re-)built automatically as and if required. The version number is incremented, or can be explicitly specified.

**All tests are run, and must pass in order for the submission to succeed.**

```sh
jet publish
jet publish %v-rc2 # custom version.
```
Here a number of special tokens can be used:
Token | Expansion
---   | ---
`%v`  | Version
`%d`  | Date
`%t`  | Time
`%D`  | Timestamp
`%c`  | Commit hash (short)
`%C`  | Commit hash (full)

### Folder structure
Every jet project has a corresponding base folder.
- The source code repository is configured for this base folder.
- None of the files outside this base folder are related to the project.
- The name of the base folder is the *short name* of the project.

There are **6** standard subfolders in the base folder: `modules`, `programs`, `docs`, `tests`, `build`, `external`.

There is **one** standard file: `fplus.yml` in the base folder.

Folder | Purpose
---    | ---
`modules`    | All your source code modules, hierarchically organized.
`programs` | Your programs. Programs may "`use`" whichever modules they need.
`docs` | Documentation for your source code.
`dependencies` | External projects on which this project depends. Each external project must be an jet project folder, for which the source code is available.
`tests` | Tests for your modules and programs.
`build` |
- `.fpb` | Hidden -- temporary build files, `.o` files, etc. Excluded from git.
- `release` | Built executables, in `release` mode.
- `debug` | Built executables, in `debug` mode.
- `fast` | Built executables, in `fast` mode.

Controversial: For version stability, it is not recommended to have symlinks in `external` that simply point to other local projects, because underlying changes in these projects may annoyingly break the dependent projects. Instead, you should use git submodules and check out the correct tag, but even with this approach you do not have a record of the last stable tags tested and known to work. The best way is to copy the source tree. Anyway.

`fplus.yml` contains:

```yaml
about: FTWeb is a state of the art web browser written in jet with a remarkably small memory footprint, blazing fast performance and latest web features for privacy and total user control over CSS, scripts, cookies, ads, tracking and hardware access.

authors:
- Blaise Pascal <blaise@pascal.org>
- Albert Einstein <nop@princept.edu>

websites:
  home: "https://www.ftweb.org"
  docs: "https://www.ftweb.org/docs"
  download: "https://www.ftweb.org/releases"

license:
- MIT
- Apache

publish:
- alpine
- ubuntu
- macports
- homebrew
- fink
- fedora
- opensuse
- freebsd
- debian
```

Dependent external packages are to be included directly in the deps folder, as git submodules or as normal files and folders. "Package manager" ideology is not encouraged here, since it is a highway to dependency hell. If you really want to track versions across dependencies, use git submodules and check out the tags you need. Better yet, name the deps folders with their versions. e.g. `mypkg-v3.3.2-rc3-b4` will be correctly identified when you do

```fortran
use ff = modu.mod2.base  ! import package with alias
use cm = modu.mod2.cellmapper  ! import from package
use b2 = somemod.submod.mod2 ! import from current project
```
