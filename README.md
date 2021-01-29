
# Jet


A modern, simple language with all the features you need for scientific computing. Fast multidimensional arrays with slicing syntax, simple modular program structure, powerful assertions and implied contracts, and in-depth compile-time analysis for all major classes of bugs.

- No pointers, no null. Variables are always initalized.
- Static and strong typing, but writing code almost feels like a dynamically typed language.
- No runtime errors for out-of-bounds access, null object access, zero division, or stack overflows.
- Array notation that you know and love from Fortran 90, MATLAB, numpy, Julia.
- Blazing fast compiler as well as language builtins and standard library routines.

Performance is on par with and often exceeds that of handwritten C or Fortran code, ahead of LuaJIT and Julia, far ahead of Python, Matlab or Octave, with the language being much simpler and providing much of the functionality you need for scientific computing:
- Full native GUI with native controls, chart views and plotting (Cocoa/GTK/UWP)
- Network library (HTTP, FTP, IMAP, POP, SMTP, SSH, SFTP) + TLS for all protocols
- Data library: JSON, XML, YAML, HDF5, MAT, binary formats, domain-specific formats
- Rich assertions that print subexpression trees on failure; detailed stack traces
- Builtin unit testing with test dependencies and 1-command build and test

There are no dependencies, except (optionally) a system C compiler and libc of your choice, typically your system's clang or gcc. You can also easily generate fully static executables on any platform if you want absolutely no dependencies, and Jet ships with a version of pcc if you don't even have a C compiler on your target system.

Jet is in active development. While it can compile simple programs, not all features are implemented yet.
