

[![Gitter](https://img.shields.io/gitter/room/jetpilots/jet?logo=gitter&style=flat)]()
[![Discord](https://img.shields.io/discord/808407017710026774?label=Discord&logo=discord&style=flat)]()
[![Twitter](https://img.shields.io/twitter/follow/JetpilotsDev?label=JetPilots&logo=twitter&style=flat)]()

![Logo](https://avatars.githubusercontent.com/u/71646691?s=200&v=4)

# Jet



[![CircleCI](https://img.shields.io/circleci/build/gh/jetpilots/jet?logo=circleci&label=&style=flat)]()
[![Travis](https://img.shields.io/travis/com/github/jetpilots/jet?logo=travis&label=&style=flat)]()
[![Appveyor](https://img.shields.io/appveyor/build/sushpa/jet?logo=appveyor&label=&style=flat)]()
[![Codecov](https://img.shields.io/codecov/c/gh/jetpilots/jet?logo=codecov&label=&style=flat)]()
[![Coveralls](https://img.shields.io/coveralls/github/jetpilots/jet?logo=coveralls&label=&style=flat)]()

[![Coverity](https://img.shields.io/coverity/scan/22583?logo=coverity&label=coverity&style=flat)]()
[![Codacy](https://img.shields.io/codacy/grade/91cc254e45394a278c96de0f02151661?label=quality&style=flat)]()
[![Codeclimate](https://img.shields.io/codeclimate/maintainability/jetpilots/jet?style=flat)]()




[![Issues](https://img.shields.io/github/issues-raw/jetpilots/jet?&style=flat)]()
[![Closed](https://img.shields.io/github/issues-closed-raw/jetpilots/jet?style=flat)]()
[![License](https://img.shields.io/github/license/jetpilots/jet?&style=flat)]()
[![Lines](https://img.shields.io/tokei/lines/github/jetpilots/jet?style=flat)]()
## A high-performance language for science



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
