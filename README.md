[![Gitter](https://img.shields.io/gitter/room/jetpilots/jet?logo=gitter&style=flat)]()
[![Discord](https://img.shields.io/discord/808407017710026774?label=Discord&logo=discord&style=flat)]()
[![Twitter](https://img.shields.io/twitter/follow/JetpilotsDev?label=JetPilots&logo=twitter&style=flat)]()

![Logo](https://avatars.githubusercontent.com/u/71646691?s=200&v=4)

# Jet
> This project is in active development. Completed items are marked with a checkbox below.

[![CircleCI](https://img.shields.io/circleci/build/gh/jetpilots/jet?logo=circleci&label=&style=flat)]()
[![Travis](https://img.shields.io/travis/com/github/jetpilots/jet?logo=travis&label=&style=flat)]()
[![Appveyor](https://img.shields.io/appveyor/build/sushpa/jet?logo=appveyor&label=&style=flat)]()
[![GitHub-Build](https://img.shields.io/github/workflow/status/jetpilots/jet/build?logo=github&label=)]()

[![Codecov](https://img.shields.io/codecov/c/gh/jetpilots/jet?logo=codecov&label=&style=flat)]()
[![Coveralls](https://img.shields.io/coveralls/github/jetpilots/jet?logo=coveralls&label=&style=flat)]()

[![Coverity](https://img.shields.io/coverity/scan/22583?logo=coverity&label=coverity&style=flat)]()
[![Codacy](https://img.shields.io/codacy/grade/91cc254e45394a278c96de0f02151661?label=quality&style=flat)]()
[![Codeclimate](https://img.shields.io/codeclimate/maintainability/jetpilots/jet?style=flat)]()

[![Issues](https://img.shields.io/github/issues-raw/jetpilots/jet?&style=flat)]()
[![Closed](https://img.shields.io/github/issues-closed-raw/jetpilots/jet?style=flat)]()
[![License](https://img.shields.io/github/license/jetpilots/jet?&style=flat)]()
[![Lines](https://img.shields.io/tokei/lines/github/jetpilots/jet?style=flat)]()

## A modern high-performance language for science

A modern, simple language with all the features you need for scientific computing. Fast multidimensional arrays with slicing and broadcasting syntax, simple modular program structure, powerful assertions and implied contracts, and in-depth compile-time analysis for several major classes of bugs, combined with run-time diagnostics when needed. 

Easily create native console or GUI applications for Linux, Windows, macOS on x86-64 or ARM, with blazing-fast performance, from a single codebase.

### Designed for high productivity, high maintainability
- [x] Static and strong typing, but writing code almost feels like a dynamically typed language
- [ ] Array notation, slicing, broadcast and manipulation as in Fortran90, MATLAB, numpy, Julia
- [x] Language syntax designed to minimise complexity and reduce reader's cognitive load
- [x] Clearly defined universal structure makes code and projects instantly understandable
- [x] Predictable, unambiguous language elements e.g. operators and builtin functions
- [x] Write code sloppily and let the linter add types, apply fixes, enforce standard format
- [ ] Builtin unit testing with test dependencies and 1-command build and test

### Built for high-performance computing
- [x] Performance on par with, and often exceeding that of handwritten C or Fortran code
- [x] Threads and message passing are both first-class language features (over pthreads and MPI)
- [ ] Easily write manual, portable SIMD vectorized code without directly using intrinsics
- [ ] GPU programming support as a first-class language feature (over OpenACC or OpenCL)
- [ ] Automatic vectorisation and parallelisation over loops and array operations

### Build APIs completely transparently
- [ ] Automatically (de)serialise any data into JSON, XML, YAML, BSON, protobuf
- [ ] Auto-generate RPC infrastructure for any function over REST, SOAP, gRPC

### Build native applications with rich GUIs
> Implementation on macOS with Cocoa is underway. Xcode is not required.
- [ ] Full native GUI with native controls, chart views and plotting (Cocoa/GTK/UWP)
- [ ] Declarative syntax for building GUI layouts and defining behaviours
- [ ] Data binding between native types and GUI elements for reactive programming

### A comprehensive standard library: Jet engine
- [x] High-performance primitives: String, arrays, trees, K-D trees, hashtables, graphs
- [ ] Algorithms and data structures for high-dimensional large-scale data analysis
- [ ] Signal processing, regression, peak-finding and curve-fitting, time-series analysis
- [ ] Linear algebra and vector calculus directly on arrays, with implicit parallelism
- [ ] Network library (HTTP, FTP, IMAP, POP, SMTP, SSH, SFTP) + TLS for all protocols
- [ ] Data library: JSON, XML, YAML, HDF5, MAT, binary formats, domain-specific formats
- [ ] Image and PDF processing, drawing, file format input/output and conversion

### Free of dependencies
- [x] No dependencies, except a system C compiler and libc of your choice
- [ ] Easily generate fully static executables on any platform (except macOS)

### Extensive program analysis and abstract interpretation for safety and security
- [ ] Catch out-of-bounds access, null access, zero division, stack overflows at compile-time
- [x] Catch them at runtime and fail with detailed, localized diagnostic messages
- [x] Rich assertions that print subexpression trees on failure; detailed stack traces
- [ ] Use assertions as hints to the compiler about program state for checking facts

### Build, test, deploy with a single command
- [x] The `jet` command allows building, running, testing, profiling, debugging and packaging
- [x] Run source files directly as if they were scripts. Compilation is fully behind-the-scenes
- [x] Fully automatic line-level profiling and coverage for production runs and/or unit tests

### One-command automatic package generation
- [ ] Linux package managers
- [ ] homebrew on macOS
- [ ] Chocolatey on Windows
- [ ] Docker images

### Works great with VS Code
- [x] Syntax highlighting
- [x] Snippets
- [ ] Run, build, debug, test

### Platform support
- [ ] Windows
- [x] macOS x86-64
- [ ] macOS M1
- [x] Linux x86-64
- [ ] Linux arm/aarch64
- [ ] BareMetal OS x86-64

## Licensing
The project is licensed under GPLv3. Contributors are required to follow the code of conduct and to assign copyright to the lead developer for license enforcement. Commercial licensing will be available upon request once we are closer to release.
