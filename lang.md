# Jet 5: Language Manual

This is the up-to-date reference for Jet 5, a modern high-performance language for scientific computing. Jet is incredibly simple, and typically takes no more than 30 minutes to learn, depending on prior programming experience. In any case, there's a helpful Gitter and Discord channel where you can always ask questions, get help, or discuss more specific topics with the friendly Jet community.

This document doesn't discuss the inner workings of Jet, such as compiler or runtime implementation. See the [Jet 5 Internals](internals.md) document instead, if you wish to read about how Jet works.

Don't forget to read the [Jet Engine 5 Manual](engine.md) after this document, to know about the extensive built-in features of Jet and how to use them instead of reinventing the wheel (or the engine).

### Contents

Code structure

All executable code is contained within **function**s in Jet. A **variable** is a piece of data of a certain type. Jet has several basic and compound **type**s to help organize and structure your data. A **module** is the fundamental unit of Jet source code, containing definitions of functions, types, and variables. Each module corresponds uniquely to a source file with the extension `.jet`. Modules may **import** other modules to use code defined within them.

Project structure

Each project must have a minimum of two folders whose names, by convention, are:

- `programs`: this folder holds source files that define a start point of execution. A project may have multiple programs.
- `modules`: this folder holds all the source files that are used by the project's programs. The bulk of a project's code resides here.

There are some more folders with specific meaning. Strictly speaking these are optional, but most projects have them, and all good projects need them:

- `docs`: holds documentation and notes about the source code and its usage.
- `tests`: holds modules whose code has the sole purpose of testing the code of other modules.

Some specific folders are created automatically:

- `build`: holds final built executables as well as temporary files generated during the build process.

All Jet projects use this folder structure. This makes Jet projects instantly recognizable: users know where to go to find what they want, even in a completely new project.

Basic data types

Number, Range

String, Byte

Boolean



Compound data types

Colour

DateTime



Collections

Functions

Operators

Control flow

Error handling

Parallelism

Physical Units



