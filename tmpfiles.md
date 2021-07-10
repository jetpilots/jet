# Jet build files

A number of temporary build files are generated and cached. 

They can be cleaned up with `jet -x` or `find . -name '.*.jet.*' -delete`.

| Kind                                | Name            | Ext   |
| ----------------------------------- | --------------- | ----- |
| (original) Source Code              | file.jet        |       |
| Emitted C                           | .file.jet.c     | c     |
| Emitted H                           | .file.jet.h     | h     |
| Compiled object                     | .file.jet.OMG.o | OMG.o |
| Uncompiled binary module (C source) | .file.jet.u     | u     |
| Compiled binary module              | .file.jet.b     | b     |
| Emitted H (temporary)               | .file.jet._     | _     |
| Executable                          | .file.jet.OMG.x | OMG.x |
| Monolithic C wrapper `main()`       | .file.jet.m     | m     |
| Incremental C wrapper `main()`      | .file.jet.i     | i     |

`O`: `0`, `1`, `2`, `3` or `s` (optim. level)

`M`: `m` if monolithic

`G`: `g` if debug symbols

Note that the C compiler is always invoked with either `-x c` or `-x objective-c` as needed, so the extensions should not play a role.

