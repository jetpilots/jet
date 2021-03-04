#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "base.h"
#include "hash.h"

int main() {
    // TODO: remove the puterr from Dict_put. What is the point of knowing
    // the status as it was before the PUT call?
    int puterr;
    Set(UInt32) S = {}; // = Set_init(UInt32)();
    Set_put(UInt32)(&S, 42, &puterr);
    Set_put(UInt32)(&S, 63, &puterr);
    Set_put(UInt32)(&S, 22, &puterr);
    Set_put(UInt32)(&S, 77, &puterr);
    Set_foreach(&S, UInt32 k, { printf("%d\n", k); });
    for (int i = 0; i < 100; i += 11)
        printf("%d: %d\n", i, Set_has(UInt32)(&S, i));
    Set_freedata(UInt32)(&S);

    static const char* strs[] = { "40", "63", "125", "6", "12", "max", "bang",
        "lacasadepapel", "wheehow", "40ten" };

    Set(CString) S2 = {}; // = Set_init(UInt32)();
    Set_put(CString)(&S2, "42", &puterr);
    Set_put(CString)(&S2, "63", &puterr);
    Set_put(CString)(&S2, "22", &puterr);
    Set_put(CString)(&S2, "lacasadepapel", &puterr);
    Set_foreach(&S2, CString k, { printf("%s\n", k); });
    for (int i = 0; i < 10; i++)
        printf("%s: %d\n", strs[i], Set_has(CString)(&S2, strs[i]));
    Set_freedata(CString)(&S2);
    return 0;
}