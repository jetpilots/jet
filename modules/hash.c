#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "jet_base.h"
#include "hash.h"

int main() {
    // TODO: remove the puterr from Dict_put. What is the point of knowing
    // the status as it was before the PUT call?
    int puterr;
    jet_Set(UInt32) S = {}; // = jet_Set_init(UInt32)();
    jet_Set_put(UInt32)(&S, 42, &puterr);
    jet_Set_put(UInt32)(&S, 63, &puterr);
    jet_Set_put(UInt32)(&S, 22, &puterr);
    jet_Set_put(UInt32)(&S, 77, &puterr);
    jet_Set_foreach(&S, UInt32 k, { printf("%d\n", k); });
    for (int i = 0; i < 100; i += 11)
        printf("%d: %d\n", i, jet_Set_has(UInt32)(&S, i));
    jet_Set_freedata(UInt32)(&S);

    static const char* strs[] = { "40", "63", "125", "6", "12", "max", "bang",
        "lacasadepapel", "wheehow", "40ten" };

    jet_Set(CString) S2 = {}; // = jet_Set_init(UInt32)();
    jet_Set_put(CString)(&S2, "42", &puterr);
    jet_Set_put(CString)(&S2, "63", &puterr);
    jet_Set_put(CString)(&S2, "22", &puterr);
    jet_Set_put(CString)(&S2, "lacasadepapel", &puterr);
    jet_Set_foreach(&S2, CString k, { printf("%s\n", k); });
    for (int i = 0; i < 10; i++)
        printf("%s: %d\n", strs[i], jet_Set_has(CString)(&S2, strs[i]));
    jet_Set_freedata(CString)(&S2);
    return 0;
}