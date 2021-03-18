#include <stdio.h>
#include <stdint.h>
__thread const char* _g_rbp;
__thread const char* _g_rsp;

extern void* getrbp(void);
extern const char* getrsp(void);
extern uintptr_t getrbpsp(void);

// actually you should compare ptrs
#define PRINTSTACKUSAGE _g_rsp=getrsp(); printf("usage: %lu B\n", _g_rbp- _g_rsp)

void recur(int x) {
PRINTSTACKUSAGE;
recur(x);
}


struct boo {};
int main(int argc, char* argv[]) {
_g_rbp = getrbp();
    struct boo _bp[0];
    printf("argc: %p\n",&argc);
    printf("argv: %p\n",&argv);
    printf("boo: %p\n",_bp);
    PRINTSTACKUSAGE;
    recur(argc);
return 0;
}
