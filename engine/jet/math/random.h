// Jet standard library random() functions (may be replaced by above)
uint64_t makerandstate(uint64_t id) {
    FILE* urandom = fopen("/dev/urandom", "r");
    setvbuf(urandom, NULL, _IONBF, 0); // turn off buffering
    // setup state buffer
    union {
        uint64_t u64;
        short s[4];
    } u;
    // actually it only needs 3 shorts.
    // unsigned short randstate[4] = {};
    // fgetc() returns a `char`, we need to fill a `short`
    u.s[0] = (fgetc(urandom) << 8) | fgetc(urandom);
    u.s[1] = (fgetc(urandom) << 8) | fgetc(urandom);
    u.s[2] = (fgetc(urandom) << 8) | fgetc(urandom);
    u.s[3] = (fgetc(urandom) << 8) | fgetc(urandom);
    // cleanup urandom
    fclose(urandom);
    return u.u64 | id;
}
thread_local uint64_t __randstate;
// ^^ when a thread launches, somehow it should update __randstate.
// ^ I guess main can do it but other threads should have it in a wrapper (which
// Thread_launch calls instead of the actual function)

double frand(uint64_t* state) { return erand48((unsigned short*)state); }

double random1() { return frand(&__randstate); }
double randomd() { return ((random1() - 0.5) * __DBL_MAX__) * 2; }
double randomi() { return (intptr_t)randomd(); }
double randomu() { return (uintptr_t)randomd(); }