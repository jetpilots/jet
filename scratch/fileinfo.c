typedef struct {
    const char *name, *target;
    const unsigned long long size : 64;
#ifndef JET_MAC
    long long created, modified, accessed;
#else
    long long created, modified, accessed, added;
#endif
    int type, flags;
    // if you make them const they cant be directly modified
    // if you make them bitfields no addresses can be taken
    const unsigned short links : 16, uid : 16, gid : 16;
    const struct {
        const unsigned char name : 1, type : 1, gid : 1, links : 1, perms : 1,
            size : 1, time : 1, uid : 1;
    } has;
    const struct {
        const unsigned char directory : 1, file : 1, link : 1, fifo : 1,
            socket : 1, readable : 1, writable : 1, executable : 1;
    } is; // get const char* groupString;
          // get const char* userString;
} File;
static const int s = sizeof(File);

int main() {
    File f = { .is.readable = 1, .has.name = 1, .name = "bang", .size = 34 };
    f.size = 33;
    unsigned long long* p = &f.size;
    *p = 33;
}