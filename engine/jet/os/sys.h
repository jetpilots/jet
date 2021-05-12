#ifdef WINDOWS
#include <windows.h>
#endif

size_t sys_pageSize() {
#ifdef WINDOWS
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
#else
  return sysconf(_SC_PAGESIZE);
#endif
}

#include <sys/resource.h>
static size_t sys_stackSize() {
  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  return limit.rlim_cur; //, limit.rlim_max);
}
