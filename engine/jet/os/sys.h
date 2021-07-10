#ifdef WINDOWS
#include <windows.h>
#endif

monostatic size_t sys_pageSize(void) {
#ifdef WINDOWS
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
#else
  return sysconf(_SC_PAGESIZE);
#endif
}

#include <sys/resource.h>
monostatic size_t sys_stackSize(void) {
  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  return limit.rlim_cur; //, limit.rlim_max);
}
