#define _btLimit_ 10
static const char* const ERROR_TRACE = (const char*)0xFFFFFFFFFFFFFFFF;
#define DONE                                                               \
  if ((_err_ = ERROR_TRACE)) goto return_;
#define BACKTRACE                                                          \
  if ((_err_ = ERROR_TRACE)) goto backtrace;
#define TRACE_IF_ERROR                                                     \
  if (_err_ == ERROR_TRACE) goto backtrace;