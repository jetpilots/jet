#define _btLimit_ 10
#define ERROR_TRACE (char*)0xFFFFFFFFFFFFFFFF
#define DONE                                                                   \
    {                                                                          \
        _err_ = ERROR_TRACE;                                                   \
        goto return_;                                                          \
    }
#define BACKTRACE                                                              \
    {                                                                          \
        _err_ = ERROR_TRACE;                                                   \
        goto backtrace;                                                        \
    }

#define TRACE_IF_ERROR                                                         \
    if (_err_ == ERROR_TRACE) goto backtrace;