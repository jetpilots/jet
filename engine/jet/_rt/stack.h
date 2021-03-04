
// *** These should be thread local
static size_t _scSize_; // size of stack
static size_t _scDepth_ = 0; // current depth, updated in each function
static size_t _scUsage_ = 0; // current depth, updated in each function
static size_t _scPrintAbove_ = 0; // used for truncating long backtraces
// static const char* _scStart_; // start of stack, set in main()

#ifndef NOSTACKCHECK
// define these unconditionally, they are needed for both debug and release
// for fast mode, NOSTACKCHECK is defined globally, so these will not be
// used.
#define STACKDEPTH_UP                                                          \
    {                                                                          \
        _scDepth_++;                                                           \
        _scUsage_ += MYSTACKUSAGE;                                             \
    }
#define STACKDEPTH_DOWN                                                        \
    {                                                                          \
        _scDepth_--;                                                           \
        _scUsage_ -= MYSTACKUSAGE;                                             \
    }
#else
#define STACKDEPTH_UP
#define STACKDEPTH_DOWN
#endif
// what is the point of a separate NOSTACKCHECK option if release mode
// doesn't track ANY info (stack depth, function name etc.) other than
// showing "stack overflow" instead of "segmentation fault".
// --- it may not show segmentation fault.

#ifdef DEBUG
#define FUNC_ENTRY_STACK_BLOWN                                                 \
    _scPrintAbove_ = _scDepth_ - _btLimit_;                                    \
    printf("\e[31mfatal: stack overflow at call depth %lu.\n    in %s\e[0m\n", \
        _scDepth_, sig_);                                                      \
    printf("\e[90mBacktrace (innermost first):\n");                            \
    if (_scDepth_ > 2 * _btLimit_)                                             \
        printf("    limited to %d outer and %d inner entries.\n", _btLimit_,   \
            _btLimit_);                                                        \
    printf("[%lu] \e[36m%s\n", _scDepth_, callsite_);
#else
#define FUNC_ENTRY_STACK_BLOWN                                                 \
    printf("\e[31mfatal: stack  overflow at call depth %lu.\e[0m\n", _scDepth_);
#endif

#ifndef NOSTACKCHECK
#define DO_STACK_CHECK                                                         \
    if (_scUsage_ >= _scSize_) {                                               \
        FUNC_ENTRY_STACK_BLOWN;                                                \
        DONE;                                                                  \
    }
#else
#define DO_STACK_CHECK
#endif

#ifdef DEBUG
#define SHOW_BACKTRACE_LINE                                                    \
    if (_scDepth_ <= _btLimit_ || _scDepth_ > _scPrintAbove_)                  \
        printf("\e[90m[%lu] \e[36m%s\n", _scDepth_, callsite_);                \
    else if (_scDepth_ == _scPrintAbove_)                                      \
        printf("\e[90m... truncated ...\e[0m\n");
#else
#define SHOW_BACKTRACE_LINE
#endif
