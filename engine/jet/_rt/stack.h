
// *** These should be thread local
// thread_local size_t _scSize_; // size of stack
thread_local size_t _scDepth_ = 0;
// current depth, updated in each function
// thread_local size_t _scUsage_ = 0;
// current depth, updated in each function
thread_local size_t _scPrintAbove_ = 0;
// used for truncating long backtraces
// static const char* _scStart_; // start of stack, set in main()
thread_local _stack_boundary;

#define _STACK_BLOWN() (&(char) { 0 } < _stack_boundary)
#define _STACK_GROWS_UP() _stack_grows_up__(&(char) { 0 })
__attribute((__noinline__)) bool _stack_grows_up__(char* parentsLocal) {
  return (uintptr_t)parentsLocal < (uintptr_t)&parentsLocal;
}

#ifndef NOSTACKCHECK
// define these unconditionally, they are needed for both debug and release
// for fast mode, NOSTACKCHECK is defined globally, so these will not be
// used.
#define STACKDEPTH_UP _scDepth_++
//   {
//     _scUsage_ += MYSTACKUSAGE;
//   }
#define STACKDEPTH_DOWN _scDepth_--
//   {
//     _scUsage_ -= MYSTACKUSAGE;
//   }
#else
#define STACKDEPTH_UP
#define STACKDEPTH_DOWN
#endif
// what is the point of a separate NOSTACKCHECK option if release mode
// doesn't track ANY info (stack depth, function name etc.) other than
// showing "stack overflow" instead of "segmentation fault".
// --- it may not show segmentation fault.

#ifdef DEBUG
#define FUNC_ENTRY_STACK_BLOWN                                             \
  _scPrintAbove_ = _scDepth_ - _btLimit_;                                  \
  printf(                                                                  \
      "\e[31mfatal: stack overflow at call depth %lu.\n    in %s\e[0m\n",  \
      _scDepth_, sig_);                                                    \
  printf("\e[90mBacktrace (innermost first):\n");                          \
  if (_scDepth_ > 2 * _btLimit_)                                           \
    printf("    limited to %d outer and %d inner entries.\n", _btLimit_,   \
        _btLimit_);                                                        \
  printf("[%lu] \e[36m%s\n", _scDepth_, callsite_);
#else
#define FUNC_ENTRY_STACK_BLOWN                                             \
  printf(                                                                  \
      "\e[31mfatal: stack overflow at call depth %lu.\e[0m\n", _scDepth_);
#endif

#ifndef NOSTACKCHECK
#define DO_STACK_CHECK                                                     \
  /*if (_scUsage_ >= _scSize_) */                                          \
  if (_STACK_BLOWN()) {                                                    \
    FUNC_ENTRY_STACK_BLOWN;                                                \
    DONE;                                                                  \
  }
#else
#define DO_STACK_CHECK
#endif

// #define C_RED "\e[31m"
// #define C_GRN "\e[32m"
// #define C_BLU "\e[33m"
// #define C_CYN "\e[34m"
// #define C_MGN "\e[35m"
// #define C_YLW "\e[36m"
// #define C_GRE "\e[37m"
// #define C_BRED "\e[41m"
// #define C_BGRN "\e[42m"
// #define C_BBLU "\e[43m"
// #define C_BCYN "\e[44m"
// #define C_BMGN "\e[45m"
// #define C_BYLW "\e[46m"
// #define C_BGRE "\e[47m"
// #define C_RSET "\e[0m"

#ifdef DEBUG
#define SHOW_BACKTRACE_LINE                                                \
  if (_scDepth_ <= _btLimit_ || _scDepth_ > _scPrintAbove_)                \
    printf("\e[90m[%lu] \e[36m%s\n", _scDepth_, callsite_);                \
  else if (_scDepth_ == _scPrintAbove_)                                    \
    printf("\e[90m... truncated ...\e[0m\n");
#else
#define SHOW_BACKTRACE_LINE
#endif
