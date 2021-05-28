
#define CHECK_HELP_OPEN printf("Here's some help:\n");
#define CHECK_HELP_CLOSE                                                   \
  printf("\n");                                                            \
  printf("Backtrace (innermost first):\n");                          \
  if (_scDepth_ > 2 * _btLimit_)                                           \
    printf("    limited to %d outer and %d inner entries.\n", _btLimit_,   \
        _btLimit_);                                                        \
  BACKTRACE;

#define CHECK_HELP_DISABLED                                                \
  eputs("(run in debug mode to get more info)\n");                         \
  exit(1);