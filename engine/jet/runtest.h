static int _total, _pass, _fail, _skip, _stop, _crash;
static const char *s_err = "✘", *s_ok = "✔︎", *s_skp = "⁃",
                  *s_crash = "✽";

int jet_quicktest = 0;

void jet_runTest(int (*f)(void), char* s, int skip) {
  // Dict_putk(UInt32, VPtr)(&runDict, pid, s);
  _total++;
  _err_ = NULL; // reset any errs from prev tests
  if (skip) {
    _skip++;
    eprintf(" %s  %-48s\n", s_skp, s);
    return;
  }
  if (jet_quicktest) {
    clock_Time t0 = clock_getTime();
    int ret = f();
    double elap = clock_clockSpanMicro(t0) / 1e3;
    char* who = s; // Dict_getk(UInt32, VPtr)(&runDict, w);
    if (ret) {
      eprintf(" %s  %-48s", s_err, who);
      _fail++;
    } else {
      printf(" %s  %-48s", s_ok, who);
      _pass++;
    }
    char* units = "ms";
    fprintf(ret ? stderr : stdout, " [%7.1f %s]\n", elap, units);
  } else {
    pid_t pid = fork();
    if (pid) { // parent
      clock_Time t0 = clock_getTime();
      int t, ret;
      pid_t w = waitpid(pid, &t, 0);
      double elap = clock_clockSpanMicro(t0) / 1e3;
      // ^ FIME inaccurate timing. child should measure n communicate. BUT
      // what if child crashes
      char* who = s; // Dict_getk(UInt32, VPtr)(&runDict, w);
      // if (!who) who = "(unknown)";
      if (WIFSIGNALED(t)) {
        eprintf(
          " %s  %-48s\n    -> %s", s_err, who, strsignal(WTERMSIG(t)));
        // psignal(WTERMSIG(t), "killed: ");
        ret = WTERMSIG(t);
        _crash++; //, _fail++;
      } else if (WIFSTOPPED(t)) {
        eprintf(
          " %s  %-48s\n    -> %s", s_err, who, strsignal(WSTOPSIG(t)));
        // psignal(WSTOPSIG(t), "stopped: ");
        ret = WSTOPSIG(t);
        _stop++;
        // _fail++;
      } else if (WEXITSTATUS(t)) {
        eprintf(" %s  %-48s", s_err, who);
        ret = WEXITSTATUS(t);
        _fail++;
      } else {
        printf(" %s  %-48s", s_ok, who);
        ret = 0;
        _pass++;
      }
      char* units = "ms";
      // if (elap > 1000.0) {
      //   elap /= 1000;
      //   units = "s";
      // }
      // if (elap > 60.0) {
      //   elap /= 60.0;
      //   units = "min";
      // }
      // if (elap > 60.0) {
      //   elap /= 60.0;
      //   units = "hr";
      // }
      fprintf(ret ? stderr : stdout, " [%7.1f %s]\n", elap, units);
      // return ret;
      // PIDs may be reused if spawn too many child processes
      // Dict_delk(UInt32, VPtr)(&runDict, pid);
    } else { // child
      exit(f());
    }
  }
}
