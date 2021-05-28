static int _total, _pass, _fail, _skip, _stop, _crash;
static const char *s_err = "[ERR]", *s_ok = "[OK!]", *s_skp = "[SKP]";

void jet_runTest(int (*f)(void), char* s, int skip) {
  // Dict_putk(UInt32, Ptr)(&runDict, pid, s);
  _total++;
  if (skip) {
    _skip++;
    eprintf("%s %s skipped\n", s_skp, s);
    return;
  }
  pid_t pid = fork();
  if (pid) { // parent
    clock_Time t0 = clock_getTime();
    int t, ret;
    pid_t w = waitpid(pid, &t, 0);
    double elap = clock_clockSpanMicro(t0) / 1e3;
    // ^ FIME inaccurate timing. child should measure n communicate. BUT
    // what if child crashes
    char* who = s; // Dict_getk(UInt32, Ptr)(&runDict, w);
    // if (!who) who = "(unknown)";
    if (WIFSIGNALED(t)) {
      eprintf(
          "-> %s \"%s\" crashed: %s", s_err, who, strsignal(WTERMSIG(t)));
      // psignal(WTERMSIG(t), "killed: ");
      ret = WTERMSIG(t);
      _crash++, _fail++;
    } else if (WIFSTOPPED(t)) {
      eprintf(
          "-> %s \"%s\" stopped: %s", s_err, who, strsignal(WSTOPSIG(t)));
      // psignal(WSTOPSIG(t), "stopped: ");
      ret = WSTOPSIG(t);
      _stop++;
      _fail++;
    } else if (WEXITSTATUS(t)) {
      eprintf("-> %s \"%s\" failed", s_err, who);
      ret = WEXITSTATUS(t);
      _fail++;
    } else {
      eprintf("-> %s \"%s\"", s_ok, who);
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
    eprintf(" (%g %s)\n", elap, units);
    // return ret;
    // PIDs may be reused if spawn too many child processes
    // Dict_delk(UInt32, Ptr)(&runDict, pid);
  } else { // child
    exit(f());
  }
}
