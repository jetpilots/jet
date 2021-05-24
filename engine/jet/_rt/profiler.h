#define JET_PROFILE_LINE(l)                                                \
  {                                                                        \
    _lprof_tmp_ = Ticks_get();                                             \
    _lprof_[l - 1] += (_lprof_tmp_ - _lprof_last_) / 100;                  \
    _lprof_last_ = _lprof_tmp_;                                            \
  }
extern Ticks _lprof_[], _lprof_last_;

static void lineprofile_report() {
  FILE* fd = fopen("." THISFILE "r", "w");
  Ticks sum = 0;
  for (int i = 0; i < NUMLINES; i++) sum += _lprof_[i];
  for (int i = 0; i < NUMLINES; i++) {
    double pct = _lprof_[i] * 100.0 / sum;
    if (pct > 1.0)
      fprintf(fd, " %8.1f%% |\n", pct);
    else if (pct == 0.0)
      fprintf(fd, "           |\n");
    else
      fprintf(fd, "         ~ |\n");
  }
  fclose(fd);
  system("paste -d ' ' ." THISFILE "r " THISFILE " > " THISFILE "r");
}

static void lineprofile_begin() { _lprof_last_ = Ticks_get(); }

// this stuff needs to be per-module