
#define cmd(s) else if (!strncasecmp(line, s "", sizeof(s "") - 1))

#define LINESIZE 1024

#include "../deps/linenoise.c"

int langserver(int argc, char* argv[]) {
  // struct sigaction sa;
  // sa.sa_flags = SA_SIGINFO;
  // sigemptyset(&sa.sa_mask);
  // sa.sa_sigaction = sighandler;
  // sigaction(SIGSEGV, &sa, NULL);

  CompilerMode mode = PMLint;
  List(Module)* modules = NULL;
  Parser* parser;
  Module* root;

  // static char linebuf[LINESIZE];
  char* line; //= linebuf;
  long len;
  while ((line = linenoise("jet> "))) {
    // printf("jet> ");
    // fgets(linebuf, LINESIZE, stdin);
    // line = linenoise("jet> ");//linebuf;
    while (*line == ' ') line++;
    len = strlen(line);
    if (line[len - 1] == '\n') line[len - 1] = 0;
    if (len > 2 && line[len - 2] == '\r') line[len - 2] = 0;

    if (0) { }
    cmd("exit") { return 0; }
    cmd("load") {
      clock_Time t0 = clock_getTime();
      char* filename = line + 5;
      while (*filename == ' ') filename++;

      parser = par_fromFile(filename, true, mode);
      if (!parser) continue;
      parser->issues.warnUnusedArg = //
          parser->issues.warnUnusedFunc = //
          parser->issues.warnUnusedType = //
          parser->issues.warnUnusedVar = 1;
      root = parseModule(parser, &modules, NULL);
      parser->elap = clock_clockSpanMicro(t0) / 1.0e3;
    }
    cmd("drop") { }
    cmd("mods") {
      foreach (Module*, mod, modules)
        printf("%s %s\n", mod->name, mod->filename);
    }
    cmd("stat") { printstats(parser); }
    cmd("syms") {
      foreach (Type*, type, root->types) {
        printf("%d:%d: %s\n", type->line, type->col, type->name);
        foreach (Var*, var, type->body->locals)
          printf("%d:%d: - %s\n", var->line, var->col, var->name);
      }
      foreach (Type*, enm, root->enums)
        printf("%d:%d: %s\n", enm->line, enm->col, enm->name);
      foreach (Func*, func, root->funcs)
        if (!func->intrinsic)
          printf("%d:%d: %s\n", func->line, func->col, func->psel);
      foreach (Test*, test, root->tests)
        printf("%d:%d: %s\n", test->line, 0, test->name);
    }
    cmd("def") { }
    cmd("expr") {
      Expr* ret;
      Func* fn;
      Scope* sco;
      Type* ty;
      char* pos = NULL;
      int col, lno = strtod(line + 5, &pos);
      if (pos) col = strtod(pos, NULL);
      ret = mod_getExpr(root, lno, col, &fn, &sco, &ty);
      if (ret) {
        expr_write(ret, 0, yes, no);
        puts("");
        if (fn)
          printf("from func %s at %d:%d\n", fn->psel, fn->line, fn->col);
        if (ty)
          printf("from type %s at %d:%d\n", ty->name, ty->line, ty->col);
      } else {
        eprintf("nothing found at %d:%d\n", lno, col);
      }
    }
    cmd("write") { mod_write(root); }
    cmd("dump") { mod_dumpc(root); }
    cmd("emit") { mod_emit(root); }
    else {
      fputs("Commands:\n"
            "    mods\n"
            "    load <file>\n"
            "    drop <file>\n"
            "    stat <file>\n"
            "    syms <file>\n"
            "    def <line> <col> <file>\n"
            "    expr <line> <col> <file> \n"
            "    delete <line> \n"
            "    replace <line> <newexpr> \n"
            "    insert <line> <newexpr> \n"
            "    write <file>\n"
            "    dump <file>\n"
            "    emit <file>\n"
            "    exit\n",
          stderr);
    }
  }

  return 0;
}
