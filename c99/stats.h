
static void expralloc_stat() {
  int iexpr, sum = 0, thres = 0;
  for_to(i, 128) sum += exprsAllocHistogram[i];
  thres = sum / 20;

  eputs("Expr allocation by kind (those above 5% of total)\n"
        "  Kind        #      %%\n");
  for_to_where(i, 128, (iexpr = exprsAllocHistogram[i]) > thres) //
    eprintf(
      "  %-8s %4d %7.2f\n", TokenKind_repr[i], iexpr, iexpr * 100.0 / sum);
  eputs("-------------------------------------------------------\n");
}

static void printstats(Parser* const parser) {
  eputs("\n=======================================================\n");
  eputs(parser->filename);
  eputs(": statistics\n");
  eputs("-------------------------------------------------------\n");

  eputs("Node allocations:\n");
  allocstat(Import);
  allocstat(Expr);
  allocstat(Var);
  allocstat(Type);
  allocstat(Scope);
  allocstat(TypeSpec);
  allocstat(Test);
  allocstat(Func);
  allocstat(Module);
  allocstat(PtrList);
  // allocstat(List_Expr);
  // allocstat(List_Var);
  // allocstat(List_Module);
  // allocstat(List_Func);
  // allocstat(List_Type);
  // allocstat(List_Import);
  // allocstat(List_Scope);
  allocstat(Parser);
  eputs("-------------------------------------------------------\n");
  eprintf("*** Total size of nodes                     = %7d B\n",
    gPool->usedTotal);
  eprintf("*** Space allocated for nodes               = %7d B\n",
    gPool->capTotal);
  eprintf("*** Node space utilisation                  = %7.1f %%\n",
    gPool->usedTotal * 100.0 / gPool->capTotal);
  eputs("-------------------------------------------------------"
        "\n");
  eprintf("*** File size                               = %7lu B\n",
    parser->end - parser->data - 2);
  eprintf("*** Node size to file size ratio            = %7.1f x\n",
    gPool->usedTotal * 1.0 / (parser->end - parser->data - 2));
  eputs("-------------------------------------------------------"
        "\n");
  eprintf("*** Space used for strings                  = %7u B\n",
    sPool->usedTotal);
  eprintf("*** Allocated for strings                   = %7u B\n",
    sPool->capTotal);
  eprintf("*** Space utilisation                       = %7.1f %%\n",
    sPool->usedTotal * 100.0 / sPool->capTotal);
  eputs("-------------------------------------------------------"
        "\n");
#ifndef NDEBUG
  eputs("Memory-related calls\n");
  eprintf("  calloc: %-7zu | malloc: %-7zu | realloc: %-7zu\n",
    _called_calloc, _called_malloc, _called_realloc);
  eprintf(
    "  strlen: %-7zu | strdup: %-7zu |\n", _called_strlen, _called_strdup);
  eputs("-------------------------------------------------------"
        "\n");
#endif
  expralloc_stat();

  eprintf("parse+check time: %.2f ms (%.0f ms / 32kB)\n", parser->elap,
    parser->elap * 32768.0
      / (parser->end - parser->data - 2)); // sw.print();
  eprintf("output time: %.1f ms\n", parser->oelap - parser->elap);
  eprintf("cc time: %.0f ms\n", parser->elap_tot - parser->oelap);
}