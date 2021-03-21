
static void expralloc_stat() {
    int iexpr, sum = 0, thres = 0;
    for (int i = 0; i < 128; i++) sum += exprsAllocHistogram[i];
    thres = sum / 20;

    eputs("\e[1mASTExpr allocation by kind \e[0m(those above 5% of "
          "total)\n  Kind    "
          "    # "
          "     %%\n");
    for (int i = 0; i < 128; i++) {
        if ((iexpr = exprsAllocHistogram[i]) > thres)
            eprintf("  %-8s %4d %7.2f\n", TokenKind_repr((TokenKind)i, false),
                iexpr, iexpr * 100.0 / sum);
    }
    eputs("-------------------------------------------------------\n");
}

static void printstats(Parser* const parser, double tms) {
    eputs("\n======================================================="
          "\n");
    eputs("\e[1mPARSER STATISTICS\e[0m\n");
    eputs("-------------------------------------------------------"
          "\n");

    eputs("\e[1mNode allocations:\e[0m\n");
    allocstat(ASTImport);
    allocstat(ASTExpr);
    allocstat(ASTVar);
    allocstat(ASTType);
    allocstat(ASTScope);
    allocstat(ASTTypeSpec);
    allocstat(ASTFunc);
    allocstat(ASTModule);
    allocstat(PtrList);
    allocstat(List_ASTExpr);
    allocstat(List_ASTVar);
    allocstat(List_ASTModule);
    allocstat(List_ASTFunc);
    allocstat(List_ASTType);
    allocstat(List_ASTImport);
    allocstat(List_ASTScope);
    allocstat(Parser);
    eputs("-------------------------------------------------------"
          "\n");
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
#ifdef DEBUG
    eputs("\e[1mMemory-related calls\e[0m\n");
    eprintf("  calloc: %-7zu | malloc: %-7zu | realloc: %-7zu\n",
        _called_calloc, _called_malloc, _called_realloc);
    eprintf(
        "  strlen: %-7zu | strdup: %-7zu |\n", _called_strlen, _called_strdup);
    eputs("-------------------------------------------------------"
          "\n");
#endif
    expralloc_stat();

    eprintf("\e[1mTime elapsed:\e[0m %.1f ms (%.1f ms / 32kB)\n", tms,
        tms * 32768.0 / (parser->end - parser->data - 2));
}