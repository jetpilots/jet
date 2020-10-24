
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
    allocstat(jet_PtrList);
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
        jet_gPool->usedTotal);
    eprintf("*** Space allocated for nodes               = %7d B\n",
        jet_gPool->capTotal);
    eprintf("*** Node space utilisation                  = %7.1f %%\n",
        jet_gPool->usedTotal * 100.0 / jet_gPool->capTotal);
    eputs("-------------------------------------------------------"
          "\n");
    eprintf("*** File size                               = %7lu B\n",
        parser->end - parser->data - 2);
    eprintf("*** Node size to file size ratio            = %7.1f x\n",
        jet_gPool->usedTotal * 1.0 / (parser->end - parser->data - 2));
    eputs("-------------------------------------------------------"
          "\n");
    eprintf("*** Space used for strings                  = %7u B\n",
        jet_sPool->usedTotal);
    eprintf("*** Allocated for strings                   = %7u B\n",
        jet_sPool->capTotal);
    eprintf("*** Space utilisation                       = %7.1f %%\n",
        jet_sPool->usedTotal * 100.0 / jet_sPool->capTotal);
    eputs("-------------------------------------------------------"
          "\n");
    eputs("\e[1mMemory-related calls\e[0m\n");
    eprintf("  calloc: %-7d | malloc: %-7d | realloc: %-7d\n",
        jet_globals__callocCount, jet_globals__mallocCount,
        jet_globals__reallocCount);
    eprintf("  strlen: %-7d | strdup: %-7d |\n", jet_globals__strlenCount,
        jet_globals__strdupCount);
    eputs("-------------------------------------------------------"
          "\n");

    expralloc_stat();

    eprintf("\e[1mTime elapsed:\e[0m %.1f ms (%.1f ms / 32kB)\n", tms,
        tms * 32768.0 / (parser->end - parser->data - 2)); // sw.print();
}