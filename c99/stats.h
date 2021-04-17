
static void expralloc_stat() {
    int iexpr, sum = 0, thres = 0;
    for (int i = 0; i < 128; i++) sum += exprsAllocHistogram[i];
    thres = sum / 20;

    eputs("ast_expr_t allocation by kind (those above 5% of total)\n"
          "  Kind        #      %%\n");
    for_to_where(i, 128, (iexpr = exprsAllocHistogram[i]) > thres) //
        eprintf("  %-8s %4d %7.2f\n", tokenkind_e_repr[i], iexpr,
            iexpr * 100.0 / sum);
    eputs("-------------------------------------------------------\n");
}

static void printstats(parser_t* const parser, double tms) {
    eputs("\n======================================================="
          "\n");
    eputs("PARSER STATISTICS\n");
    eputs("-------------------------------------------------------"
          "\n");

    eputs("Node allocations:\n");
    allocstat(ast_import_t);
    allocstat(ast_expr_t);
    allocstat(ast_var_t);
    allocstat(ast_type_t);
    allocstat(ast_scope_t);
    allocstat(ast_typespec_t);
    allocstat(ast_func_t);
    allocstat(ast_module_t);
    allocstat(PtrList);
    // allocstat(List_ast_expr);
    // allocstat(List_ast_var);
    // allocstat(List_ast_module);
    // allocstat(List_ast_func);
    // allocstat(List_ast_type);
    // allocstat(List_ast_import);
    // allocstat(List_ast_scope);
    allocstat(parser_t);
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
    eputs("Memory-related calls\n");
    eprintf("  calloc: %-7zu | malloc: %-7zu | realloc: %-7zu\n",
        _called_calloc, _called_malloc, _called_realloc);
    eprintf(
        "  strlen: %-7zu | strdup: %-7zu |\n", _called_strlen, _called_strdup);
    eputs("-------------------------------------------------------"
          "\n");
#endif
    expralloc_stat();

    eprintf("Time elapsed: %.1f ms (%.1f ms / 32kB)\n", tms,
        tms * 32768.0 / (parser->end - parser->data - 2)); // sw.print();
}