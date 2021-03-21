

void List_dumpcstatic(PtrList* list) {

    dumpcstatic(&list->item);
    dumpcstatic((void**)&list->next);
}

void ASTTypeSpec_dumpcstatic(ASTTypeSpec* typeSpec);

void ASTExpr_dumpcstatic(ASTExpr* expr) { }
void ASTUnits_dumpcstatic(ASTUnits* units) { }

void ASTVar_dumpcstatic(ASTVar* var) {
    dumpcstatic_s(&var->name);
    ASTTypeSpec_dumpcstatic(var->typeSpec);
    dumpcstatic((void**)&var->typeSpec);
    ASTExpr_dumpcstatic(var->init);
    dumpcstatic((void**)&var->init);
}

void ASTScope_dumpcstatic(ASTScope* scope) {
    foreach (ASTExpr*, stmt, scope->stmts)
        ASTExpr_dumpcstatic(stmt);
    foreach (ASTVar*, var, scope->locals)
        ASTVar_dumpcstatic(var);
    List_dumpcstatic(scope->stmts);
    List_dumpcstatic(scope->locals);

    dumpcstatic((void**)&scope->stmts);
    dumpcstatic((void**)&scope->locals);
    dumpcstatic((void**)&scope->parent);
}

void ASTType_dumpcstatic(ASTType* type) {
    ASTScope_dumpcstatic(type->body);
    dumpcstatic((void**)&type->body);
    dumpcstatic_s(&type->name);
}

void ASTImport_dumpcstatic(ASTImport* import) { dumpcstatic_s(&import->name); }

void ASTTypeSpec_dumpcstatic(ASTTypeSpec* typeSpec) {
    if (typeSpec->typeType == TYObject) {
        ASTType_dumpcstatic(typeSpec->type);
        dumpcstatic((void**)&typeSpec->type);
    } else if (TypeType_isnum(typeSpec->typeType) && typeSpec->units) {
        ASTUnits_dumpcstatic(typeSpec->units);
        dumpcstatic((void**)&typeSpec->units);
    } else {

        unreachable("%s", "-");
    }
}

void ASTEnum_dumpcstatic(ASTType* enm) {
    ASTScope_dumpcstatic(enm->body);
    dumpcstatic((void**)&enm->body);
    dumpcstatic_s(&enm->name);
}

void ASTFunc_dumpcstatic(ASTFunc* func) {

    printf(".name = \"%s\",", func->name);
    printf(".selector = \"%s\",", func->selector);
    printf(".isDeclare = %d,", func->isDeclare);
    printf(".args = ");
    List_dumpc(ASTVar, func->args);
    printf(".returnSpec = ", func->name);
    printf(".args = \"%s\",", func->name);
}

void ASTTest_dumpcstatic(ASTTest* test) {
    ASTScope_dumpcstatic(test->body);
    dumpcstatic((void**)&test->body);
    dumpcstatic_s(&test->name);
    dumpcstatic_s(&test->selector);
}

void ASTModule_dumpcstatic(ASTModule* mod) {
    foreach (ASTVar*, var, mod->scope.locals)
        ASTVar_dumpcstatic(var);
    foreach (ASTFunc*, func, mod->funcs)
        ASTFunc_dumpcstatic(func);
    foreach (ASTTest*, test, mod->tests)
        ASTTest_dumpcstatic(test);
    foreach (ASTExpr*, expr, mod->scope.stmts)
        ASTExpr_dumpcstatic(expr);
    foreach (ASTImport*, import, mod->imports)
        ASTImport_dumpcstatic(import);
    foreach (ASTType*, enm, mod->enums)
        ASTEnum_dumpcstatic(enm);
    foreach (ASTType*, type, mod->types)
        ASTType_dumpcstatic(type);

    List_dumpcstatic(mod->scope.locals);
    List_dumpcstatic(mod->funcs);
    List_dumpcstatic(mod->tests);
    List_dumpcstatic(mod->scope.stmts);
    List_dumpcstatic(mod->imports);
    List_dumpcstatic(mod->enums);
    List_dumpcstatic(mod->types);

    dumpcstatic((void**)&mod->scope.locals);
    dumpcstatic((void**)&mod->funcs);
    dumpcstatic((void**)&mod->tests);
    dumpcstatic((void**)&mod->scope.stmts);
    dumpcstatic((void**)&mod->imports);
    dumpcstatic((void**)&mod->enums);
    dumpcstatic((void**)&mod->types);
    dumpcstatic_s(&mod->name);
    dumpcstatic_s(&mod->moduleName);
}
