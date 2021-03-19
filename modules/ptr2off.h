// This could be generated easily. Every struct has associated ptr2off and
// off2ptr funcs, and you can dump the pool contents out to disk by converting
// all ptrs to offsets and reconverting them on read.

// You may want to save main()'s stack vars as well (everything else is temp)
// as well as stuff that goes into static storage / .data / .bss

// keep a global var ptr2off_dirn that says whether the implementation of
// ptr2off should be forward (to offset) or backward (from offset). This way
// you need only 1 ptr2off func for each type. Best is keep a func ptr that
// takes you to the actual implementation.

void ptr2off_s_fwd(char** str) { }

void ptr2off_fwd(void** ptr) {
    size_t pd = 0;
    pd = *ptr - gPool->ref;
    if (pd > 0 && pd < gPool->cap) {
        pd |= 0xffffUL << 48;
        *ptr = (void*)pd;
        return;
    }

    for_to(i, gPool->ptrs.used) {
        pd = *ptr - gPool->ptrs.ref[i];
        if (pd > 0 && pd < gPool->caps.ref[i]) {
            pd |= ((size_t)i) << 48;
            // high16 holds subpool index, low48 is the offset
            *ptr = (void*)pd;
            return;
        }
    }

    unreachable("ptr2off failed %s", "-");
}

void ptr2off_rev(void** ptr) {
    size_t pd = (size_t)*ptr;
    unsigned short owner = pd >> 48;
    pd |= 0x0000ffffffffffff;
    if (owner == 0xffff)
        *ptr = pd + gPool->ref;
    else if (owner < gPool->ptrs.used)
        *ptr = pd + gPool->ptrs.ref[owner];
    else
        unreachable("ptr2off failed %s", "-");
}

static void (*ptr2off)(void**) = ptr2off_fwd;
static void (*ptr2off_s)(char**) = ptr2off_s_fwd;

// BEGIN STRUCT HELPERS

void List_ptr2off(PtrList* list) {
    // CALL THIS AFTER ITERATING THROUGH THE LIST!!
    ptr2off(&list->item);
    ptr2off((void**)&list->next);
}

void ASTTypeSpec_ptr2off(ASTTypeSpec* typeSpec);

void ASTExpr_ptr2off(ASTExpr* expr) { }
void ASTUnits_ptr2off(ASTUnits* units) { }

void ASTVar_ptr2off(ASTVar* var) {
    ptr2off_s(&var->name);
    ASTTypeSpec_ptr2off(var->typeSpec);
    ptr2off((void**)&var->typeSpec);
    ASTExpr_ptr2off(var->init);
    ptr2off((void**)&var->init);
}

void ASTScope_ptr2off(ASTScope* scope) {
    foreach (ASTExpr*, stmt, scope->stmts)
        ASTExpr_ptr2off(stmt);
    foreach (ASTVar*, var, scope->locals)
        ASTVar_ptr2off(var);
    List_ptr2off(scope->stmts);
    List_ptr2off(scope->locals);
    // DO NOT process the parent!
    ptr2off((void**)&scope->stmts);
    ptr2off((void**)&scope->locals);
    ptr2off((void**)&scope->parent);
}

void ASTType_ptr2off(ASTType* type) {
    ASTScope_ptr2off(type->body);
    ptr2off((void**)&type->body);
    ptr2off_s(&type->name);
    // TODO: type super?
}

void ASTImport_ptr2off(ASTImport* import) {
    // ASTScope_ptr2off(type->body);
    // ptr2off(&type->body);
    ptr2off_s(&import->importFile);
    // TODO: type super?
}

void ASTTypeSpec_ptr2off(ASTTypeSpec* typeSpec) {
    if (typeSpec->typeType == TYObject) {
        ASTType_ptr2off(typeSpec->type);
        ptr2off((void**)&typeSpec->type);
    } else if (TypeType_isnum(typeSpec->typeType) && typeSpec->units) {
        ASTUnits_ptr2off(typeSpec->units);
        ptr2off((void**)&typeSpec->units);
    } else {
        // what's left. unresolved?
        unreachable("%s", "-");
    }
}

void ASTEnum_ptr2off(ASTType* enm) {
    ASTScope_ptr2off(enm->body);
    ptr2off((void**)&enm->body);
    ptr2off_s(&enm->name);
    // TODO: type super?
}

void ASTFunc_ptr2off(ASTFunc* func) {
    foreach (ASTVar*, arg, func->args)
        ASTVar_ptr2off(arg);
    List_ptr2off(func->args);
    ASTScope_ptr2off(func->body);
    ASTTypeSpec_ptr2off(func->returnSpec);

    ptr2off((void**)&func->body);
    ptr2off((void**)&func->returnSpec);
    ptr2off((void**)&func->args);

    ptr2off_s(&func->name);
    ptr2off_s(&func->selector);
}

void ASTTest_ptr2off(ASTTest* test) {
    ASTScope_ptr2off(test->body);
    ptr2off((void**)&test->body);
    ptr2off_s(&test->name);
    ptr2off_s(&test->selector);
}

void ASTModule_ptr2off(ASTModule* mod) {
    foreach (ASTVar*, var, mod->scope->locals)
        ASTVar_ptr2off(var);
    foreach (ASTFunc*, func, mod->funcs)
        ASTFunc_ptr2off(func);
    foreach (ASTTest*, test, mod->tests)
        ASTTest_ptr2off(test);
    foreach (ASTExpr*, expr, mod->scope->stmts)
        ASTExpr_ptr2off(expr);
    foreach (ASTImport*, import, mod->imports)
        ASTImport_ptr2off(import);
    foreach (ASTType*, enm, mod->enums)
        ASTEnum_ptr2off(enm);
    foreach (ASTType*, type, mod->types)
        ASTType_ptr2off(type);

    List_ptr2off(mod->scope->locals);
    List_ptr2off(mod->funcs);
    List_ptr2off(mod->tests);
    List_ptr2off(mod->scope->stmts);
    List_ptr2off(mod->imports);
    List_ptr2off(mod->enums);
    List_ptr2off(mod->types);

    ptr2off((void**)&mod->scope->locals);
    ptr2off((void**)&mod->funcs);
    ptr2off((void**)&mod->tests);
    ptr2off((void**)&mod->scope->stmts);
    ptr2off((void**)&mod->imports);
    ptr2off((void**)&mod->enums);
    ptr2off((void**)&mod->types);
    ptr2off_s(&mod->name);
    ptr2off_s(&mod->moduleName);
}
