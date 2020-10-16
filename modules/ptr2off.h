// This could be generated easily. Every struct has associated ptr2off and
// off2ptr funcs, and you can dump the pool contents out to disk by converting
// all ptrs to offsets and reconverting them on read.

// You may want to save main()'s stack vars as well (everything else is temp)
// as well as stuff that goes into static storage / .data / .bss

// keep a global var ptr2off_dirn that says whether the implementation of
// ptr2off should be forward (to offset) or backward (from offset). This way
// you need only 1 ptr2off func for each type. Best is keep a func ptr that
// takes you to the actual implementation.

void ASTTypeSpec_ptr2off(ASTTypeSpec* typeSpec)
{
    if (typeSpec->typeType == TYObject) {
        ASTType_ptr2off(typeSpec->type);
        ptr2off(&typeSpec->type);
    } else if (TypeType_isNumeric(typeSpec->typeType) and typeSpec->units) {
        ASTUnits_ptr2off(typeSpec->units);
        ptr2off(&typeSpec->units);
    } else {
        // what's left. unresolved?
        unreachable("");
    }
}

void ASTVar_ptr2off(ASTVar* var)
{
    ptr2off_s(var->name);
    ASTTypeSpec_ptr2off(var->typeSpec);
    ptr2off(&var->typeSpec);
    ASTExpr_ptr2off(var->init);
    ptr2off(&var->init);
}

void ASTScope_ptr2off(ASTScope* scope)
{
    foreach (ASTExpr, stmt, scope->stmts)
        ASTExpr_ptr2off(stmt);
    foreach (ASTVar, var, stmt->locals)
        ASTVar_ptr2off(var);
    List_ptr2off(scope->stmts);
    List_ptr2off(scope->locals);
    // DO NOT process the parent!
    ptr2off(&scope->stmts);
    ptr2off(&scope->locals);
    ptr2off(&scope->parent);
}

void ASTType_ptr2off(ASTType* type)
{
    ASTScope_ptr2off(type->body);
    ptr2off(&type->body);
    ptr2off_s(&type->name);
    // TODO: type super?
}

void ASTEnum_ptr2off(ASTEnum* enm)
{
    ASTScope_ptr2off(enm->body);
    ptr2off(&enm->body);
    ptr2off_s(&enm->name);
    // TODO: type super?
}

void ASTFunc_ptr2off(ASTFunc* func)
{
    foreach (ASTVar, arg, func->args)
        ASTVar_ptr2off(arg);
    List_ptr2off(func->args);
    ASTScope_ptr2off(func->body);
    ASTTypeSpec_ptr2off(func->returnSpec);

    ptr2off(&func->body);
    ptr2off(&func->returnSpec);
    ptr2off(&func->args);

    ptr2off_s(&func->name);
    ptr2off_s(&func->selector);
}

void ASTTest_ptr2off(ASTTest* test)
{
    ASTScope_ptr2off(test->body);
    ptr2off(&test->body);
    ptr2off_s(&test->name);
    ptr2off_s(&test->selector);
}

void ASTModule_ptr2off(ASTModule* mod)
{
    foreach (ASTVar, var, mod->globals)
        ASTVar_ptr2off(var);
    foreach (ASTFunc, func, mod->funcs)
        ASTFunc_ptr2off(func);
    foreach (ASTTest, test, mod->tests)
        ASTTest_ptr2off(test);
    foreach (ASTExpr, expr, mod->exprs)
        ASTExpr_ptr2off(expr);
    foreach (ASTImport, import, mod->imports)
        ASTImport_ptr2off(import);
    foreach (ASTEnum, enm, mod->enums)
        ASTEnum_ptr2off(enm);
    foreach (ASTType, type, mod->types)
        ASTType_ptr2off(type);

    List_ptr2off(mod->globals);
    List_ptr2off(mod->funcs);
    List_ptr2off(mod->tests);
    List_ptr2off(mod->exprs);
    List_ptr2off(mod->imports);
    List_ptr2off(mod->enums);
    List_ptr2off(mod->types);

    ptr2off(&mod->globals);
    ptr2off(&mod->funcs);
    ptr2off(&mod->tests);
    ptr2off(&mod->exprs);
    ptr2off(&mod->imports);
    ptr2off(&mod->enums);
    ptr2off(&mod->types);
    ptr2off_s(&mod->name);
    ptr2off_s(&mod->moduleName);
}

static void (*ptr2off)(void**) = ptr2off_fwd;

void ptr2off_fwd(void** ptr)
{
    ptrdiff_t pd = 0;
    for_to(i, gPool->subpools->count)
    {
        pd = ptr - gPool->subpools->ref[i];
        if (pd > 0 and pd < gPool->subpools->used[i]) {
            pd |= i << 48; // high16 holds subpool index, low48 is the offset
            *ptr = pd;
            return;
        }
    }
    unreachable("ptr2off failed");
}

void ptr2off_rev(void** ptr)
{
    ptrdiff_t pd = ptr;
    int owner = pd >> 48;
    pd |= 0x0000ffffffffffff;
    if (owner < gPool->subpools->used)
        *ptr = pd + gPool->subpools->ref[owner];
    else
        unreachable("ptr2off failed");
}