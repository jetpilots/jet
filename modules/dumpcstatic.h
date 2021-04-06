// This could be generated easily. Every struct has associated dumpcstatic and
// off2ptr funcs, and you can dump the pool contents out to disk by converting
// all ptrs to offsets and reconverting them on read.

// You may want to save main()'s stack vars as well (everything else is temp)
// as well as stuff that goes into static storage / .data / .bss

// keep a global var dumpcstatic_dirn that says whether the implementation of
// dumpcstatic should be forward (to offset) or backward (from offset). This way
// you need only 1 dumpcstatic func for each type. Best is keep a func ptr that
// takes you to the actual implementation.

// void dumpcstatic_s_fwd(char** str) { }

// void dumpcstatic_fwd(void** ptr) {
//     size_t pd = 0;
//     pd = *ptr - gPool->ref;
//     if (pd > 0 && pd < gPool->cap) {
//         pd |= 0xffffUL << 48;
//         *ptr = (void*)pd;
//         return;
//     }

//     for_to(i, gPool->ptrs.used) {
//         pd = *ptr - gPool->ptrs.ref[i];
//         if (pd > 0 && pd < gPool->caps.ref[i]) {
//             pd |= ((size_t)i) << 48;
//             // high16 holds subpool index, low48 is the offset
//             *ptr = (void*)pd;
//             return;
//         }
//     }

//     unreachable("dumpcstatic failed %s", "-");
// }

// void dumpcstatic_rev(void** ptr) {
//     size_t pd = (size_t)*ptr;
//     unsigned short owner = pd >> 48;
//     pd |= 0x0000ffffffffffff;
//     if (owner == 0xffff)
//         *ptr = pd + gPool->ref;
//     else if (owner < gPool->ptrs.used)
//         *ptr = pd + gPool->ptrs.ref[owner];
//     else
//         unreachable("dumpcstatic failed %s", "-");
// }

// static void (*dumpcstatic)(void**) = dumpcstatic_fwd;
// static void (*dumpcstatic_s)(char**) = dumpcstatic_s_fwd;

// BEGIN STRUCT HELPERS

void List_dumpcstatic(PtrList* list) {
    // CALL THIS AFTER ITERATING THROUGH THE LIST!!
    dumpcstatic(&list->item);
    dumpcstatic((void**)&list->next);
}

void ASTTypeSpec_dumpcstatic(ASTTypeSpec* typeSpec);

void ASTExpr_dumpcstatic(ASTExpr* expr) { }
void ASTUnits_dumpcstatic(ASTUnits* units) { }

void ASTVar_dumpcstatic(ASTVar* var) {
    printf(".name = \"%s\",", var->name);
    printf(".typeSpec = ");
    ASTTypeSpec_dumpcstatic(var->typeSpec);
    printf(".init = ");
    ASTExpr_dumpcstatic(var->init);

    // dumpcstatic_s(&var->name);
    // dumpcstatic((void**)&var->typeSpec);
    // dumpcstatic((void**)&var->init);
}

void ASTScope_dumpcstatic(ASTScope* scope) {
    foreach (ASTExpr*, stmt, scope->stmts)
        ASTExpr_dumpcstatic(stmt);
    foreach (ASTVar*, var, scope->locals)
        ASTVar_dumpcstatic(var);
    List_dumpcstatic(scope->stmts);
    List_dumpcstatic(scope->locals);
    // DO NOT process the parent!
    dumpcstatic((void**)&scope->stmts);
    dumpcstatic((void**)&scope->locals);
    dumpcstatic((void**)&scope->parent);
}

void ASTType_dumpcstatic(ASTType* type) {
    printf(".name = \"%s\",", type->name);
    printf(".isEnum = %d,", type->isEnum);

    ASTScope_dumpcstatic(type->body);
    dumpcstatic((void**)&type->body);
    dumpcstatic_s(&type->name);
    // TODO: type super?
}

void ASTImport_dumpcstatic(ASTImport* import) {
    // ASTScope_dumpcstatic(type->body);
    // dumpcstatic(&type->body);
    // dumpcstatic_s(&import->name);
    // TODO: type super?
    printf(".name = \"%s\",", import->name);
    printf(".alias = \"%s\",", import->alias);
}

void ASTTypeSpec_dumpcstatic(ASTTypeSpec* typeSpec) {
    if (typeSpec->typeType == TYObject) {
        ASTType_dumpcstatic(typeSpec->type);
        dumpcstatic((void**)&typeSpec->type);
    } else if (TypeType_isnum(typeSpec->typeType) && typeSpec->units) {
        ASTUnits_dumpcstatic(typeSpec->units);
        dumpcstatic((void**)&typeSpec->units);
    } else {
        // what's left. unresolved?
        unreachable("%s", "-");
    }
}

void ASTEnum_dumpcstatic(ASTType* enm) {
    ASTScope_dumpcstatic(enm->body);
    dumpcstatic((void**)&enm->body);
    dumpcstatic_s(&enm->name);
    // TODO: type super?
}

void ASTFunc_dumpcstatic(ASTFunc* func) {

    printf(".name = \"%s\",", func->name);
    printf(".selector = \"%s\",", func->selector);
    printf(".isDeclare = %d,", func->isDeclare);
    printf(".args = ");
    List_dumpc(ASTVar, func->args);
    printf(".returnSpec = ", func->name);
    // printf(".args = \"%s\",", func->name);

    // foreach (ASTVar*, arg, func->args)
    //     ASTVar_dumpcstatic(arg);
    // List_dumpcstatic(ASTVar, func->args);
    // ASTScope_dumpcstatic(func->body);
    // ASTTypeSpec_dumpcstatic(func->returnSpec);

    // dumpcstatic((void**)&func->body);
    // dumpcstatic((void**)&func->returnSpec);
    // dumpcstatic((void**)&func->args);

    // dumpcstatic_s(&func->name);
    // dumpcstatic_s(&func->selector);
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
