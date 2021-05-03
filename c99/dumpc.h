
static void Import_dumpc(Import* import, int level) {
    printf("(Import[]) {{.name = \"%s\", .aliasOffset = "
           "%d}}",
        import->name, import->aliasOffset);
}

static void TypeSpec_dumpc(TypeSpec* spec, int level) {
    printf("&(TypeSpec) { .typeType=%d, .collectionType = %d, ", spec->typeType,
        spec->collectionType);
    if (spec->typeType == TYObject)
        printf(".type = &%s_%s, ", spec->type->isEnum ? "enum" : "type",
            spec->type->name);
    else
        printf(".name = \"%s\", ", spec->name);
    printf(".dims = %d, .nullable = %d }", spec->dims, spec->nullable);
}

static void Expr_dumpc(Expr* expr, int level, bool spacing, bool escapeStrings);

static void Var_dumpc(Var* var, int level);
static void Var_dumpc(Var* var, int level) {
    printf("{ .name = \"%s\", .spec = ", var->name);
    TypeSpec_dumpc(var->spec, 0);
    if (var->init) {
        printf(", .init = ");
        Expr_dumpc(var->init, 0, yes, no);
    }
    printf("}");
}

static void Scope_dumpc(Scope* scope, int level) {
    foreachn(Expr*, expr, exprList, scope->stmts) {
        Expr_dumpc(expr, 0, true, false);
        if (exprList->next) printf(", ");
        continue;

        switch (expr->kind) {
        case tkKeyword_case:
        case tkKeyword_match:
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr[expr->kind]);
            if (expr->left) Expr_dumpc(expr->left, 0, true, false);
            puts("");
            //, true, escapeStrings);
            if (expr->kind == tkKeyword_match) {
                if (expr->body) Scope_dumpc(expr->body, level);
                printf("%.*send %s\n", level, spaces, ""); // "match");
            } else {
                if (expr->body) Scope_dumpc(expr->body, level + STEP);
            }
            break;

        case tkKeyword_for:
        case tkKeyword_if:
        case tkKeyword_elif:
        case tkKeyword_else:
        case tkKeyword_while: {
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr[expr->kind]);
            if (expr->left) Expr_dumpc(expr->left, 0, true, false);
            puts("");
            if (expr->body)
                Scope_dumpc(expr->body, level + STEP); //, true, escapeStrings);
            //            const char* tok = TokenKind_repr[expr->kind];
            //            if (expr->kind == tkKeyword_else || expr->kind ==
            //            tkKeyword_elif)
            //                tok = "if";
            if (expr->kind == tkKeyword_if || expr->kind == tkKeyword_elif)
                if (exprList->next) {
                    Expr* next = exprList->next->item;
                    if (next->kind == tkKeyword_else
                        || next->kind == tkKeyword_elif)
                        break;
                }
            printf("%.*send %s\n", level, spaces, ""); // tok);
        } break;
        default: Expr_dumpc(expr, level, true, false); puts("");
        }
    }
}

static void Type_dumpc(Type* type, int level) {
    printf("static const Type type_%s = { .name =\"%s\" };\n", type->name,
        type->name);
    foreach (Var*, var, type->body->locals)
        Var_dumpc(var, 0), puts("");
}

static void JetEnum_dumpc(Type* type, int level) {
    printf("static const Type enum_%s = { .name =\"%s\" , .isEnum = true };\n",
        type->name, type->name);
}

static void Func_dumpc(Func* func, int level) {
    printf("static const Func func_%s = { .name = \"%s\", .selector = "
           "\"%s\", ",
        func->selector, func->name, func->selector);
    if (func->argCount) {
        printf(".args = ");
        PtrList* args = func->args;
        do {
            printf("&(PtrList) { .item = &(Var)");
            Var_dumpc(args->item, 0);
            if ((args = args->next)) { printf(", .next = "); }
        } while (args);
        for_to(i, func->argCount - 1) printf("} ");
        printf("}, ");
    }
    printf(".intrinsic = %d };\n", func->intrinsic);
    // if (func->isDefCtor || func->intrinsic) return;

    // printf("~ [ ");
    // if (func->recursivity > 1) printf("recurs:%d ", func->recursivity);
    // if (func->throws) printf("throws ");
    // if (func->isCalledFromWithinLoop) printf("looped ");
    // if (func->isCalledAsync) printf("asyncable ");
    // printf("]\n");

    // if (func->isDeclare) printf("declare ");

    // printf("%s%s(", func->isStmt ? "\n" : "function ", func->name);

    // foreachn(Var*, arg, args, func->args) {
    //     Var_dumpc(arg, level);
    //     printf(args->next ? ", " : "");
    // }
    // printf(")");

    // if (func->spec && !func->isStmt) {
    //     printf(" as ");
    //     TypeSpec_dumpc(func->spec, level);
    // }
    // if (func->isDeclare) {
    //     puts("");
    //     return;
    // } else if (!func->isStmt) {
    //     puts("");
    //     Scope_dumpc(func->body, level + STEP);
    //     puts("end\n");
    // } else {
    //     Expr* def = func->body->stmts->item;
    //     def = def->right; // its a return expr
    //     printf(" := ");
    //     Expr_dumpc(def, 0, true, false);
    //     puts("\n");
    // }
}

static void JetTest_dumpc(JetTest* test, int level) {
    // printf("test '%s'\n", test->name);
    // Scope_dumpc(test->body, level + STEP);
    // puts("end\n");
}
#define PRFIELD(ex, fi, fmt)                                                   \
    if (ex->fi) printf("." #fi " = " fmt ",\n", ex->fi)

static void Expr_dumpc(
    Expr* expr, int level, bool spacing, bool escapeStrings) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.

    printf("&(Expr) { .kind = %s,\n", TokenKind_names[expr->kind]);
    PRFIELD(expr, line, "%d");
    PRFIELD(expr, col, "%d");

    switch (expr->kind) {
    case tkNumber:
    case tkMultiDotNumber:
    case tkRawString:
    case tkRegexp:
    case tkIdentifier:
    case tkArgumentLabel:
    case tkString: printf(".string = \"%s\",\n", expr->string); break;

    case tkIdentifierResolved:
        printf(".var = &var_%d_%s, ", expr->var->line, expr->var->name);
        break;

    case tkSubscript:
    case tkFunctionCall:
        printf(".string = \"%s\",\n", expr->string);
        if (expr->left) {
            printf(".left = ");
            Expr_dumpc(expr->left, 0, 0, 0);
            printf(",\n");
        }
        break;

    case tkKeyword_no: break;
    case tkKeyword_yes: break;
    case tkKeyword_nil: break;

    case tkLineComment:
        printf("%s%s", TokenKind_repr[tkLineComment], expr->string);
        break;

        printf("%s(", expr->string);
        if (expr->left) Expr_dumpc(expr->left, 0, false, escapeStrings);
        printf(")");
        break;
    case tkFunctionCallResolved:
        // char* tmp = (expr->kind == tkFunctionCallResolved) ?
        //                                                    : expr->string;
        printf(".func = &func_%s, \n", expr->func->selector);
        if (expr->left) {
            printf(".left = ");
            Expr_dumpc(expr->left, 0, 0, 0);
            printf(",\n");
        }

        break;

    case tkSubscriptResolved: {
        char* tmp = (expr->kind == tkSubscriptResolved) ? expr->var->name
                                                        : expr->string;
        printf("%s[", tmp);
        if (expr->left) Expr_dumpc(expr->left, 0, false, escapeStrings);
        printf("]");
    } break;

    case tkObjectInit:
    case tkObjectInitResolved:
        break;

        // case tkPeriod:
        //     if (expr->left && expr->left->typeType == TYObject
        //         && !expr->left->var->spec->type->isEnum)
        //         Expr_dumpc(expr->left, 0, spacing, escapeStrings);
        //     printf(".");
        //     Expr_dumpc(expr->right, 0, spacing, escapeStrings);
        //     break;

    case tkVarAssign:
        // var x as XYZ = abc... -> becomes an Var and an Expr
        // (to keep location). Send it to Var_dumpc.
        assert(expr->var != NULL);
        Var_dumpc(expr->var, 0);
        break;

        // case tkArrayOpen:
        // case tkBraceOpen:
        //     printf("%s", TokenKind_repr[expr->kind]);
        //     if (expr->right)
        //         Expr_dumpc(
        //             expr->right, level, expr->kind != tkArrayOpen,
        //             escapeStrings);
        //     printf("%s",
        //     TokenKind_repr[TokenKind_reverseBracket(expr->kind)]); break;

        // case tkKeyword_in:
        // case tkKeyword_notin:
        //     // these seem to add precedence parens aruns expr->right if done
        //     as
        //     // normal binops. so ill do them separately here.
        //     Expr_dumpc(expr->left, 0, spacing, escapeStrings);
        //     printf("%s", TokenKind_repr[expr->kind]);
        //     Expr_dumpc(expr->right, 0, spacing, escapeStrings);
        //     break;

    default:
        if (!expr->prec) break;
        if (!expr->unary && expr->left) {
            printf(".left = ");
            Expr_dumpc(expr->left, 0, 0, 0);
            printf(",\n");
        }
        if (expr->right) {
            printf(".right = ");
            Expr_dumpc(expr->right, 0, 0, 0);
            printf(",\n");
        }
    }

    PRFIELD(expr, throws, "%d");
    PRFIELD(expr, prec, "%d");
    PRFIELD(expr, unary, "%d");
    PRFIELD(expr, rassoc, "%d");
    // printf(".throws = %d,\n", expr->throws);
    printf("}");
}

static void Module_dumpc(Module* module) {

    foreach (Import*, import, module->imports)
        Import_dumpc(import, 0);

    puts("");

    puts("");

    foreach (Type*, type, module->types)
        Type_dumpc(type, 0);

    foreach (Type*, en, module->enums)
        JetEnum_dumpc(en, 0);

    foreach (Func*, func, module->funcs)
        Func_dumpc(func, 0);

    foreach (JetTest*, test, module->tests)
        JetTest_dumpc(test, 0);

    foreach (Var*, var, module->scope->locals)
        Var_dumpc(var, 0), puts("");
}
