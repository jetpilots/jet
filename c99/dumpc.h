
static void JetImport_dumpc(JetImport* import, int level) {
    printf("(JetImport[]) {{.name = \"%s\", .aliasOffset = "
           "%d}}",
        import->name, import->aliasOffset);
}

static void JetTypeSpec_dumpc(JetTypeSpec* spec, int level) {
    // char* name = spec->typeType==TYObject?spec->type->name:spec->name;
    printf("(JetTypeSpec[]) {{ .typeType=%d, "
           ".collectionType = %d, ",
        spec->typeType, spec->collectionType);
    if (spec->typeType == TYObject)
        printf(".type = type_%s, ", spec->type->name);
    else
        printf(".name = \"%s\", ", spec->name);
    printf(".dims = %d, .nullable = %d }}", spec->dims, spec->nullable);
}

static void JetExpr_dumpc(
    JetExpr* expr, int level, bool spacing, bool escapeStrings);

static void JetVar_dumpc(JetVar* var, int level) {
    printf("(JetVar[]) {{ .name = \"%s\", .spec = ", var->name);
    JetTypeSpec_dumpc(var->spec, 0);
    printf("}}");
}

static void JetScope_dumpc(JetScope* scope, int level) {
    foreachn(JetExpr*, expr, exprList, scope->stmts) {
        JetExpr_dumpc(expr, 0, true, false);
        if (exprList->next) printf(", ");
        continue;

        switch (expr->kind) {
        case tkKeyword_case:
        case tkKeyword_match:
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr[expr->kind]);
            if (expr->left) JetExpr_dumpc(expr->left, 0, true, false);
            puts("");
            //, true, escapeStrings);
            if (expr->kind == tkKeyword_match) {
                if (expr->body) JetScope_dumpc(expr->body, level);
                printf("%.*send %s\n", level, spaces, ""); // "match");
            } else {
                if (expr->body) JetScope_dumpc(expr->body, level + STEP);
            }
            break;

        case tkKeyword_for:
        case tkKeyword_if:
        case tkKeyword_elif:
        case tkKeyword_else:
        case tkKeyword_while: {
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr[expr->kind]);
            if (expr->left) JetExpr_dumpc(expr->left, 0, true, false);
            puts("");
            if (expr->body)
                JetScope_dumpc(
                    expr->body, level + STEP); //, true, escapeStrings);
            //            const char* tok = TokenKind_repr[expr->kind];
            //            if (expr->kind == tkKeyword_else || expr->kind ==
            //            tkKeyword_elif)
            //                tok = "if";
            if (expr->kind == tkKeyword_if || expr->kind == tkKeyword_elif)
                if (exprList->next) {
                    JetExpr* next = exprList->next->item;
                    if (next->kind == tkKeyword_else
                        || next->kind == tkKeyword_elif)
                        break;
                }
            printf("%.*send %s\n", level, spaces, ""); // tok);
        } break;
        default: JetExpr_dumpc(expr, level, true, false); puts("");
        }
    }
}

static void JetType_dumpc(JetType* type, int level) {

    printf("static JetType* type_%s = (JetType[]){{ .name =\"%s\" }};\n",
        type->name, type->name);
    // return;
    //     if (type->isDeclare) printf("declare ");
    //     printf("type %s", type->name);
    //     if (type->super) {
    //         printf(" extends ");
    //         JetTypeSpec_dumpc(type->super, level);
    //     }
    //     puts("");
    //     if (!type->body) return;

    //     foreach (JetExpr*, stmt, type->body->stmts) {
    //         if (!stmt) continue;
    //         JetExpr_dumpc(stmt, level + STEP, true, false);
    //         puts("");
    //     }
    //     puts("end\n");
}

static void JetEnum_dumpc(JetType* type, int level) {
    // if (!type->body) printf("declare ");
    printf("enum %s\n", type->name);
    // if (type->super) {
    //     printf(" extends ");
    //     JetTypeSpec_dumpc(type->super, level);
    // }
    // puts("");
    if (type->body) foreach (JetExpr*, stmt, type->body->stmts) {
            if (!stmt) continue;
            JetExpr_dumpc(stmt, level + STEP, true, false);
            puts("");
        }
    puts("end\n");
}

static void JetFunc_dumpc(JetFunc* func, int level) {
    printf(
        "static JetFunc* func_%s = (JetFunc[]){{ .name = \"%s\", .selector = "
        "\"%s\", ",
        func->selector, func->name, func->selector);
    if (func->argCount) {
        printf(".args = ");
        PtrList* args = func->args;
        do {
            printf("(PtrList[]) {{ .item = ");
            JetVar_dumpc(args->item, 0);
            if ((args = args->next)) {
                printf(", .next = ");
                // printf("}} ");
            }
        } while (args);
        for_to(i, func->argCount - 1) printf("}} ");
        printf("}}, ");
    }
    printf(".intrinsic = %d }};\n", func->intrinsic);
    // if (func->isDefCtor || func->intrinsic) return;

    // printf("~ [ ");
    // if (func->recursivity > 1) printf("recurs:%d ", func->recursivity);
    // if (func->throws) printf("throws ");
    // if (func->isCalledFromWithinLoop) printf("looped ");
    // if (func->isCalledAsync) printf("asyncable ");
    // printf("]\n");

    // if (func->isDeclare) printf("declare ");

    // printf("%s%s(", func->isStmt ? "\n" : "function ", func->name);

    // foreachn(JetVar*, arg, args, func->args) {
    //     JetVar_dumpc(arg, level);
    //     printf(args->next ? ", " : "");
    // }
    // printf(")");

    // if (func->spec && !func->isStmt) {
    //     printf(" as ");
    //     JetTypeSpec_dumpc(func->spec, level);
    // }
    // if (func->isDeclare) {
    //     puts("");
    //     return;
    // } else if (!func->isStmt) {
    //     puts("");
    //     JetScope_dumpc(func->body, level + STEP);
    //     puts("end\n");
    // } else {
    //     JetExpr* def = func->body->stmts->item;
    //     def = def->right; // its a return expr
    //     printf(" := ");
    //     JetExpr_dumpc(def, 0, true, false);
    //     puts("\n");
    // }
}

static void JetTest_dumpc(JetTest* test, int level) {
    // printf("test '%s'\n", test->name);
    // JetScope_dumpc(test->body, level + STEP);
    // puts("end\n");
}

static void JetExpr_dumpc(
    JetExpr* expr, int level, bool spacing, bool escapeStrings) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.

    printf("CT_EXPR(.kind = %s)", TokenKind_names[expr->kind]);

    return;
    printf("%.*s", level, spaces);

    switch (expr->kind) {
    case tkNumber:
    case tkMultiDotNumber: printf("%s", expr->string); break;
    case tkRawString: printf("'%s'", expr->string + 1); break;
    case tkRegexp: printf("`%s`", expr->string + 1); break;

    case tkIdentifier:
    case tkArgumentLabel:
    case tkIdentifierResolved: {
        char* tmp = (expr->kind != tkIdentifierResolved) ? expr->string
                                                         : expr->var->name;
        printf("%s", tmp);
    } break;

    case tkString:
        printf(escapeStrings ? "\\%s\\\"" : "%s\"", expr->string);
        break;
    case tkKeyword_no: printf("no"); break;
    case tkKeyword_yes: printf("yes"); break;
    case tkKeyword_nil: printf("nil"); break;

    case tkLineComment:
        printf("%s%s", TokenKind_repr[tkLineComment], expr->string);
        break;

    case tkFunctionCall:
        printf("%s(", expr->string);
        if (expr->left) JetExpr_dumpc(expr->left, 0, false, escapeStrings);
        printf(")");
        break;
    case tkFunctionCallResolved:
        // char* tmp = (expr->kind == tkFunctionCallResolved) ?
        //                                                    : expr->string;
        printf("%s(", expr->func->name);
        if (expr->left) {
            JetExpr* carg = expr->left;
            foreachn(JetVar*, var, listp, expr->func->args) {
                if (!carg) break;
                JetExpr* arg = (carg->kind == tkOpComma) ? carg->left : carg;
                if (arg->kind != tkOpAssign && listp != expr->func->args)
                    printf("%s=", var->name);
                JetExpr_dumpc(arg, 0, false, escapeStrings);
                if (listp->next) printf(", ");
                carg = (carg->kind == tkOpComma) ? carg->right : NULL;
            }
        }
        printf(")");
        break;

    case tkSubscript:
    case tkSubscriptResolved: {
        char* tmp = (expr->kind == tkSubscriptResolved) ? expr->var->name
                                                        : expr->string;
        printf("%s[", tmp);
        if (expr->left) JetExpr_dumpc(expr->left, 0, false, escapeStrings);
        printf("]");
    } break;

    case tkObjectInit:
    case tkObjectInitResolved: break;

    case tkPeriod:
        if (expr->left && expr->left->typeType == TYObject
            && !expr->left->var->spec->type->isEnum)
            JetExpr_dumpc(expr->left, 0, spacing, escapeStrings);
        printf(".");
        JetExpr_dumpc(expr->right, 0, spacing, escapeStrings);
        break;

    case tkVarAssign:
        // var x as XYZ = abc... -> becomes an JetVar and an JetExpr
        // (to keep location). Send it to JetVar_dumpc.
        assert(expr->var != NULL);
        JetVar_dumpc(expr->var, 0);
        break;

    case tkArrayOpen:
    case tkBraceOpen:
        printf("%s", TokenKind_repr[expr->kind]);
        if (expr->right)
            JetExpr_dumpc(
                expr->right, level, expr->kind != tkArrayOpen, escapeStrings);
        printf("%s", TokenKind_repr[TokenKind_reverseBracket(expr->kind)]);
        break;

    case tkKeyword_in:
    case tkKeyword_notin:
        // these seem to add precedence parens aruns expr->right if done as
        // normal binops. so ill do them separately here.
        JetExpr_dumpc(expr->left, 0, spacing, escapeStrings);
        printf("%s", TokenKind_repr[expr->kind]);
        JetExpr_dumpc(expr->right, 0, spacing, escapeStrings);
        break;

    default:
        if (!expr->prec) break;
        // not an operator, but this should be error if you reach here
        bool leftBr
            = expr->left && expr->left->prec && expr->left->prec < expr->prec;
        bool rightBr = expr->right && expr->right->prec
            && expr->right->kind != tkKeyword_return // found in 'or return'
            && expr->right->prec < expr->prec;

        if (expr->kind == tkOpColon) {
            // expressions like arr[a:x-3:2] should become
            // arr[a:(x-3):2]
            // or list literals [8, 9, 6, 77, sin(c)]
            if (expr->left) switch (expr->left->kind) {
                case tkNumber:
                case tkIdentifier:
                case tkString:
                case tkOpColon:
                case tkMultiDotNumber:
                case tkOpUnaryMinus: break;
                default: leftBr = true;
                }
            if (expr->right) switch (expr->right->kind) {
                case tkNumber:
                case tkIdentifier:
                case tkString:
                case tkOpColon:
                case tkMultiDotNumber:
                case tkOpUnaryMinus: break;
                default: rightBr = true;
                }
        }

        if (expr->kind == tkOpPower && !spacing) putc('(', stdout);

        char lpo = leftBr && expr->left->kind == tkOpColon ? '[' : '(';
        char lpc = leftBr && expr->left->kind == tkOpColon ? ']' : ')';
        if (leftBr) putc(lpo, stdout);
        if (expr->left)
            JetExpr_dumpc(expr->left, 0,
                spacing && !leftBr && expr->kind != tkOpColon, escapeStrings);
        if (leftBr) putc(lpc, stdout);

        printf("%s",
            spacing ? TokenKind_srepr[expr->kind] : TokenKind_repr[expr->kind]);

        char rpo = rightBr && expr->right->kind == tkOpColon ? '[' : '(';
        char rpc = rightBr && expr->right->kind == tkOpColon ? ']' : ')';
        if (rightBr) putc(rpo, stdout);
        if (expr->right)
            JetExpr_dumpc(expr->right, 0,
                spacing && !rightBr && expr->kind != tkOpColon, escapeStrings);
        if (rightBr) putc(rpc, stdout);

        if (expr->kind == tkOpPower && !spacing) putc(')', stdout);
    }
}

static void JetModule_dumpc(JetModule* module) {
    printf("// module %s\n", module->name);

    foreach (JetImport*, import, module->imports)
        JetImport_dumpc(import, 0);

    puts("");

    foreach (JetVar*, var, module->scope->locals)
        JetVar_dumpc(var, 0), puts("");

    puts("");

    foreach (JetType*, type, module->types)
        JetType_dumpc(type, 0);

    foreach (JetType*, en, module->enums)
        JetEnum_dumpc(en, 0);

    foreach (JetFunc*, func, module->funcs)
        JetFunc_dumpc(func, 0);

    foreach (JetTest*, test, module->tests)
        JetTest_dumpc(test, 0);
}
