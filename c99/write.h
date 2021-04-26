
static void JetImport_write(JetImport* import, int level) {
    char* alias = import->name + import->aliasOffset;
    printf("import %s%s%s\n", import->name, alias ? " as " : "",
        alias ? alias : "");
}

static void JetTypeSpec_write(JetTypeSpec* spec, int level) {
    switch (spec->typeType) {
    case TYObject: printf("%s", spec->type->name); break;
    case TYUnresolved: printf("%s", spec->name); break;
    default: printf("%s", TypeType_name(spec->typeType)); break;
    }

    switch (spec->collectionType) {
    case CTYDictS: printf("[DICTK]"); break;
    case CTYArray: printf("[]"); break;
    case CTYTensor:
        if (spec->dims) {
            static const char* dimsstr
                = ":,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:";
            printf("[%.*s]", 2 * spec->dims - 1, dimsstr);
        } else {
            printf("[?]");
        }
        break;
    default:;
    }
}

static void JetExpr_write(
    JetExpr* expr, int level, bool spacing, bool escapeStrings);

static void JetVar_write(JetVar* var, int level) {
    printf("%.*s%s%s", level, spaces,
        var->isVar       ? "var "
            : var->isLet ? "let "
                         : "",
        var->name);
    TokenKind kind = var->init ? var->init->kind : tkUnknown;

    bool genType = false; // set it here to override
    // if ((var->init and var->init->kind == tkFunctionCall
    //         and !strcmp(var->init->string, var->spec->name))) {
    // } else if ((var->init and var->init->kind == tkNumber
    //                or var->init->kind == tkRegexp or var->init->kind ==
    //                tkString or var->init->kind == tkRawString)) {
    if (kind == tkFunctionCall // unresolved constructor
        && !strcmp(var->init->string, var->spec->name)) {
    } else if (kind == tkFunctionCallResolved && var->spec->typeType == TYObject
        && !strcmp(var->init->func->name, var->spec->type->name)) {
        // resolved constructor, so type is also resolved
    } else if ((kind == tkNumber || kind == tkRegexp || kind == tkKeyword_no
                   || kind == tkKeyword_yes || kind == tkString
                   || kind == tkRawString)) { // simple literals
    } else if (var->init
        && (isBoolOp(var->init)
            || isCmpOp(var->init))) { // simple stuff that gives Boolean
    }
    // else if (var->spec->typeType == TYObject
    //     && var->spec->type->isEnum) {
    // }
    else if (var->spec->typeType == TYErrorType
        || var->spec->typeType == TYNoType
        || (var->spec->typeType == TYUnresolved && *var->spec->name == '\0')) {
        genType = false;
    } else {
        genType = true;
    }

    if (genType) {
        printf(" "); // as ");
        JetTypeSpec_write(var->spec, level + STEP);
    }
    // }
    // else {
    //     // should make this Expr_defaultType and it does it recursively
    //     for
    //     // [, etc
    //     const char* ctyp = TokenKind_defaultType(
    //         self->init ? self->init->kind : tkUnknown);
    //     if (self->init and self->init->kind == tkListLiteral)
    //         ctyp = TokenKind_defaultType(
    //             self->init->right ? self->init->right->kind : tkUnknown);
    //     if (self->init and self->init->kind == tkFunctionCall
    //         and *self->init->name >= 'A' and *self->init->name <= 'Z')
    //         ctyp = NULL;
    //     if (ctyp) printf(" as %s", ctyp);
    // }
    if (var->init) {
        printf(" = ");
        JetExpr_write(var->init, 0, true, false);
    }
}

static void JetScope_write(JetScope* scope, int level) {
    foreachn(JetExpr*, expr, exprList, scope->stmts) {
        switch (expr->kind) {
        case tkKeyword_case:
        case tkKeyword_match:
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr[expr->kind]);
            if (expr->left) JetExpr_write(expr->left, 0, true, false);
            puts("");
            //, true, escapeStrings);
            if (expr->kind == tkKeyword_match) {
                if (expr->body) JetScope_write(expr->body, level);
                printf("%.*send %s\n", level, spaces, ""); // "match");
            } else {
                if (expr->body) JetScope_write(expr->body, level + STEP);
            }
            break;

        case tkKeyword_for:
        case tkKeyword_if:
        case tkKeyword_elif:
        case tkKeyword_else:
        case tkKeyword_while: {
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr[expr->kind]);
            if (expr->left) JetExpr_write(expr->left, 0, true, false);
            puts("");
            if (expr->body)
                JetScope_write(
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
        default: JetExpr_write(expr, level, true, false); puts("");
        }
    }
}

static void JetType_write(JetType* type, int level) {
    if (type->isDeclare) printf("declare ");
    printf("type %s", type->name);
    if (type->super) {
        printf(" extends ");
        JetTypeSpec_write(type->super, level);
    }
    puts("");
    if (!type->body) return;

    foreach (JetExpr*, stmt, type->body->stmts) {
        if (!stmt) continue;
        JetExpr_write(stmt, level + STEP, true, false);
        puts("");
    }
    puts("end\n");
}

static void JetEnum_write(JetType* type, int level) {
    // if (!type->body) printf("declare ");
    printf("enum %s\n", type->name);
    // if (type->super) {
    //     printf(" extends ");
    //     JetTypeSpec_write(type->super, level);
    // }
    // puts("");
    if (type->body) foreach (JetExpr*, stmt, type->body->stmts) {
            if (!stmt) continue;
            JetExpr_write(stmt, level + STEP, true, false);
            puts("");
        }
    puts("end\n");
}

static void JetFunc_write(JetFunc* func, int level) {
    if (func->isDefCtor || func->intrinsic) return;

    printf("~ [ ");
    if (func->recursivity > 1) printf("recurs:%d ", func->recursivity);
    if (func->throws) printf("throws ");
    if (func->isCalledFromWithinLoop) printf("looped ");
    if (func->isCalledAsync) printf("asyncable ");
    printf("]\n");

    if (func->isDeclare) printf("declare ");

    printf("%s%s(", func->isStmt ? "\n" : "function ", func->name);

    foreachn(JetVar*, arg, args, func->args) {
        JetVar_write(arg, level);
        printf(args->next ? ", " : "");
    }
    printf(")");

    if (func->spec && !func->isStmt) {
        printf(" as ");
        JetTypeSpec_write(func->spec, level);
    }
    if (func->isDeclare) {
        puts("");
        return;
    } else if (!func->isStmt) {
        puts("");
        JetScope_write(func->body, level + STEP);
        puts("end\n");
    } else {
        JetExpr* def = func->body->stmts->item;
        def = def->right; // its a return expr
        printf(" := ");
        JetExpr_write(def, 0, true, false);
        puts("\n");
    }
}

static void JetTest_write(JetTest* test, int level) {
    printf("test '%s'\n", test->name);
    JetScope_write(test->body, level + STEP);
    puts("end\n");
}

static void JetExpr_write(
    JetExpr* expr, int level, bool spacing, bool escapeStrings) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.
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
        if (expr->left) JetExpr_write(expr->left, 0, false, escapeStrings);
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
                JetExpr_write(arg, 0, false, escapeStrings);
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
        if (expr->left) JetExpr_write(expr->left, 0, false, escapeStrings);
        printf("]");
    } break;

    case tkObjectInit:
    case tkObjectInitResolved: break;

    case tkPeriod:
        if (expr->left && expr->left->typeType == TYObject
            && !expr->left->var->spec->type->isEnum)
            JetExpr_write(expr->left, 0, spacing, escapeStrings);
        printf(".");
        JetExpr_write(expr->right, 0, spacing, escapeStrings);
        break;

    case tkVarAssign:
        // var x as XYZ = abc... -> becomes an JetVar and an JetExpr
        // (to keep location). Send it to JetVar_write.
        assert(expr->var != NULL);
        JetVar_write(expr->var, 0);
        break;

    case tkArrayOpen:
    case tkBraceOpen:
        printf("%s", TokenKind_repr[expr->kind]);
        if (expr->right)
            JetExpr_write(
                expr->right, level, expr->kind != tkArrayOpen, escapeStrings);
        printf("%s", TokenKind_repr[TokenKind_reverseBracket(expr->kind)]);
        break;

    case tkKeyword_in:
    case tkKeyword_notin:
        // these seem to add precedence parens aruns expr->right if done as
        // normal binops. so ill do them separately here.
        JetExpr_write(expr->left, 0, spacing, escapeStrings);
        printf("%s", TokenKind_repr[expr->kind]);
        JetExpr_write(expr->right, 0, spacing, escapeStrings);
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
            JetExpr_write(expr->left, 0,
                spacing && !leftBr && expr->kind != tkOpColon, escapeStrings);
        if (leftBr) putc(lpc, stdout);

        printf("%s",
            spacing ? TokenKind_srepr[expr->kind] : TokenKind_repr[expr->kind]);

        char rpo = rightBr && expr->right->kind == tkOpColon ? '[' : '(';
        char rpc = rightBr && expr->right->kind == tkOpColon ? ']' : ')';
        if (rightBr) putc(rpo, stdout);
        if (expr->right)
            JetExpr_write(expr->right, 0,
                spacing && !rightBr && expr->kind != tkOpColon, escapeStrings);
        if (rightBr) putc(rpc, stdout);

        if (expr->kind == tkOpPower && !spacing) putc(')', stdout);
    }
}

static void JetModule_write(JetModule* module) {
    printf("~ module %s\n", module->name);

    foreach (JetImport*, import, module->imports)
        JetImport_write(import, 0);

    puts("");

    foreach (JetVar*, var, module->scope->locals)
        JetVar_write(var, 0), puts("");

    puts("");

    foreach (JetType*, type, module->types)
        JetType_write(type, 0);

    foreach (JetType*, en, module->enums)
        JetEnum_write(en, 0);

    foreach (JetFunc*, func, module->funcs)
        JetFunc_write(func, 0);

    foreach (JetTest*, test, module->tests)
        JetTest_write(test, 0);
}