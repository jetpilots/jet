
static void ASTImport_lint(ASTImport* import, int level) {
    printf("import %s%s%s%s\n", import->isPackage ? "@" : "",
        import->importFile, import->hasAlias ? " as " : "",
        import->hasAlias ? import->importFile + import->aliasOffset : "");
}

static void ASTTypeSpec_lint(ASTTypeSpec* spec, int level) {
    switch (spec->typeType) {
    case TYObject:
        printf("%s", spec->type->name);
        break;
    case TYUnresolved:
        printf("%s", spec->name);
        break;
    default:
        printf("%s", TypeType_name(spec->typeType));
        break;
    }

    switch (spec->collectionType) {
    case CTYDictS:
        printf("[DICTK]");
        break;
    case CTYArray:
        printf("[]");
        break;
    case CTYTensor:
        if (spec->dims) {
            static const char* dimsstr
                = ":,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:";
            //        char str[32];
            //        str[31]=0;
            //        int sz= 2 + dims + (dims ? (dims-1) : 0) + 1;
            //        str[0] = '[';
            //        str[sz-2] = ']';
            //        str[sz-1] = 0;
            //        for (i=0; i<sz; i++) {
            //            str[i*2+1]=':';
            //            str[i*2+2]=',';
            //        }
            printf("[%.*s]", 2 * spec->dims - 1, dimsstr);
        } else {
            printf("[?]");
        }
        break;
    default:;
    }
}

static void ASTExpr_lint(
    ASTExpr* expr, int level, bool spacing, bool escapeStrings);

static void ASTVar_lint(ASTVar* var, int level) {
    printf("%.*s%s%s", level, spaces,
        var->isVar       ? "var "
            : var->isLet ? "let "
                         : "",
        var->name);
    TokenKind kind = var->init ? var->init->kind : tkUnknown;

    bool genType = true; // set it here to override
    // if ((var->init and var->init->kind == tkFunctionCall
    //         and !strcmp(var->init->string, var->typeSpec->name))) {
    // } else if ((var->init and var->init->kind == tkNumber
    //                or var->init->kind == tkRegexp or var->init->kind ==
    //                tkString or var->init->kind == tkRawString)) {
    if (kind == tkFunctionCall // unresolved constructor
        and !strcmp(var->init->string, var->typeSpec->name)) {
    } else if (kind == tkFunctionCallResolved
        and var->typeSpec->typeType == TYObject
        and !strcmp(var->init->func->name, var->typeSpec->type->name)) {
        // resolved constructor, so type is also resolved
    } else if ((kind == tkNumber or kind == tkRegexp or kind == tkString
                   or kind == tkRawString)) { // simple literals
    } else if (var->typeSpec->typeType == TYErrorType
        or var->typeSpec->typeType == TYNoType
        or (var->typeSpec->typeType == TYUnresolved
            and *var->typeSpec->name == '\0')) {
        genType = false;
    } else {
        genType = true;
    }

    if (genType) {
        printf(" as ");
        ASTTypeSpec_lint(var->typeSpec, level + STEP);
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
        ASTExpr_lint(var->init, 0, true, false);
    }
}

static void ASTScope_lint(ASTScope* scope, int level) {
    foreachn(ASTExpr*, expr, exprList, scope->stmts) {
        switch (expr->kind) {
        case tkKeyword_for:
        case tkKeyword_if:
        case tkKeyword_elif:
        case tkKeyword_else:
        case tkKeyword_while: {
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr(expr->kind, false));
            if (expr->left) ASTExpr_lint(expr->left, 0, true, false);
            puts("");
            if (expr->body)
                ASTScope_lint(
                    expr->body, level + STEP); //, true, escapeStrings);
            const char* tok = TokenKind_repr(expr->kind, false);
            if (expr->kind == tkKeyword_else or expr->kind == tkKeyword_elif)
                tok = "if";
            if (expr->kind == tkKeyword_if or expr->kind == tkKeyword_elif)
                if (exprList->next) {
                    ASTExpr* next = exprList->next->item;
                    if (next->kind == tkKeyword_else
                        or next->kind == tkKeyword_elif)
                        break;
                }
            printf("%.*send %s\n", level, spaces, ""); // tok);
        } break;
        default:
            ASTExpr_lint(expr, level, true, false);
            puts("");
        }
    }
}

static void ASTType_lint(ASTType* type, int level) {
    if (not type->body) printf("declare ");
    printf("type %s", type->name);
    if (type->super) {
        printf(" extends ");
        ASTTypeSpec_lint(type->super, level);
    }
    puts("");
    if (not type->body) return;

    foreach (ASTExpr*, stmt, type->body->stmts) {
        if (not stmt) continue;
        ASTExpr_lint(stmt, level + STEP, true, false);
        puts("");
    }
    puts("end\n");
}

static void ASTFunc_lint(ASTFunc* func, int level) {
    if (func->isDefCtor or func->intrinsic) return;
    if (func->isDeclare) printf("declare ");

    printf("%s%s(", func->isStmt ? "\n" : "function ", func->name);

    foreachn(ASTVar*, arg, args, func->args) {
        ASTVar_lint(arg, level);
        printf(args->next ? ", " : "");
    }
    printf(")");

    if (func->returnSpec and not func->isStmt) {
        printf(" as ");
        ASTTypeSpec_lint(func->returnSpec, level);
    }
    if (func->isDeclare) {
        puts("");
        return;
    } else if (not func->isStmt) {
        puts("");
        ASTScope_lint(func->body, level + STEP);
        puts("end\n");
    } else {
        ASTExpr* def = func->body->stmts->item;
        def = def->right; // its a return expr
        printf(" := ");
        ASTExpr_lint(def, 0, true, false);
        puts("\n");
    }
}

static void ASTTest_lint(ASTTest* test, int level) {
    printf("test '%s'\n", test->name);
    ASTScope_lint(test->body, level + STEP);
    puts("end\n");
}

static void ASTExpr_lint(
    ASTExpr* expr, int level, bool spacing, bool escapeStrings) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.
    printf("%.*s", level, spaces);

    switch (expr->kind) {
    case tkNumber:
    case tkMultiDotNumber:
        printf("%s", expr->string);
        break;
    case tkRawString:
        printf("'%s'", expr->string + 1);
        break;
    case tkRegexp:
        printf("`%s`", expr->string + 1);
        break;

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

    case tkLineComment:
        printf("%s%s", TokenKind_repr(tkLineComment, *expr->string != ' '),
            expr->string);
        break;

    case tkFunctionCall:
    case tkFunctionCallResolved: {
        char* tmp = (expr->kind == tkFunctionCallResolved) ? expr->func->name
                                                           : expr->string;
        printf("%s(", tmp);
        if (expr->left) ASTExpr_lint(expr->left, 0, false, escapeStrings);
        printf(")");
    } break;

    case tkSubscript:
    case tkSubscriptResolved: {
        char* tmp = (expr->kind == tkSubscriptResolved) ? expr->var->name
                                                        : expr->string;
        printf("%s[", tmp);
        if (expr->left) ASTExpr_lint(expr->left, 0, false, escapeStrings);
        printf("]");
    } break;

    case tkObjectInit:
    case tkObjectInitResolved:
        break;

    case tkVarAssign:
        // var x as XYZ = abc... -> becomes an ASTVar and an ASTExpr
        // (to keep location). Send it to ASTVar_lint.
        assert(expr->var != NULL);
        ASTVar_lint(expr->var, 0);
        break;

    case tkArrayOpen:
    case tkBraceOpen:
        printf("%s", tkrepr[expr->kind]);
        if (expr->right)
            ASTExpr_lint(
                expr->right, level, expr->kind != tkArrayOpen, escapeStrings);
        printf("%s", tkrepr[TokenKind_reverseBracket(expr->kind)]);
        break;

    default:
        if (not expr->prec) break;
        // not an operator, but this should be error if you reach here
        bool leftBr
            = expr->left and expr->left->prec and expr->left->prec < expr->prec;
        bool rightBr = expr->right and expr->right->prec
            and expr->right->kind != tkKeyword_return // found in 'or return'
            and expr->right->prec < expr->prec;

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
                case tkUnaryMinus:
                    break;
                default:
                    leftBr = true;
                }
            if (expr->right) switch (expr->right->kind) {
                case tkNumber:
                case tkIdentifier:
                case tkString:
                case tkOpColon:
                case tkMultiDotNumber:
                case tkUnaryMinus:
                    break;
                default:
                    rightBr = true;
                }
        }

        //        if (false and self->kind == tkKeyword_return and
        //        self->right) {
        //            switch (self->right->kind) {
        //            case tkString:
        //            case tkNumber:
        //            case tkIdentifier:
        //            case tkFunctionCall:
        //            case tkSubscript:
        //            case tkRawString:
        //            case tkMultiDotNumber:
        //                break;
        //            default:
        //                rightBr = true;
        //                break;
        //            }
        //        }

        if (expr->kind == tkPower and not spacing) putc('(', stdout);

        char lpo = leftBr and expr->left->kind == tkOpColon ? '[' : '(';
        char lpc = leftBr and expr->left->kind == tkOpColon ? ']' : ')';
        if (leftBr) putc(lpo, stdout);
        if (expr->left)
            ASTExpr_lint(expr->left, 0,
                spacing and !leftBr and expr->kind != tkOpColon, escapeStrings);
        if (leftBr) putc(lpc, stdout);

        printf("%s", TokenKind_repr(expr->kind, spacing));

        char rpo = rightBr and expr->right->kind == tkOpColon ? '[' : '(';
        char rpc = rightBr and expr->right->kind == tkOpColon ? ']' : ')';
        if (rightBr) putc(rpo, stdout);
        if (expr->right)
            ASTExpr_lint(expr->right, 0,
                spacing and !rightBr and expr->kind != tkOpColon,
                escapeStrings);
        if (rightBr) putc(rpc, stdout);

        if (expr->kind == tkPower and not spacing) putc(')', stdout);
        // if (expr->kind == tkArrayOpen) putc(']', stdout);
        // if (expr->kind == tkBraceOpen) putc('}', stdout);
    }
}

static void ASTModule_lint(ASTModule* module, int level) {
    printf("# module %s\n", module->name);

    foreach (ASTImport*, import, module->imports)
        ASTImport_lint(import, level);

    puts("");

    foreach (ASTType*, type, module->types)
        ASTType_lint(type, level);

    foreach (ASTFunc*, func, module->funcs)
        ASTFunc_lint(func, level);

    foreach (ASTTest*, test, module->tests)
        ASTTest_lint(test, level);
}
