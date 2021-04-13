
static void lint(ASTImport& import, int level) {
    char* alias = import.name + import.aliasOffset;
    printf("import %s%s%s\n", import.name, alias ? " as " : "",
        alias ? alias : "");
}

static void lint(ASTTypeSpec& spec, int level) {
    switch (spec.typeType) {
    case TYObject:
        printf("%s", spec.type.name);
        break;
    case TYUnresolved:
        printf("%s", spec.name);
        break;
    default:
        printf("%s", TypeType_name(spec.typeType));
        break;
    }

    switch (spec.collectionType) {
    case CTYDictS:
        printf("[DICTK]");
        break;
    case CTYArray:
        printf("[]");
        break;
    case CTYTensor:
        if (spec.dims) {
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
            printf("[%.*s]", 2 * spec.dims - 1, dimsstr);
        } else {
            printf("[?]");
        }
        break;
    default:;
    }
}

static void lint(ASTExpr& expr, int level, bool spacing, bool escapeStrings);

static void lint(ASTVar& var, int level) {
    printf("%.*s%s%s", level, spaces,
        var.isVar       ? "var "
            : var.isLet ? "let "
                        : "",
        var.name);
    TokenKind kind = var.init ? var.init->kind : tkUnknown;

    bool genType = false; // set it here to override
    // if ((var.init and var.init->kind == tkFunctionCall
    //         and !strcmp(var.init->string, var.typeSpec.name))) {
    // } else if ((var.init and var.init->kind == tkNumber
    //                or var.init->kind == tkRegexp or var.init->kind ==
    //                tkString or var.init->kind == tkRawString)) {
    if (kind == tkFunctionCall // unresolved constructor
        and !strcmp(var.init->string, var.typeSpec->name)) {
    } else if (kind == tkFunctionCallResolved
        and var.typeSpec.typeType == TYObject
        and !strcmp(var.init->func.name, var.typeSpec.type.name)) {
        // resolved constructor, so type is also resolved
    } else if ((kind == tkNumber || kind == tkRegexp || kind == tkKeyword_no
                   || kind == tkKeyword_yes || kind == tkString
                   || kind == tkRawString)) { // simple literals
    } else if (var.init
        and (isBoolOp(var.init)
            || isCmpOp(var.init))) { // simple stuff that gives Boolean
    }
    // else if (var.typeSpec.typeType == TYObject
    //     and var.typeSpec.type.isEnum) {
    // }
    else if (var.typeSpec.typeType == TYErrorType
        || var.typeSpec.typeType == TYNoType
        || (var.typeSpec.typeType == TYUnresolved
            and *var.typeSpec.name == '\0')) {
        genType = false;
    } else {
        genType = true;
    }

    if (genType) {
        printf(" "); // as ");
        lint(var.typeSpec, level + STEP);
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
    if (var.init) {
        printf(" = ");
        lint(var.init, 0, true, false);
    }
}

static void lint(ASTScope& scope, int level) {
    for (ASTExpr& expr : scope.stmts) {
        switch (expr.kind) {
        case tkKeyword_case:
        case tkKeyword_match:
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr(expr.kind, false));
            if (expr.left) lint(expr.left, 0, true, false);
            puts("");
            //, true, escapeStrings);
            if (expr.kind == tkKeyword_match) {
                if (expr.body) lint(expr.body, level);
                printf("%.*send %s\n", level, spaces, ""); // "match");
            } else {
                if (expr.body) lint(expr.body, level + STEP);
            }
            break;

        case tkKeyword_for:
        case tkKeyword_if:
        case tkKeyword_elif:
        case tkKeyword_else:
        case tkKeyword_while: {
            printf("%.*s", level, spaces);
            printf("%s ", TokenKind_repr(expr.kind, false));
            if (expr.left) lint(expr.left, 0, true, false);
            puts("");
            if (expr.body)
                lint(expr.body, level + STEP); //, true, escapeStrings);
            //            const char* tok = TokenKind_repr(expr.kind, false);
            //            if (expr.kind == tkKeyword_else || expr.kind ==
            //            tkKeyword_elif)
            //                tok = "if";
            if (expr.kind == tkKeyword_if || expr.kind == tkKeyword_elif)
                if (exprList->next) {
                    ASTExpr* next = exprList->next->item;
                    if (next->kind == tkKeyword_else
                        || next->kind == tkKeyword_elif)
                        break;
                }
            printf("%.*send %s\n", level, spaces, ""); // tok);
        } break;
        default:
            lint(expr, level, true, false);
            puts("");
        }
    }
}

static void lint(ASTType& type, int level) {
    if (type.isDeclare) printf("declare ");
    printf("type %s", type.name);
    if (type.super) {
        printf(" extends ");
        lint(type.super, level);
    }
    puts("");
    if (!type.body) return;

    for (ASTExpr& stmt : type.body->stmts) {
        // if (!stmt) continue;
        lint(stmt, level + STEP, true, false);
        puts("");
    }
    puts("end\n");
}

static void lint(ASTType& type, int level) {
    // if (!type.body) printf("declare ");
    printf("enum %s\n", type.name);
    // if (type.super) {
    //     printf(" extends ");
    //     lint(type.super, level);
    // }
    // puts("");
    if (type.body)
        for (ASTExpr& stmt : type.body->stmts) {
            lint(stmt, level + STEP, true, false);
            puts("");
        }
    puts("end\n");
}

static void lint(ASTFunc& func, int level) {
    if (func.isDefCtor || func.intrinsic) return;
    if (func.isDeclare) printf("declare ");

    printf("%s%s(", func.isStmt ? "\n" : "function ", func.name);

    for (ASTVar* arg : func.args) {
        lint(arg, level);
        printf(args->next ? ", " : "");
    }
    printf(")");

    if (func.returnSpec and not func.isStmt) {
        printf(" as ");
        lint(func.returnSpec, level);
    }
    if (func.isDeclare) {
        puts("");
        return;
    } else if (!func.isStmt) {
        puts("");
        lint(func.body, level + STEP);
        puts("end\n");
    } else {
        ASTExpr* def = func.body->stmts->item;
        def = def->right; // its a return expr
        printf(" := ");
        lint(def, 0, true, false);
        puts("\n");
    }
}

static void lint(ASTTest& test, int level) {
    printf("test '%s'\n", test.name);
    lint(test.body, level + STEP);
    puts("end\n");
}

static void lint(ASTExpr& expr, int level, bool spacing, bool escapeStrings) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.
    indentf(level); // printf("%.*s", level, spaces);

    switch (expr.kind) {
    case tkNumber:
    case tkMultiDotNumber:
        printf("%s", expr.string);
        break;
    case tkRawString:
        printf("'%s'", expr.string + 1);
        break;
    case tkRegexp:
        printf("`%s`", expr.string + 1);
        break;

    case tkIdentifier:
    case tkArgumentLabel:
    case tkIdentifierResolved: {
        char* tmp
            = (expr.kind != tkIdentifierResolved) ? expr.string : expr.var.name;
        printf("%s", tmp);
    } break;

    case tkString:
        printf(escapeStrings ? "\\%s\\\"" : "%s\"", expr.string);
        break;
    case tkKeyword_no:
        printf("no");
        break;
    case tkKeyword_yes:
        printf("yes");
        break;
    case tkKeyword_nil:
        printf("nil");
        break;

    case tkLineComment:
        printf("%s%s", TokenKind_repr(tkLineComment, *expr.string != ' '),
            expr.string);
        break;

    case tkFunctionCall:
    case tkFunctionCallResolved: {
        char* tmp = (expr.kind == tkFunctionCallResolved) ? expr.func.name
                                                          : expr.string;
        printf("%s(", tmp);
        if (expr.left) lint(expr.left, 0, false, escapeStrings);
        printf(")");
    } break;

    case tkSubscript:
    case tkSubscriptResolved: {
        char* tmp
            = (expr.kind == tkSubscriptResolved) ? expr.var.name : expr.string;
        printf("%s[", tmp);
        if (expr.left) lint(expr.left, 0, false, escapeStrings);
        printf("]");
    } break;

    case tkObjectInit:
    case tkObjectInitResolved:
        break;

    case tkPeriod:
        if (expr.left and expr.left->typeType == TYObject
            and !expr.left->var.typeSpec.type.isEnum)
            lint(expr.left, 0, spacing, escapeStrings);
        printf(".");
        lint(expr.right, 0, spacing, escapeStrings);
        break;

    case tkVarAssign:
        // var x as XYZ = abc... -> becomes an ASTVar and an ASTExpr
        // (to keep location). Send it to lint.
        assert(expr.var != NULL);
        lint(expr.var, 0);
        break;

    case tkArrayOpen:
    case tkBraceOpen:
        printf("%s", tkrepr[expr.kind]);
        if (expr.right)
            lint(expr.right, level, expr.kind != tkArrayOpen, escapeStrings);
        printf("%s", tkrepr[TokenKind_reverseBracket(expr.kind)]);
        break;

    case tkKeyword_in:
    case tkKeyword_notin:
        // these seem to add precedence parens aruns expr.right if done as
        // normal binops. so ill do them separately here.
        lint(expr.left, 0, spacing, escapeStrings);
        printf("%s", tkrepr[expr.kind]);
        lint(expr.right, 0, spacing, escapeStrings);
        break;

    default:
        if (!expr.prec) break;
        // not an operator, but this should be error if you reach here
        bool leftBr
            = expr.left and expr.left->prec and expr.left->prec < expr.prec;
        bool rightBr = expr.right and expr.right->prec
            and expr.right->kind != tkKeyword_return // found in 'or return'
            and expr.right->prec < expr.prec;

        if (expr.kind == tkOpColon) {
            // expressions like arr[a:x-3:2] should become
            // arr[a:(x-3):2]
            // or list literals [8, 9, 6, 77, sin(c)]
            if (expr.left) switch (expr.left->kind) {
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
            if (expr.right) switch (expr.right->kind) {
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

        if (expr.kind == tkPower and !spacing) putc('(', stdout);

        char lpo = leftBr and expr.left->kind == tkOpColon ? '[' : '(';
        char lpc = leftBr and expr.left->kind == tkOpColon ? ']' : ')';
        if (leftBr) putc(lpo, stdout);
        if (expr.left)
            lint(expr.left, 0, spacing and !leftBr and expr.kind != tkOpColon,
                escapeStrings);
        if (leftBr) putc(lpc, stdout);

        printf("%s", TokenKind_repr(expr.kind, spacing));

        char rpo = rightBr and expr.right->kind == tkOpColon ? '[' : '(';
        char rpc = rightBr and expr.right->kind == tkOpColon ? ']' : ')';
        if (rightBr) putc(rpo, stdout);
        if (expr.right)
            lint(expr.right, 0, spacing and !rightBr and expr.kind != tkOpColon,
                escapeStrings);
        if (rightBr) putc(rpc, stdout);

        if (expr.kind == tkPower and !spacing) putc(')', stdout);
        // if (expr.kind == tkArrayOpen) putc(']', stdout);
        // if (expr.kind == tkBraceOpen) putc('}', stdout);
    }
}

static void lint(ASTModule& module) {
    // printf("~ module %s\n", module.name);

    for (ASTImport& import : module.imports) lint(import, 0);

    puts("");

    for (ASTVar& var : module.scope->locals) {
        lint(var, 0);
        puts("");
    } // for (ASTVar*, var, mod->scope->locals)
    //     if (var.init)
    //         analyseExpr(parser, var.init, mod->scope, mod, false);
    puts("");

    for (ASTType& type : module.types) lint(type, 0);
    for (ASTType& enm : module.enums) lint(enm, 0);
    for (ASTFunc& func : module.funcs) lint(func, 0);
    for (ASTTest& test : module.tests) lint(test, 0);
}
