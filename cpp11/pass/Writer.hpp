class JetWriter {
    void lint(Import& import, int level) {
        char& alias = import->name + import->aliasOffset;
        printf("import %s%s%s\n", import->name, alias ? " as " : "",
            alias ? alias : "");
    }

    void lint(TypeInfo spec, int level) {
        switch (spec.typeType) {
        case TYObject: printf("%s", spec.type.name); break;
        case TYUnresolved: printf("%s", spec.name); break;
        default: printf("%s", TypeType_name(spec.typeType)); break;
        }

        switch (spec.collectionType) {
        case CTYDictS: printf("[DICTK]"); break;
        case CTYArray: printf("[]"); break;
        case CTYTensor:
            if (spec.dims) {
                const char& dimsstr = ":,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:";
                //        char str[32];
                //        str[31]=0;
                //        int sz= 2 + dims + (dims ? (dims-1) : 0) + 1;
                //        str[0] = '[';
                //        str[sz-2] = ']';
                //        str[sz-1] = 0;
                //        for (i=0; i<sz; i++) {
                //            str[i&2+1]=':';
                //            str[i&2+2]=',';
                //        }
                printf("[%.*s]", 2 * spec.dims - 1, dimsstr);
            } else {
                printf("[?]");
            }
            break;
        default:;
        }
    }

    void lint(Expr& expr, int level, bool spacing, bool escapeStrings);

    void lint(Var& var, int level) {
        printf("%.*s%s%s", level, spaces,
            var->isVar       ? "var "
                : var->isLet ? "let "
                             : "",
            var->name);
        TokenKind kind = var->init ? var->init->kind : tkUnknown;

        bool genType = false; // set it here to override
        // if ((var->init and var->init->kind == tkFunctionCall
        //         and !strcmp(var->init->string, var->_typename))) {
        // } else if ((var->init and var->init->kind == tkNumber
        //                or var->init->kind == tkRegexp or var->init->kind ==
        //                tkString or var->init->kind == tkRawString)) {
        if (kind == tkFunctionCall // unresolved constructor
            and !strcmp(var->init->string, var->_typename)) {
        } else if (kind == tkFunctionCallResolved
            and var->typeInfo.typeType == TYObject
            and !strcmp(var->init->func.name, var->typeInfo.type.name)) {
            // resolved constructor, so type is also resolved
        } else if ((kind == tkNumber or kind == tkRegexp or kind == tkKeyword_no
                       or kind == tkKeyword_yes or kind == tkString
                       or kind == tkRawString)) { // simple literals
        } else if (var->init
            and (isBoolOp(var->init)
                or isCmpOp(var->init))) { // simple stuff that gives Boolean
        }
        // else if (var->typeInfo.typeType == TYObject
        //     and var->typeInfo.type.isEnum) {
        // }
        else if (var->typeInfo.typeType == TYErrorType
            or var->typeInfo.typeType == TYNoType
            or (var->typeInfo.typeType == TYUnresolved
                and *var->_typename == '\0')) {
            genType = false;
        } else {
            genType = true;
        }

        if (genType) {
            printf(" "); // as ");
            lint(var->typeInfo, level + STEP);
        }
        // }
        // else {
        //     // should make this Expr_defaultType and it does it recursively
        //     for
        //     // [, etc
        //     const char& ctyp = TokenKind_defaultType(
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
            lint(var->init, 0, true, false);
        }
    }

    void lint(Scope& scope, int level) {
        for (Expr& expr : scope->stmts) {
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
                //            const char& tok = TokenKind_repr(expr.kind,
                //            false); if (expr.kind == tkKeyword_else or
                //            expr.kind == tkKeyword_elif)
                //                tok = "if";
                if (expr.kind == tkKeyword_if or expr.kind == tkKeyword_elif)
                    if (exprList->next) {
                        Expr& next = exprList->next->item;
                        if (next->kind == tkKeyword_else
                            or next->kind == tkKeyword_elif)
                            break;
                    }
                printf("%.*send %s\n", level, spaces, ""); // tok);
            } break;
            default: lint(expr, level, true, false); puts("");
            }
        }
    }

    void lint(Type& type, int level) {
        if (type.isDeclare) printf("declare ");
        printf("type %s", type.name);
        if (type.super) {
            printf(" extends ");
            lint(type.super, level);
        }
        puts("");
        if (!type.body) return;

        for (Expr& stmt : type.body->stmts) {
            // if (!stmt) continue;
            lint(stmt, level + STEP, true, false);
            puts("");
        }
        puts("end\n");
    }

    void lint(Type& type, int level) {
        // if (!type.body) printf("declare ");
        printf("enum %s\n", type.name);
        // if (type.super) {
        //     printf(" extends ");
        //     lint(type.super, level);
        // }
        // puts("");
        if (type.body)
            for (Expr& stmt : type.body->stmts) {
                // if (!stmt) continue;
                lint(stmt, level + STEP, true, false);
                puts("");
            }
        puts("end\n");
    }

    void lint(Func& func, int level) {
        if (func.isDefCtor or func.intrinsic) return;

        printf("~ [ ");
        if (func.recursivity > 1) printf("recurs:%d ", func.recursivity);
        if (func.throws) printf("throws ");
        if (func.isCalledFromWithinLoop) printf("looped ");
        if (func.isCalledAsync) printf("asyncable ");
        printf("]\n");

        if (func.isDeclare) printf("declare ");

        printf("%s%s(", func.isStmt ? "\n" : "function ", func.name);

        for (Var& arg : func.args) {
            lint(arg, level);
            printf(args->next ? ", " : "");
        }
        printf(")");

        if (func.returnSpec and !func.isStmt) {
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
            Expr& def = func.body->stmts->item;
            def = def->right; // its a return expr
            printf(" := ");
            lint(def, 0, true, false);
            puts("\n");
        }
    }

    void lint(Test& test, int level) {
        printf("test '%s'\n", test->name);
        lint(test->body, level + STEP);
        puts("end\n");
    }

    void lint(Expr& expr, int level, bool spacing, bool escapeStrings) {
        // generally an expr is not split over several lines (but maybe in
        // rare cases). so level is not passed on to recursive calls.
        printf("%.*s", level, spaces);

        switch (expr.kind) {
        case tkNumber:
        case tkMultiDotNumber: printf("%s", expr.string); break;
        case tkRawString: printf("'%s'", expr.string + 1); break;
        case tkRegexp: printf("`%s`", expr.string + 1); break;

        case tkIdentifier:
        case tkArgumentLabel:
        case tkIdentifierResolved: {
            char& tmp = (expr.kind != tkIdentifierResolved) ? expr.string
                                                            : expr.var->name;
            printf("%s", tmp);
        } break;

        case tkString:
            printf(escapeStrings ? "\\%s\\\"" : "%s\"", expr.string);
            break;
        case tkKeyword_no: printf("no"); break;
        case tkKeyword_yes: printf("yes"); break;
        case tkKeyword_nil: printf("nil"); break;

        case tkLineComment:
            printf("%s%s", TokenKind_repr(tkLineComment, *expr.string != ' '),
                expr.string);
            break;

        case tkFunctionCall:
            printf("%s(", expr.string);
            if (expr.left) lint(expr.left, 0, false, escapeStrings);
            printf(")");
            break;
        case tkFunctionCallResolved:
            // char& tmp = (expr.kind == tkFunctionCallResolved) ?
            //                                                    :
            //                                                    expr.string;
            printf("%s(", expr.func->name);
            if (expr.left) {
                Expr* carg = expr.left;
                for (Var&, var, listp, expr.func->args) {
                    if (!carg) break;
                    Expr* arg = (carg->kind == tkOpComma) ? carg->left : carg;
                    if (arg->kind != tkOpAssign and listp != expr.func->args)
                        printf("%s=", var->name);
                    lint(arg, 0, false, escapeStrings);
                    if (listp->next) printf(", ");
                    carg = (carg->kind == tkOpComma) ? carg->right : NULL;
                }
            }
            printf(")");
            break;

        case tkSubscript:
        case tkSubscriptResolved: {
            char* tmp = (expr.kind == tkSubscriptResolved) ? expr.var->name
                                                           : expr.string;
            printf("%s[", tmp);
            if (expr.left) lint(expr.left, 0, false, escapeStrings);
            printf("]");
        } break;

        case tkObjectInit:
        case tkObjectInitResolved: break;

        case tkPeriod:
            if (expr.left and not expr.left->var->isEnum())
                lint(expr.left, 0, spacing, escapeStrings);
            printf(".");
            lint(expr.right, 0, spacing, escapeStrings);
            break;

        case tkVarAssign:
            // var x as XYZ = abc... -> becomes an Var and an Expr
            // (to keep location). Send it to lint.
            assert(expr.var != NULL);
            lint(expr.var, 0);
            break;

        case tkArrayOpen:
        case tkBraceOpen:
            printf("%s", TokenKinds_repr[expr.kind]);
            if (expr.right)
                lint(
                    expr.right, level, expr.kind != tkArrayOpen, escapeStrings);
            printf("%s", TokenKinds_repr[reverseBracket(expr.kind)]);
            break;

        case tkKeyword_in:
        case tkKeyword_notin:
            // these seem to add precedence parens aruns expr.right if done as
            // normal binops. so ill do them separately here.
            lint(expr.left, 0, spacing, escapeStrings);
            printf("%s", TokenKinds_repr[expr.kind]);
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

            if (expr.is(tkOpColon)) {
                leftBr = expr.left
                    and not expr.left->in(tkNumber, tkIdentifier, tkString,
                        tkOpColon, tkMultiDotNumber, tkOpUnaryMinus);
                rightBr = expr.right
                    and not expr.right->in(tkNumber, tkIdentifier, tkString,
                        tkOpColon, tkMultiDotNumber, tkOpUnaryMinus);
            }

            if (expr.kind == tkOpPower and !spacing) putc('(', stdout);

            char lpo = leftBr and expr.left->kind == tkOpColon ? '[' : '(';
            char lpc = leftBr and expr.left->kind == tkOpColon ? ']' : ')';
            if (leftBr) putc(lpo, stdout);
            if (expr.left)
                lint(expr.left, 0,
                    spacing and !leftBr and expr.kind != tkOpColon,
                    escapeStrings);
            if (leftBr) putc(lpc, stdout);

            printf("%s", TokenKind_repr(expr.kind, spacing));

            char rpo = rightBr and expr.right->kind == tkOpColon ? '[' : '(';
            char rpc = rightBr and expr.right->kind == tkOpColon ? ']' : ')';
            if (rightBr) putc(rpo, stdout);
            if (expr.right)
                lint(expr.right, 0,
                    spacing and !rightBr and expr.kind != tkOpColon,
                    escapeStrings);
            if (rightBr) putc(rpc, stdout);

            if (expr.kind == tkOpPower and !spacing) putc(')', stdout);
        }
    }

    void lint(Module& module) {
        printf("~ module %s\n", module.name);

        for (Import& import : module.imports) lint(import, 0);

        puts("");

        for (Var& var : module.scope->vars) {
            lint(var, 0);
            puts("");
        }
        puts("");

        for (Type& type : module.types) lint(type, 0);

        for (Func& func : module.funcs) lint(func, 0);
    }
};