
static void format_import(ast_import_t* import, int level) {
    char* alias = import->name + import->aliasOffset;
    printf("import %s%s%s\n", import->name, alias ? " as " : "",
        alias ? alias : "");
}

static void ast_typespec_t_format(ast_typespec_t* spec, int level) {
    switch (spec->typeType) {
    case ty_object: printf("%s", spec->type->name); break;
    case ty_unresolved: printf("%s", spec->name); break;
    default: printf("%s", typetype_e_name(spec->typeType)); break;
    }

    switch (spec->collectionType) {
    case cty_dictS: printf("[DICTK]"); break;
    case cty_array: printf("[]"); break;
    case cty_tensor:
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

static void format_expr(
    ast_expr_t* expr, int level, bool spacing, bool escapeStrings);

static void format_var(ast_var_t* var, int level) {
    printf("%.*s%s%s", level, spaces,
        var->isVar       ? "var "
            : var->isLet ? "let "
                         : "",
        var->name);
    tokenkind_e kind = var->init ? var->init->kind : tk_unknown;

    bool genType = false; // set it here to override
    // if ((var->init and var->init->kind == tk_functionCall
    //         and !strcmp(var->init->string, var->typespec->name))) {
    // } else if ((var->init and var->init->kind == tk_number
    //                or var->init->kind == tk_regexp or var->init->kind ==
    //                tk_string or var->init->kind == tk_rawString)) {
    if (kind == tk_functionCall // unresolved constructor
        && !strcmp(var->init->string, var->typespec->name)) {
    } else if (kind == tk_functionCallResolved
        && var->typespec->typeType == ty_object
        && !strcmp(var->init->func->name, var->typespec->type->name)) {
        // resolved constructor, so type is also resolved
    } else if ((kind == tk_number || kind == tk_regexp || kind == tk_keyword_no
                   || kind == tk_keyword_yes || kind == tk_string
                   || kind == tk_rawString)) { // simple literals
    } else if (var->init
        && (isBoolOp(var->init)
            || isCmpOp(var->init))) { // simple stuff that gives Boolean
    }
    // else if (var->typespec->typeType == ty_object
    //     && var->typespec->type->isEnum) {
    // }
    else if (var->typespec->typeType == ty_errorType
        || var->typespec->typeType == ty_noType
        || (var->typespec->typeType == ty_unresolved
            && *var->typespec->name == '\0')) {
        genType = false;
    } else {
        genType = true;
    }

    if (genType) {
        printf(" "); // as ");
        ast_typespec_t_format(var->typespec, level + STEP);
    }
    // }
    // else {
    //     // should make this Expr_defaultType and it does it recursively
    //     for
    //     // [, etc
    //     const char* ctyp = tokenkind_e_defaultType(
    //         self->init ? self->init->kind : tk_unknown);
    //     if (self->init and self->init->kind == tk_listLiteral)
    //         ctyp = tokenkind_e_defaultType(
    //             self->init->right ? self->init->right->kind : tk_unknown);
    //     if (self->init and self->init->kind == tk_functionCall
    //         and *self->init->name >= 'A' and *self->init->name <= 'Z')
    //         ctyp = NULL;
    //     if (ctyp) printf(" as %s", ctyp);
    // }
    if (var->init) {
        printf(" = ");
        format_expr(var->init, 0, true, false);
    }
}

static void format_scope(ast_scope_t* scope, int level) {
    foreachn(ast_expr_t*, expr, exprList, scope->stmts) {
        switch (expr->kind) {
        case tk_keyword_case:
        case tk_keyword_match:
            printf("%.*s", level, spaces);
            printf("%s ", tokenkind_e_repr[expr->kind]);
            if (expr->left) format_expr(expr->left, 0, true, false);
            puts("");
            //, true, escapeStrings);
            if (expr->kind == tk_keyword_match) {
                if (expr->body) format_scope(expr->body, level);
                printf("%.*send %s\n", level, spaces, ""); // "match");
            } else {
                if (expr->body) format_scope(expr->body, level + STEP);
            }
            break;

        case tk_keyword_for:
        case tk_keyword_if:
        case tk_keyword_elif:
        case tk_keyword_else:
        case tk_keyword_while: {
            printf("%.*s", level, spaces);
            printf("%s ", tokenkind_e_repr[expr->kind]);
            if (expr->left) format_expr(expr->left, 0, true, false);
            puts("");
            if (expr->body)
                format_scope(
                    expr->body, level + STEP); //, true, escapeStrings);
            //            const char* tok = tokenkind_e_repr[expr->kind];
            //            if (expr->kind == tk_keyword_else || expr->kind ==
            //            tk_keyword_elif)
            //                tok = "if";
            if (expr->kind == tk_keyword_if || expr->kind == tk_keyword_elif)
                if (exprList->next) {
                    ast_expr_t* next = exprList->next->item;
                    if (next->kind == tk_keyword_else
                        || next->kind == tk_keyword_elif)
                        break;
                }
            printf("%.*send %s\n", level, spaces, ""); // tok);
        } break;
        default: format_expr(expr, level, true, false); puts("");
        }
    }
}

static void format_type(ast_type_t* type, int level) {
    if (type->isDeclare) printf("declare ");
    printf("type %s", type->name);
    if (type->super) {
        printf(" extends ");
        ast_typespec_t_format(type->super, level);
    }
    puts("");
    if (!type->body) return;

    foreach (ast_expr_t*, stmt, type->body->stmts) {
        if (!stmt) continue;
        format_expr(stmt, level + STEP, true, false);
        puts("");
    }
    puts("end\n");
}

static void format_enum(ast_type_t* type, int level) {
    // if (!type->body) printf("declare ");
    printf("enum %s\n", type->name);
    // if (type->super) {
    //     printf(" extends ");
    //     ast_typespec_t_format(type->super, level);
    // }
    // puts("");
    if (type->body) foreach (ast_expr_t*, stmt, type->body->stmts) {
            if (!stmt) continue;
            format_expr(stmt, level + STEP, true, false);
            puts("");
        }
    puts("end\n");
}

static void format_func(ast_func_t* func, int level) {
    if (func->isDefCtor || func->intrinsic) return;

    printf("~ [ ");
    if (func->recursivity > 1) printf("recurs:%d ", func->recursivity);
    if (func->throws) printf("throws ");
    if (func->isCalledFromWithinLoop) printf("looped ");
    if (func->isCalledAsync) printf("asyncable ");
    printf("]\n");

    if (func->isDeclare) printf("declare ");

    printf("%s%s(", func->isStmt ? "\n" : "function ", func->name);

    foreachn(ast_var_t*, arg, args, func->args) {
        format_var(arg, level);
        printf(args->next ? ", " : "");
    }
    printf(")");

    if (func->returnSpec && !func->isStmt) {
        printf(" as ");
        ast_typespec_t_format(func->returnSpec, level);
    }
    if (func->isDeclare) {
        puts("");
        return;
    } else if (!func->isStmt) {
        puts("");
        format_scope(func->body, level + STEP);
        puts("end\n");
    } else {
        ast_expr_t* def = func->body->stmts->item;
        def = def->right; // its a return expr
        printf(" := ");
        format_expr(def, 0, true, false);
        puts("\n");
    }
}

static void format_test(ast_test_t* test, int level) {
    printf("test '%s'\n", test->name);
    format_scope(test->body, level + STEP);
    puts("end\n");
}

static void format_expr(
    ast_expr_t* expr, int level, bool spacing, bool escapeStrings) {
    // generally an expr is not split over several lines (but maybe in
    // rare cases). so level is not passed on to recursive calls.
    printf("%.*s", level, spaces);

    switch (expr->kind) {
    case tk_number:
    case tk_multiDotNumber: printf("%s", expr->string); break;
    case tk_rawString: printf("'%s'", expr->string + 1); break;
    case tk_regexp: printf("`%s`", expr->string + 1); break;

    case tk_identifier:
    case tk_argumentLabel:
    case tk_identifierResolved: {
        char* tmp = (expr->kind != tk_identifierResolved) ? expr->string
                                                          : expr->var->name;
        printf("%s", tmp);
    } break;

    case tk_string:
        printf(escapeStrings ? "\\%s\\\"" : "%s\"", expr->string);
        break;
    case tk_keyword_no: printf("no"); break;
    case tk_keyword_yes: printf("yes"); break;
    case tk_keyword_nil: printf("nil"); break;

    case tk_lineComment:
        printf("%s%s", tokenkind_e_repr[tk_lineComment], expr->string);
        break;

    case tk_functionCall:
        printf("%s(", expr->string);
        if (expr->left) format_expr(expr->left, 0, false, escapeStrings);
        printf(")");
        break;
    case tk_functionCallResolved:
        // char* tmp = (expr->kind == tk_functionCallResolved) ?
        //                                                    : expr->string;
        printf("%s(", expr->func->name);
        if (expr->left) {
            ast_expr_t* carg = expr->left;
            foreachn(ast_var_t*, var, listp, expr->func->args) {
                if (!carg) break;
                ast_expr_t* arg
                    = (carg->kind == tk_opComma) ? carg->left : carg;
                if (arg->kind != tk_opAssign && listp != expr->func->args)
                    printf("%s=", var->name);
                format_expr(arg, 0, false, escapeStrings);
                if (listp->next) printf(", ");
                carg = (carg->kind == tk_opComma) ? carg->right : NULL;
            }
        }
        printf(")");
        break;

    case tk_subscript:
    case tk_subscriptResolved: {
        char* tmp = (expr->kind == tk_subscriptResolved) ? expr->var->name
                                                         : expr->string;
        printf("%s[", tmp);
        if (expr->left) format_expr(expr->left, 0, false, escapeStrings);
        printf("]");
    } break;

    case tk_objectInit:
    case tk_objectInitResolved: break;

    case tk_period:
        if (expr->left && expr->left->typeType == ty_object
            && !expr->left->var->typespec->type->isEnum)
            format_expr(expr->left, 0, spacing, escapeStrings);
        printf(".");
        format_expr(expr->right, 0, spacing, escapeStrings);
        break;

    case tk_varAssign:
        // var x as XYZ = abc... -> becomes an ast_var_t and an ast_expr_t
        // (to keep location). Send it to format_var.
        assert(expr->var != NULL);
        format_var(expr->var, 0);
        break;

    case tk_arrayOpen:
    case tk_braceOpen:
        printf("%s", tokenkind_e_repr[expr->kind]);
        if (expr->right)
            format_expr(
                expr->right, level, expr->kind != tk_arrayOpen, escapeStrings);
        printf("%s", tokenkind_e_repr[tokenkind_e_reverseBracket(expr->kind)]);
        break;

    case tk_keyword_in:
    case tk_keyword_notin:
        // these seem to add precedence parens aruns expr->right if done as
        // normal binops. so ill do them separately here.
        format_expr(expr->left, 0, spacing, escapeStrings);
        printf("%s", tokenkind_e_repr[expr->kind]);
        format_expr(expr->right, 0, spacing, escapeStrings);
        break;

    default:
        if (!expr->prec) break;
        // not an operator, but this should be error if you reach here
        bool leftBr
            = expr->left && expr->left->prec && expr->left->prec < expr->prec;
        bool rightBr = expr->right && expr->right->prec
            && expr->right->kind != tk_keyword_return // found in 'or return'
            && expr->right->prec < expr->prec;

        if (expr->kind == tk_opColon) {
            // expressions like arr[a:x-3:2] should become
            // arr[a:(x-3):2]
            // or list literals [8, 9, 6, 77, sin(c)]
            if (expr->left) switch (expr->left->kind) {
                case tk_number:
                case tk_identifier:
                case tk_string:
                case tk_opColon:
                case tk_multiDotNumber:
                case tk_opUnaryMinus: break;
                default: leftBr = true;
                }
            if (expr->right) switch (expr->right->kind) {
                case tk_number:
                case tk_identifier:
                case tk_string:
                case tk_opColon:
                case tk_multiDotNumber:
                case tk_opUnaryMinus: break;
                default: rightBr = true;
                }
        }

        if (expr->kind == tk_opPower && !spacing) putc('(', stdout);

        char lpo = leftBr && expr->left->kind == tk_opColon ? '[' : '(';
        char lpc = leftBr && expr->left->kind == tk_opColon ? ']' : ')';
        if (leftBr) putc(lpo, stdout);
        if (expr->left)
            format_expr(expr->left, 0,
                spacing && !leftBr && expr->kind != tk_opColon, escapeStrings);
        if (leftBr) putc(lpc, stdout);

        printf("%s",
            spacing ? tokenkind_e_srepr[expr->kind]
                    : tokenkind_e_repr[expr->kind]);

        char rpo = rightBr && expr->right->kind == tk_opColon ? '[' : '(';
        char rpc = rightBr && expr->right->kind == tk_opColon ? ']' : ')';
        if (rightBr) putc(rpo, stdout);
        if (expr->right)
            format_expr(expr->right, 0,
                spacing && !rightBr && expr->kind != tk_opColon, escapeStrings);
        if (rightBr) putc(rpc, stdout);

        if (expr->kind == tk_opPower && !spacing) putc(')', stdout);
    }
}

static void format_module(ast_module_t* module) {
    printf("~ module %s\n", module->name);

    foreach (ast_import_t*, import, module->imports)
        format_import(import, 0);

    puts("");

    foreach (ast_var_t*, var, module->scope->locals)
        format_var(var, 0), puts("");

    puts("");

    foreach (ast_type_t*, type, module->types)
        format_type(type, 0);

    foreach (ast_type_t*, en, module->enums)
        format_enum(en, 0);

    foreach (ast_func_t*, func, module->funcs)
        format_func(func, 0);

    foreach (ast_test_t*, test, module->tests)
        format_test(test, 0);
}
