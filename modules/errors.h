#define RELF(s) (*s == '/' ? "" : "./"), s

#define fatal(str, ...)                                                        \
    {                                                                          \
        eprintf(str, __VA_ARGS__);                                             \
        exit(1);                                                               \
    }

static void Parser_errorIncrement(Parser* parser) {
    if (++parser->issues.errCount < parser->issues.errLimit) return;
    if (parser->mode == PMLint) {
        fatal(
            "\n*** too many errors (%d), quitting\n", parser->issues.errLimit);
    } else {
        fatal("\n*** %s has errors, please lint it first.\n", parser->filename);
    }
}
static const char* const carets
    = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
      "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
#define _PRLINE(line)                                                          \
    if (line > 0 && line <= parser->orig.used)                                 \
        eprintf("\e[0;2m%4d | %s\e[0m\n", line + offs,                         \
            parser->orig.ref[line + offs - 1]);
void _PRREDLINE(
    Parser* parser, int line, int col, int len, int offs, char* msg) {
    char* c = parser->orig.ref[line + offs - 1];
    if (!col) len = strlen(c), col = 1;
    eprintf("\e[31;1m%4d |\e[0m %.*s", line + offs, col - 1, c);
    eprintf("\e[31m%.*s\e[0m", len, c + col - 1);
    eprintf("%s\n", c + col + len - 1);
    eprintf(
        "\e[31;1m       %.*s%.*s %s\e[0m\n", col - 1, spaces, len, carets, msg);
}
void Parser__printSourceLinesWithOffset(
    Parser* parser, int line, int col, int len, int offs, char* msg) {
    eputs("\n"); //-----+\n");
    _PRLINE(line - 2)
    _PRLINE(line - 1)
    _PRREDLINE(parser, line, col, len, offs, msg);
    _PRLINE(line + 1)
    _PRLINE(line + 2)

    eputs("\n"); //-----+\n");
}
void Parser__printSourceLines(
    Parser* parser, int line, int col, int len, char* msg) {
    Parser__printSourceLinesWithOffset(parser, line, col, len, 0, msg);
}

static void Parser_errorExpectedToken(Parser* parser, TokenKind expected) {
    eprintf("\n------ \e[31m#%d\e[0m ---------------------------" //
            "------------------------------------\n" //
            "\e[31;1merror:\e[0m expected \e[34m'%s'\e[0m (%s) "
            "but found \e[34m'%s'\e[0m\n" //
            "where: %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        TokenKind_repr(expected, false), //
        TokenKind_str[expected] + 2, //
        TokenKind_repr(parser->token.kind, false), //
        RELF(parser->filename), //
        parser->token.line, //
        parser->token.col //
    );

    char msg[128];
    msg[127] = 0;
    snprintf(msg, 127, "expected '%s' here", TokenKind_repr(expected, false));

    Parser__printSourceLines(parser, parser->token.line, parser->token.col,
        parser->token.matchlen, msg);

    // when you have an unexpected token on one line, the rest are also
    // going to be unexpected. so skip to the next newline.
    while (!Parser_matches(parser, tkNewline)
        && !Parser_matches(parser, tkNullChar))
        Token_advance(&parser->token);

    // eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n"
    //         "      expected '%s' (%s) but found '%s'\n",
    //     parser->issues.errCount + 1, //
    //     RELF(parser->filename), //
    //     parser->token.line,
    //     parser->token.col, //
    //     TokenKind_repr(expected, false), //
    //     TokenKind_str[expected] + 2, //
    //     TokenKind_repr(parser->token.kind, false));

    Parser_errorIncrement(parser);
}

bool isKeywordKind(TokenKind kind) {
    switch (kind) {
    case tkKeyword_cheater:
    case tkKeyword_for:
    case tkKeyword_while:
    case tkKeyword_if:
    case tkKeyword_end:
    case tkKeyword_enum:
    case tkKeyword_match:
    case tkKeyword_case:
    case tkKeyword_function:
    case tkKeyword_declare:
    case tkKeyword_test:
    case tkKeyword_check:
    case tkKeyword_not:
    case tkKeyword_notin:
    case tkKeyword_and:
    case tkKeyword_yes:
    case tkKeyword_no:
    case tkKeyword_nil:
    case tkKeyword_or:
    case tkKeyword_in:
    case tkKeyword_do:
    case tkKeyword_then:
    case tkKeyword_as:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_type:
    case tkKeyword_return:
    case tkKeyword_result:
    case tkKeyword_extends:
    case tkKeyword_var:
    case tkKeyword_let:
    case tkKeyword_import:
        return true;
    default:;
    }
    return false;
}

static void Parser_errorParsingExpr(Parser* parser, ASTExpr* expr) {
    eprintf("\n------ \e[31m#%d\e[0m ---------------------------" //
            "------------------------------------\n" //
            "\e[31merror:\e[0m invalid syntax\n" //
            "where: %s%s:%d:%d\n",

        // eprintf("\n(%d) \e[31merror:\e[0m syntax error at %s%s:%d/%d\n"
        //         "    probably around %d:%d\n",
        parser->issues.errCount + 1, RELF(parser->filename),
        expr->line ? expr->line : parser->token.line - 1, expr->col);
    int len;
    switch (expr->kind) {
    case tkKeyword_cheater:
    case tkKeyword_for:
    case tkKeyword_while:
    case tkKeyword_if:
    case tkKeyword_end:
    case tkKeyword_enum:
    case tkKeyword_match:
    case tkKeyword_case:
    case tkKeyword_function:
    case tkKeyword_declare:
    case tkKeyword_test:
    case tkKeyword_check:
    case tkKeyword_not:
    case tkKeyword_notin:
    case tkKeyword_and:
    case tkKeyword_yes:
    case tkKeyword_no:
    case tkKeyword_nil:
    case tkKeyword_or:
    case tkKeyword_in:
    case tkKeyword_do:
    case tkKeyword_then:
    case tkKeyword_as:
    case tkKeyword_else:
    case tkKeyword_elif:
    case tkKeyword_type:
    case tkKeyword_return:
    case tkKeyword_result:
    case tkKeyword_extends:
    case tkKeyword_var:
    case tkKeyword_let:
    case tkKeyword_import:
    case tkIdentifier:
    case tkArgumentLabel:
    case tkFunctionCall:
    case tkSubscript:
    case tkObjectInit:
    case tkNumber:
        len = strlen(expr->string);
        break;
    default:
        len = 1;
    }

    Parser__printSourceLines(parser,
        expr->line ? expr->line : parser->token.line - 1, expr->col, len,
        "syntax error");

    if (isKeywordKind(expr->kind))
        eputs("\e[36;1minfo:\e[0m keywords cannot be used as identifiers\n");

    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidIdent(Parser* parser) {
    eprintf("\n(%d) \e[31merror:\e[0m invalid name '%.*s' at "
            "%s%s:%d:%d\n", //
        parser->issues.errCount + 1, //
        parser->token.matchlen, //
        parser->token.pos, //
        RELF(parser->filename), //
        parser->token.line, parser->token.col);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTypeMember(Parser* parser) {
    eprintf("\n(%d) \e[31merror:\e[0m invalid member at %s%s:%d\n", //
        parser->issues.errCount + 1, //
        RELF(parser->filename), //
        parser->token.line - 1);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedVar(Parser* parser, ASTExpr* expr) {
    eprintf(
        "\n------ \e[31m#%d\e[0m ---------------------------" //
        "------------------------------------\n" //
        "\e[31merror:\e[0m variable \e[34m'%.*s'\e[0m not found in scope\n" //
        "where: %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        64, expr->string,
        RELF(parser->filename), //
        expr->line, //
        expr->col //
    );

    Parser__printSourceLines(parser, expr->line, expr->col,
        strlen(expr->string), "unknown variable");

    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedMember(
    Parser* parser, ASTType* type, ASTExpr* expr) {
    eprintf("\n(%d) \e[31merror:\e[0m type \e[34m%s\e[0m has no member "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        type->name, expr->string, //
        RELF(parser->filename), //
        expr->line, //
        expr->col);

    if (type->body) foreach (ASTVar*, var, type->body->locals) {
            unsigned l1 = strlen(var->name);
            unsigned l2 = strlen(expr->string);

            if (leven(var->name, expr->string, l1, l2) <= 3)
                eprintf("    did you mean: \e[34m%s\e[0m? (found "
                        "at %d:%d)\n",
                    var->name, var->line, var->col);
        }
    Parser_errorIncrement(parser);
}

static void Parser_warnUnusedArg(Parser* parser, ASTVar* var) {
    if (!parser->issues.warnUnusedArg) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused argument "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++parser->issues.warnCount, //
        var->name, //
        RELF(parser->filename), //
        var->line, //
        var->col);
}

static void Parser_warnUnusedVar(Parser* parser, ASTVar* var) {
    if (!parser->issues.warnUnusedVar) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++parser->issues.warnCount, //
        var->name, //
        RELF(parser->filename), //
        var->line, //
        var->col);
}

static void Parser_warnUnusedFunc(Parser* parser, ASTFunc* func) {
    if (!parser->issues.warnUnusedFunc) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused function "
            "\e[34m%s\e[0m at %s%s:%d\n"
            "            selector is \e[34m%s\e[0m\n",
        ++parser->issues.warnCount, //
        func->name, //
        RELF(parser->filename),
        func->line, //
        func->selector);
}

static void Parser_warnUnusedType(Parser* parser, ASTType* type) {
    if (!parser->issues.warnUnusedType) return;
    eprintf("\n(%d) \e[33mwarning:\e[0m unused type "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        ++parser->issues.warnCount, //
        type->name, //
        RELF(parser->filename),
        type->line, //
        type->col);
}

static void Parser_errorDuplicateVar(
    Parser* parser, ASTVar* var, ASTVar* orig) {
    eprintf("\n(%d) \e[31merror:\e[0m duplicate variable "
            "\e[34m%s\e[0m at %s%s:%d:%d\n   "
            "          already declared at %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        var->name, //
        RELF(parser->filename),
        var->line, //
        var->col, //
        RELF(parser->filename), //
        orig->line, //
        orig->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateType(
    Parser* parser, ASTType* type, ASTType* orig) {
    if (orig)
        eprintf("\n(%d) \e[31merror:\e[0m duplicate type "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          already declared at %s%s:%d:%d\n",
            parser->issues.errCount + 1, type->name, RELF(parser->filename),
            type->line, type->col, RELF(parser->filename), orig->line,
            orig->col);
    else
        eprintf("\n(%d) \e[31merror:\e[0m invalid type name "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          refers to a built-in type\n",
            parser->issues.errCount + 1, type->name, RELF(parser->filename),
            type->line, type->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateEnum(
    Parser* parser, ASTType* en, ASTType* orig) {
    if (orig)
        eprintf("\n(%d) \e[31merror:\e[0m duplicate enum "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          already declared at %s%s:%d:%d\n",
            parser->issues.errCount + 1, en->name, RELF(parser->filename),
            en->line, en->col, RELF(parser->filename), orig->line, orig->col);
    else
        eprintf("\n(%d) \e[31merror:\e[0m invalid enum name "
                "\e[34m%s\e[0m at %s%s:%d:%d\n   "
                "          refers to a built-in type\n",
            parser->issues.errCount + 1, en->name, RELF(parser->filename),
            en->line, en->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeInheritsSelf(Parser* parser, ASTType* type) {
    eprintf("\n(%d) \e[31merror:\e[0m type inherits from parser "
            "\e[34m%s\e[0m at %s%s:%d:%d\n",
        parser->issues.errCount + 1, type->name, RELF(parser->filename),
        type->line, type->col);
    Parser_errorIncrement(parser);
}
static void Parser_errorCtorHasType(
    Parser* parser, ASTFunc* func, ASTType* orig) {
    eprintf("\n(%d) \e[31merror:\e[0m constructor needs no return "
            "type: \e[34m%s\e[0m at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             remove the return type specification\n",
        parser->issues.errCount + 1, func->name, RELF(parser->filename),
        func->line, 1, RELF(parser->filename), orig->line, orig->col);
    Parser_errorIncrement(parser);
}
static void Parser_warnCtorCase(Parser* parser, ASTFunc* func) {
    ASTType* orig = func->returnSpec->type;
    eprintf("\n(%d) \e[33mwarning:\e[0m wrong case "
            "\e[34m%s\e[0m for constructor at %s%s:%d:%d\n"
            "             type declared at %s%s:%d:%d\n"
            "             change it to \e[34m%s\e[0m or lint the "
            "file\n",
        ++parser->issues.warnCount, func->name, RELF(parser->filename),
        func->line, 1, RELF(parser->filename), orig->line, orig->col,
        orig->name);
}

static void Parser_errorDuplicateFunc(
    Parser* parser, ASTFunc* func, ASTFunc* orig) {
    eprintf("\n(%d) \e[31merror:\e[0m duplicate function "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "             already declared at %s%s:%d:%d\n"
            "             selector is \e[34m%s\e[0m\n",
        parser->issues.errCount + 1, func->name, RELF(parser->filename),
        func->line, 1, RELF(parser->filename), orig->line, 1, func->selector);
    Parser_errorIncrement(parser);
}

static void Parser_errorDuplicateTest(
    Parser* parser, ASTTest* test, ASTTest* orig) {
    eprintf("\n(%d) \e[31merror:\e[0m duplicate test "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "             already declared at %s%s:%d:%d\n",
        parser->issues.errCount + 1, test->name, RELF(parser->filename),
        test->line, 1, RELF(parser->filename), orig->line, 1);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedFunc(
    Parser* parser, ASTExpr* expr, char* selector) {
    if (*selector == '<') return; // invalid type; already error'd

    eprintf("\n------ \e[31m#%d\e[0m ---------------------------" //
            "------------------------------------\n" //
            "\e[31merror:\e[0m cannot resolve call to function "
            "\e[34m'%s'\e[0m\n" //
            "where: %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        expr->string, //
        RELF(parser->filename), //
        expr->line, //
        expr->col //
    );

    Parser__printSourceLines(parser, expr->line, expr->col,
        strlen(expr->string), "unknown function");

    eprintf("\e[36;1minfo:\e[0m no function with selector \e[34m'%s'\e[0m\n",
        selector);

    // eprintf("\n\e[31;1;4m ERROR                                    "
    //         "           "
    //         "                       \e[0m\n %s%s:%d:%d:\n This "
    //         "\e[1m%s\e[0m call could not be resolved.\n"
    //         " There is no method with selector \e[1m%s\e[0m and %d "
    //         "arguments.\n",
    //     RELF(parser->filename), expr->line, expr->col, expr->string,
    //     selector, ASTExpr_countCommaList(expr->left));
    Parser_errorIncrement(parser);
}

static void Parser_errorStringInterp(Parser* parser, ASTExpr* expr, char* pos) {
    eprintf("\n\e[31;1;4m ERROR                                    "
            "           "
            "                       \e[0m\n %s%s:%d:%d:\n There is "
            "a syntax "
            "error within"
            " this string:\n"
            "     %s\"\n"
            " $ is used to specify a variable whose value is to be "
            "interpolated into the\n"
            " string. If you want a literal $ sign in the string, "
            "write it as "
            "\\$.\n",
        RELF(parser->filename), expr->line,
        expr->col + (int)(pos - expr->string), expr->string);
    Parser_errorIncrement(parser);
}

static void Parser_errorCallingFuncWithVoid(
    Parser* parser, ASTExpr* expr, ASTExpr* arg) {
    eprintf("\n\e[31;1;4m ERROR                                    "
            "           "
            "                       \e[0m\n %s%s:%d:%d:\n The "
            "\e[1m%s\e[0m function does not return a value.\n"
            " You cannot use it as an argument in the call to "
            "\e[1m%s\e[0m.\n",
        RELF(parser->filename), expr->line, expr->col, arg->func->name,
        expr->string);
    Parser_errorIncrement(parser);
}

static void Parser_errorInheritanceCycle(Parser* parser, ASTType* type) {
    eprintf("\n\e[31;1;4m ERROR                                    "
            "           "
            "                       \e[0m\n %s%s:%d:%d:\n Type "
            "\e[1m%s\e[0m has a cycle in its inheritance graph.",
        RELF(parser->filename), type->line, type->col, type->name);
    ASTType* super = type->super->type;
    eputs("\e[;2m");
    do {
        eprintf("\n extends \e[;1;2m%s\e[0;2m (defined at %s%s:%d:%d)",
            super->name, RELF(parser->filename), super->line, super->col);
        if (super == super->super->type || super == type) {
            if (type != super) eprintf("\n extends %s", super->name);
            break;
        }
        super = super->super->type;
    } while (1);
    eputs("\n ...\e[0m\n");
    Parser_errorIncrement(parser);
}

static void Parser_errorConstructorHasCycle(Parser* parser, ASTType* type) {
    eprintf("\n\e[31;1;4m ERROR                                    "
            "           "
            "                       \e[0m\n %s%s:%d:%d:\n Type "
            "\e[1m%s\e[0m has an endless cycle in its "
            "initialization.\n",
        RELF(parser->filename), type->line, type->col, type->name);
    Parser_errorIncrement(parser);
}

static void Parser_errorArgsCountMismatch(Parser* parser, ASTExpr* expr) {
    assert(expr->kind == tkFunctionCallResolved);
    eprintf("\n(%d) \e[31merror:\e[0m arg count mismatch for "
            "\e[34m%s\e[0m at %s%s:%d:%d\n"
            "          have %d args, need %d, func defined at "
            "%s%s:%d\n",
        parser->issues.errCount + 1, expr->func->name, RELF(parser->filename),
        expr->line, expr->col, ASTExpr_countCommaList(expr->left),
        expr->func->argCount, RELF(parser->filename), expr->func->line);
    Parser_errorIncrement(parser);
}

static void Parser_errorIndexDimsMismatch(
    Parser* parser, ASTExpr* expr, int nhave) {
    assert(expr->kind == tkSubscriptResolved);
    if (expr->var->typeSpec->typeType == TYErrorType
        || expr->var->typeSpec->typeType == TYUnresolved)
        return;
    // ^ type resolution failed (and must have raised error) so
    // don't process further
    int reqdDims = expr->var->typeSpec->dims;
    if (!reqdDims)
        eprintf("\n(%d) \e[31merror:\e[0m not an array: "
                "\e[34m%s\e[0m at %s%s:%d:%d\n"
                "          indexing a non-array with %d dims, var "
                "defined "
                "at %s%s:%d\n",
            parser->issues.errCount + 1, expr->var->name,
            RELF(parser->filename), expr->line, expr->col, nhave,
            RELF(parser->filename), expr->var->typeSpec->line);
    else {
        eprintf("(%d) \e[31merror:\e[0m index dims mismatch for "
                "\e[34m%s\e[0m at %s%s:%d:%d\n"
                "          have %d indexes, need %d, var defined "
                "at %s%s:%d\n",
            parser->issues.errCount + 1, expr->var->name,
            RELF(parser->filename), expr->line, expr->col, nhave, reqdDims,
            RELF(parser->filename), expr->var->typeSpec->line);
    }
    Parser_errorIncrement(parser);
}

static void Parser_errorMissingInit(Parser* parser, ASTExpr* expr) {
    assert(expr->kind == tkVarAssign);
    eprintf("\n(%d) \e[31merror:\e[0m missing initializer for "
            "\e[34m%s\e[0m at %s%s:%d-%d\n",
        parser->issues.errCount + 1, expr->var->name, RELF(parser->filename),
        expr->line - 1, expr->line);
    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedType(
    Parser* parser, ASTTypeSpec* typeSpec) {
    eprintf("\n(%d) \e[31merror:\e[0m unknown typespec \e[33m%s\e[0m "
            "at %s%s:%d:%d\n",
        parser->issues.errCount + 1, typeSpec->name, RELF(parser->filename),
        typeSpec->line, typeSpec->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorUnrecognizedCtor(Parser* parser, ASTFunc* func) {
    eprintf("\n(%d) \e[31merror:\e[0m unknown type \e[33m%s\e[0m "
            "for constructor at %s%s:%d\n",
        parser->issues.errCount + 1, func->name, RELF(parser->filename),
        func->line);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTestName(Parser* parser) {
    eprintf("\n(%d) \e[31merror:\e[0m invalid test name "
            "\e[33m%.*s\e[0m at %s%s:%d"
            "\n       test names must be strings\n",
        parser->issues.errCount + 1, parser->token.matchlen, parser->token.pos,
        RELF(parser->filename), parser->token.line);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeMismatchBinOp(Parser* parser, ASTExpr* expr) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* leftTypeName = ASTExpr_typeName(expr->left);
    const char* rightTypeName = ASTExpr_typeName(expr->right);
    if (*leftTypeName == '<' || *rightTypeName == '<') return;
    eprintf("\n(%d) \e[31merror:\e[0m type mismatch at %s%s:%d:%d\n"
            "             can't apply '\e[34m%s\e[0m' to \e[34m%s\e[0m"
            " and \e[34m%s\e[0m\n",
        parser->issues.errCount + 1, RELF(parser->filename), expr->line,
        expr->col, TokenKind_repr(expr->kind, false), leftTypeName,
        rightTypeName);
    Parser_errorIncrement(parser);
}

static void Parser_errorTypeMismatch(Parser* parser, ASTExpr* e1, ASTExpr* e2) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* leftTypeName = ASTExpr_typeName(e1);
    const char* rightTypeName = ASTExpr_typeName(e2);
    if (*leftTypeName == '<' || *rightTypeName == '<') return;
    eprintf("\n(%d) \e[31merror:\e[0m expected same types at:\n"
            "       %s%s:%d:%d: found '\e[33m%s\e[0m'\n"
            "       %s%s:%d:%d: found '\e[33m%s\e[0m'\n",
        parser->issues.errCount + 1, RELF(parser->filename), e1->line, e1->col,
        leftTypeName, RELF(parser->filename), e2->line, e2->col, rightTypeName);
    Parser_errorIncrement(parser);
}

static void Parser_errorInitMismatch(Parser* parser, ASTExpr* expr) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    const char* leftTypeName = ASTTypeSpec_name(expr->var->typeSpec);
    const char* rightTypeName = ASTExpr_typeName(expr->var->init);
    //    if (*leftTypeName == '<' or *rightTypeName == '<') return;

    // for collections, RHS is allowed to be an empty [] or {} to
    // indicate that the array starts out empty. Any-dim arrays can
    // be initialized with []. e.g. var arr[:,:,:] as Number = [] of
    // course, the LHS must have a type, you cannot have e.g. var
    // arr[:,:,:] = [] that would be an error.
    if ((expr->var->init->kind == tkArrayOpen
            || expr->var->init->kind == tkBraceOpen)
        && expr->var->typeSpec->collectionType != CTYNone
        && !expr->var->init->right
        && expr->var->typeSpec->typeType != TYUnresolved)
        return;

    eprintf("\n(%d) \e[31merror:\e[0m initializer mismatch at "
            "%s%s:%d:%d\n"
            "             can't init \e[34m%s\e[0m with an expression "
            "of "
            "type \e[34m%s\e[0m\n"
            "             just remove the type, the linter will take "
            "care of it.\n",
        parser->issues.errCount + 1, //
        RELF(parser->filename), //
        expr->line,
        expr->col, //
        leftTypeName, //
        rightTypeName);
    Parser_errorIncrement(parser);
}

static void Parser_errorInitDimsMismatch(
    Parser* parser, ASTExpr* expr, int dims) {
    // if one of the types is "<invalid>", an error has already been
    // reported for it; so don't bother
    //    char* leftTypeName =
    //    ASTTypeSpec_name(expr->var->typeSpec); char* rightTypeName
    //    = ASTExpr_typeName(expr->var->init); if (*leftTypeName ==
    //    '<' or *rightTypeName == '<') return;
    eprintf("\n(%d) \e[31merror:\e[0m dimensions mismatch at "
            "%s%s:%d:%d\n"
            "             can't init a \e[34m%dD\e[0m array "
            "\e[34m%s\e[0m with "
            "a \e[34m%dD\e[0m literal. \n"
            "             just remove the dimension specification, "
            "the linter "
            "will take "
            "care of it.\n",
        parser->issues.errCount + 1, //
        RELF(parser->filename), //
        expr->line,
        expr->col, //
        expr->var->typeSpec->dims, //
        expr->var->name, dims);
    Parser_errorIncrement(parser);
}

static void Parser_errorBinOpDimsMismatch(Parser* parser, ASTExpr* expr) {

    eprintf("\n(%d) \e[31merror:\e[0m dimensions mismatch at "
            "%s%s:%d:%d\n"
            "             operator '%s' has \e[34m%dD\e[0m on left, "
            "\e[34m%dD\e[0m on right\n",
        parser->issues.errCount + 1, //
        RELF(parser->filename), //
        expr->line,
        expr->col, //
        tkrepr[expr->kind], //
        expr->left->dims, //
        expr->right->dims);
    Parser_errorIncrement(parser);
}

static void Parser_errorReadOnlyVar(Parser* parser, ASTExpr* expr) {
    eprintf("\n(%d) \e[31merror:\e[0m mutating read-only variable '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        expr->var->name, //
        RELF(parser->filename), //
        expr->line, //
        expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorNoEnumInferred(Parser* parser, ASTExpr* expr) {
    eprintf("\n(%d) \e[31merror:\e[0m cannot infer enum type for '"
            "\e[34m.%s\e[0m' at %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        expr->string, //
        RELF(parser->filename), //
        expr->line, //
        expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorInvalidTypeForOp(Parser* parser, ASTExpr* expr) {
    if (expr->left->typeType == TYErrorType
        || expr->right->typeType == TYErrorType)
        return;

    eprintf("\n(%d) \e[31merror:\e[0m invalid types for operator '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        TokenKind_repr(expr->kind, false), //
        RELF(parser->filename), //
        expr->line, expr->col);
    Parser_errorIncrement(parser);
}

static void Parser_errorArgTypeMismatch(
    Parser* parser, ASTExpr* expr, ASTVar* var) {
    eprintf("\n(%d) \e[31merror:\e[0m type mismatch for argument '"
            "\e[34m%s\e[0m' at %s%s:%d:%d\n"
            "    need %s (%d), got %s (%d)\n",
        parser->issues.errCount + 1, //
        var->name, //
        RELF(parser->filename), //
        expr->line, //
        expr->col, //
        ASTTypeSpec_name(var->typeSpec),
        var->typeSpec->typeType, //
        ASTExpr_typeName(expr), //
        expr->typeType);
    parser->issues.hasParseErrors = 1;
    Parser_errorIncrement(parser);
}

static void Parser_errorUnexpectedToken(Parser* parser, char* msg) {
    eprintf("\n_________________________________" //
            "____________________________________ \e[31;1m#%d\e[0m\n" //
            "\e[31;1merror:\e[0;1m unexpected token \e[34;1m'%.*s'\e[0;1m\n" //
            " file:\e[0m %s%s:%d:%d\n",
        parser->issues.errCount + 1, //
        parser->token.matchlen, //
        parser->token.pos, //
        RELF(parser->filename), //
        parser->token.line, //
        parser->token.col //
    );

    Parser__printSourceLines(parser, parser->token.line, parser->token.col,
        parser->token.matchlen, msg);

    // when you have an unexpected token on one line, the rest are also going to
    // be unexpected. so skip to the next newline.
    while (!Parser_matches(parser, tkNewline)
        && !Parser_matches(parser, tkNullChar))
        Token_advance(&parser->token);

    Parser_errorIncrement(parser);
}

static void Parser_errorUnexpectedExpr(Parser* parser, ASTExpr* expr) {
    eprintf("\n(%d) \e[31merror:\e[0m at %s%s:%d:%d\n"
            "      unexpected expr '%s' (%s)\n",
        parser->issues.errCount + 1, //
        RELF(parser->filename), //
        expr->line, //
        expr->col,
        expr->prec //
            ? TokenKind_repr(expr->kind, false) //
            : expr->string,
        TokenKind_str[expr->kind] + 2);
    Parser_errorIncrement(parser);
}
